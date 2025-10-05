library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(plotly)
library(DT)
library(geosphere)
library(markovchain)
library(reshape2)
library(lubridate)
library(expm) 
library(leaflet.extras)

# Load and preprocess your actual data
Full_Shark_Data <- read.csv("shark_data_fully_imputed_complete.csv")

# Your actual analysis code - FIXED with correct column names
max_gap_h <- 24  
all_states <- c("resting", "foraging", "migrating")

# Process shark states with correct column names
Shark_states_daily <- Full_Shark_Data %>%
  filter(latitude >= -90 & latitude <= 90 & longitude >= -180 & longitude <= 180) %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(id, date) %>%
  summarise(
    latitude = mean(latitude),
    longitude = mean(longitude),
    depth = mean(bathymetry, na.rm = TRUE),
    chlorophyll = mean(chlorophyll, na.rm = TRUE),
    sst = mean(sst, na.rm = TRUE),
    species = first(species)  # Use the species column that exists
  ) %>%
  arrange(id, date) %>%
  group_by(id) %>%
  mutate(
    next_lat = lead(latitude),
    next_lon = lead(longitude),
    next_date = lead(date),
    time_diff_days = as.numeric(difftime(next_date, date, units = "days")),
    dist_m = distHaversine(
      p1 = data.frame(lon = longitude, lat = latitude), 
      p2 = data.frame(lon = next_lon, lat = next_lat)
    ),
    speed_mpd = ifelse(!is.na(time_diff_days) & time_diff_days > 0, 
                       dist_m / time_diff_days, NA),
    state = case_when(
      !is.na(speed_mpd) & speed_mpd < 500 ~ "resting",
      !is.na(speed_mpd) & speed_mpd < 5000 ~ "foraging",
      !is.na(speed_mpd) ~ "migrating",
      TRUE ~ NA_character_
    ),
    next_state = lead(state)
  ) %>%
  filter(!is.na(state) & !is.na(next_state)) %>%
  ungroup()

# Simplify species names for better visualization
Shark_states_daily <- Shark_states_daily %>%
  mutate(
    species_simple = case_when(
      grepl("White", species, ignore.case = TRUE) ~ "White Shark",
      grepl("Mako", species, ignore.case = TRUE) ~ "Mako Shark",
      TRUE ~ species  # Keep original if no match
    )
  )

# Markov chain analysis
complete_transitions <- expand.grid(state = all_states, next_state = all_states)

transition_counts <- Shark_states_daily %>%
  count(state, next_state) %>%
  right_join(complete_transitions, by = c("state", "next_state")) %>%
  mutate(n = ifelse(is.na(n), 0, n))

transition_prob <- transition_counts %>%
  group_by(state) %>%
  mutate(prob = n / sum(n)) %>%
  mutate(prob = ifelse(is.nan(prob), 0, prob)) %>%
  ungroup() %>%
  select(state, next_state, prob) %>%
  pivot_wider(names_from = next_state, values_from = prob)

mat <- as.matrix(transition_prob[, -1])
rownames(mat) <- transition_prob$state

row_sums <- rowSums(mat)
if (any(row_sums == 0)) {
  for (i in which(row_sums == 0)) {
    mat[i, i] <- 1
  }
}

mc <- new("markovchain", states = all_states, transitionMatrix = mat)

# Environmental reward analysis
grid_cell_size <- 0.5
assignment_file_path <- "shark_grid_assignment.rds"

shark_sf_daily <- st_as_sf(Shark_states_daily, coords = c("longitude", "latitude"), crs = 4326)

if (file.exists(assignment_file_path)) {
  ocean_grid <- st_make_grid(shark_sf_daily, cellsize = c(grid_cell_size, grid_cell_size)) %>%
    st_as_sf() %>%
    mutate(grid_id = 1:n())
  
  shark_grid_assignment <- readRDS(assignment_file_path)
} else {
  ocean_grid <- st_make_grid(shark_sf_daily, cellsize = c(grid_cell_size, grid_cell_size)) %>%
    st_as_sf() %>%
    mutate(grid_id = 1:n())
  shark_grid_assignment <- st_join(shark_sf_daily, ocean_grid, join = st_within)
  saveRDS(shark_grid_assignment, file = assignment_file_path)
}

Ocean_grid_rewards <- shark_grid_assignment %>%
  as_tibble() %>% 
  group_by(grid_id) %>%
  summarise(
    mean_chloro = mean(chlorophyll, na.rm = TRUE),
    mean_SST = mean(sst, na.rm = TRUE),
    mean_depth = mean(depth, na.rm = TRUE)
  ) %>%
  filter(!is.na(mean_chloro) & !is.na(mean_SST)) %>%
  mutate(
    chloro_norm = (mean_chloro - min(mean_chloro)) / (max(mean_chloro) - min(mean_chloro)),
    sst_norm = 1 - abs(mean_SST - 16) / (max(mean_SST) - 16),
    reward = (0.6 * chloro_norm) + (0.4 * sst_norm)
  ) %>%
  right_join(ocean_grid, by = "grid_id") %>%
  st_as_sf() %>%
  filter(!is.na(reward))

# Create foraging hotspots data
foraging_hotspots <- Shark_states_daily %>%
  filter(state == "foraging") %>%
  group_by(lat_rounded = round(latitude, 1), 
           lon_rounded = round(longitude, 1)) %>%
  summarise(
    foraging_intensity = n(),
    avg_chloro = mean(chlorophyll, na.rm = TRUE),
    avg_sst = mean(sst, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    hotspot_score = scale(foraging_intensity)[,1]
  )

# Get all unique species names for the dropdown
all_species_choices <- sort(unique(Shark_states_daily$species_simple))
species_dropdown_choices <- c("All", all_species_choices)

# Region detection function
detect_region <- function(longitude, latitude) {
  case_when(
    longitude >= -80 & longitude <= 40 & latitude >= -70 & latitude <= 80 ~ "Atlantic",
    longitude >= 100 & longitude <= 290 & latitude >= -70 & latitude <= 70 ~ "Pacific",
    longitude >= 20 & longitude <= 120 & latitude >= -50 & latitude <= 30 ~ "Indian",
    latitude <= -50 ~ "Southern",
    latitude >= 70 ~ "Arctic",
    TRUE ~ "Unknown"
  )
}

# Add region to Shark_states_daily
Shark_states_daily <- Shark_states_daily %>%
  mutate(region = detect_region(longitude, latitude))

# UI Definition
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Montserrat:wght@400;600;700&display=swap');
      
      body {
          font-family: 'Montserrat', sans-serif;
          background-color: #f4f7f6;
      }
      .sidebar {
          background-color: #2c3e50;
          color: white;
          padding: 15px;
          box-shadow: 2px 0 5px rgba(0,0,0,0.1);
      }
      .title-panel {
          background-color: #34495e;
          padding: 20px 0;
          margin-bottom: 20px;
          text-align: center;
          border-bottom: 3px solid #1abc9c;
      }
      .title-panel h1 {
          color: white;
          font-weight: 700;
          margin: 0;
      }
      .metric-card {
          background: white;
          border-left: 5px solid;
          border-radius: 8px;
          padding: 15px;
          margin-bottom: 20px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }
      .metric-card p, .metric-card h5 { 
          color: #333333;
      }
      .metric-value {
          font-weight: 700;
          font-size: 2.0em;
          margin: 5px 0 5px;
          color: #2c3e50;
      }
      #card-obs { border-left-color: #e74c3c; }
      #card-species { border-left-color: #3498db; }
      #card-coverage { border-left-color: #27ae60; }
      #card-time { border-left-color: #f39c12; }
      
      .nav-tabs > li > a {
          color: #2c3e50;
          font-weight: 600;
      }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
          color: #1abc9c !important;
          border-top: 3px solid #1abc9c;
      }
      
      .prediction-panel {
          background: rgba(255,255,255,0.95);
          padding: 15px;
          border-radius: 10px;
          box-shadow: 0 4px 6px rgba(0,0,0,0.1);
          margin-bottom: 15px;
      }
      
      /* Fix for small plots to prevent margin errors */
      .shiny-plot-output {
          padding: 5px !important;
      }
      .plot-container {
          margin: 0 !important;
          padding: 0 !important;
      }
      
      /* Style for info panel in Movement tab */
      .info-panel {
          background: white;
          border-radius: 8px;
          padding: 15px;
          margin-bottom: 15px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.05);
          border-left: 5px solid #1abc9c;
      }
      
      .btn-success {
          background-color: #27ae60;
          border-color: #27ae60;
          width: 100%;
      }
    "))
  ),
  
  div(class = "title-panel",
      h1("🦈 Shark Tracker Dashboard")
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      width = 3,
      
      h3(icon("tachometer-alt"), "Key Metrics"),
      hr(),
      
      div(id = "card-obs", class = "metric-card",
          p("Total Observations"),
          div(class = "metric-value", textOutput("total_obs"))
      ),
      
      div(id = "card-species", class = "metric-card",
          p("Species Tracked"),
          div(class = "metric-value", textOutput("species_count"))
      ),
      
      div(id = "card-coverage", class = "metric-card",
          p("Data Coverage"),
          div(class = "metric-value", "100%")
      ),
      
      div(id = "card-time", class = "metric-card",
          p("Tracking Duration"),
          div(class = "metric-value", textOutput("time_span"))
      ),
      
      hr(),
      
      h3(icon("sliders-h"), "Global Controls"),
      
      selectInput("speciesSelect", "Filter by Species:",
                  choices = species_dropdown_choices,
                  selected = "All"),
      
      selectInput("oceanRegion", "Ocean Region:",
                  choices = c("All", "Atlantic", "Pacific", "Indian", "Southern", "Arctic"),
                  selected = "All"),
      
      hr(),
      
      h4(icon("bullseye"), "Behavior Predictor"),
      selectInput("currentState", "Current State:",
                  choices = all_states, selected = "foraging"),
      numericInput("steps", "Prediction Steps (Days):",
                   value = 1, min = 1, max = 10, step = 1),
      
      div(class = "metric-card",
          style = "border-left-color: #1abc9c;", 
          h5("Predicted Distribution:"),
          div(style = "height: 120px; margin: 0; padding: 0;", 
              plotlyOutput("predictionPlot", height = "100%"))
      )
      
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        id = "main_tabs",
        
        tabPanel(title = div(icon("crosshairs"), "Interactive Predictor"),
                 br(),
                 
                 fluidRow(
                   column(8,
                          div(class = "metric-card",
                              h4(icon("map-marked-alt"), "Click to Predict Shark Behavior"),
                              p("Click anywhere on the map to predict shark behavior and movement patterns"),
                              leafletOutput("predictiveMap", height = "600px")
                          )
                   ),
                   column(4,
                          div(class = "prediction-panel",
                              h4("📍 Prediction Results"),
                              verbatimTextOutput("clickInfo"),
                              hr(),
                              h5("🎯 Predicted Behavior:"),
                              textOutput("predictedBehavior"),
                              h5("📈 Environmental Score:"),
                              textOutput("envScore"),
                              h5("🦈 Common Species:"),
                              textOutput("commonSpecies"),
                              h5("➡️ Movement Direction:"),
                              div(style = "height: 100px; margin: 0; padding: 0; border: 1px solid #eee;",
                                  plotOutput("movementArrow", height = "100%"))
                          ),
                          div(class = "metric-card",
                              h4(icon("layer-group"), "Map Overlays"),
                              checkboxGroupInput("overlays", "Show:",
                                                 choices = c("Foraging Hotspots" = "foraging",
                                                             "Environmental Rewards" = "rewards",
                                                             "Shark Observations" = "observations"),
                                                 selected = "foraging")
                          )
                   )
                 )
        ),
        
        # NEW: Movement Predictor Tab
        tabPanel(title = div(icon("route"), "Movement Predictor"),
                 br(),
                 
                 fluidRow(
                   column(4,
                          div(class = "prediction-panel",
                              h4(icon("sliders-h"), "Prediction Controls"),
                              
                              selectInput("predictionSpecies", "Shark Species:",
                                          choices = species_dropdown_choices,
                                          selected = "All"),
                              
                              selectInput("predictionRegion", "Ocean Region:",
                                          choices = c("All", "Atlantic", "Pacific", "Indian", "Southern", "Arctic"),
                                          selected = "All"),
                              
                              selectInput("startBehavior", "Starting Behavior:",
                                          choices = all_states,
                                          selected = "foraging"),
                              
                              numericInput("predictionSteps", "Prediction Steps (Days):",
                                          value = 3, min = 1, max = 10, step = 1),
                              
                              sliderInput("simulationCount", "Number of Simulations:",
                                         min = 10, max = 200, value = 50, step = 10),
                              
                              actionButton("runPrediction", "Run Prediction", 
                                         icon = icon("play"), class = "btn-success"),
                              
                              hr(),
                              
                              h5(icon("info-circle"), "Current Settings:"),
                              verbatimTextOutput("predictionSettings")
                          )
                   ),
                   column(8,
                          div(class = "metric-card",
                              h4(icon("map-marked-alt"), "Predicted Movement Map"),
                              leafletOutput("predictionMap", height = "500px")
                          ),
                          fluidRow(
                            column(6,
                                   div(class = "metric-card",
                                       h4(icon("chart-bar"), "Path Analytics"),
                                       plotlyOutput("pathAnalytics", height = "250px")
                                   )
                            ),
                            column(6,
                                   div(class = "metric-card",
                                       h4(icon("project-diagram"), "State Transitions"),
                                       plotlyOutput("stateTransitions", height = "250px")
                                   )
                            )
                          )
                   )
                 )
        ),
        
        tabPanel(title = div(icon("globe-americas"), "Movement & Habitat"),
                 br(),
                 
                 fluidRow(
                   column(3,
                          div(class = "info-panel",
                              h4(icon("info-circle"), "Current Filter"),
                              textOutput("currentFilterInfo"),
                              hr(),
                              h5("Map Legend"),
                              p("• Orange: Resting"),
                              p("• Green: Foraging"), 
                              p("• Blue: Migrating"),
                              hr(),
                              h5("Tip:"),
                              p("Use the 'Global Controls' in the left sidebar to filter by species and region")
                          ),
                          div(class = "metric-card",
                              h4(icon("chart-pie"), "Behavior Distribution"),
                              plotlyOutput("behaviorPlot", height = "300px")
                          )
                   ),
                   column(9,
                          div(class = "metric-card",
                              h4(icon("map-marked-alt"), "Shark Movement & Behavior"),
                              p("Showing data for: ", strong(textOutput("mapFilterInfo", inline = TRUE))),
                              leafletOutput("movementMap", height = "500px")
                          ),
                          fluidRow(
                            column(6,
                                   div(class = "metric-card",
                                       h4(icon("sun"), "Habitat Preferences"),
                                       plotlyOutput("habitatPrefs", height = "280px")
                                   )
                            ),
                            column(6,
                                   div(class = "metric-card",
                                       h4(icon("chart-bar"), "Behavior by Species"),
                                       plotlyOutput("speciesBehavior", height = "280px")
                                   )
                            )
                          )
                   )
                 )
        ),
        
        tabPanel(title = div(icon("brain"), "Advanced Analysis"),
                 br(),
                 
                 fluidRow(
                   column(7,
                          div(class = "metric-card",
                              h4(icon("project-diagram"), "Environmental Reward Map"),
                              leafletOutput("rewardMap", height = "500px")
                          )
                   ),
                   column(5,
                          div(class = "metric-card",
                              h4(icon("table"), "State Transition Matrix"),
                              tableOutput("transitionMatrix")
                          ),
                          div(class = "metric-card",
                              h4(icon("chart-area"), "Environmental Reward Distribution"),
                              plotlyOutput("rewardDistribution", height = "250px")
                          )
                   )
                 ),
                 
                 div(class = "metric-card",
                     h4(icon("code-branch"), "Markov Chain Transition Probabilities"),
                     plotOutput("markovPlot", height = "400px")
                 )
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Reactive data processing
  processedData <- reactive({
    Shark_states_daily
  })
  
  # Reactive data filtered by global species and region filters
  filteredData <- reactive({
    data <- processedData()
    
    # Apply global species filter
    if (input$speciesSelect != "All") {
      data <- data %>% filter(species_simple == input$speciesSelect)
    }
    
    # Apply global region filter
    if (input$oceanRegion != "All") {
      data <- data %>% filter(region == input$oceanRegion)
    }
    
    return(data)
  })
  
  # Reactive data for movement predictor (separate from global filters)
  predictionData <- reactive({
    data <- processedData()
    
    if (input$predictionSpecies != "All") {
      data <- data %>% filter(species_simple == input$predictionSpecies)
    }
    
    if (input$predictionRegion != "All") {
      data <- data %>% filter(region == input$predictionRegion)
    }
    
    return(data)
  })
  
  # Reactive values for predictive map
  click_data <- reactiveValues(
    last_click = NULL,
    prediction = NULL
  )
  
  # Reactive values for movement predictor
  prediction_results <- reactiveValues(
    paths = NULL,
    heatmap_data = NULL,
    last_prediction = NULL
  )
  
  # Info outputs for the Movement tab
  output$currentFilterInfo <- renderText({
    filters <- c()
    if (input$speciesSelect != "All") filters <- c(filters, paste("Species:", input$speciesSelect))
    if (input$oceanRegion != "All") filters <- c(filters, paste("Region:", input$oceanRegion))
    
    if (length(filters) == 0) {
      "Showing all shark species and regions"
    } else {
      paste(filters, collapse = " | ")
    }
  })
  
  output$mapFilterInfo <- renderText({
    filters <- c()
    if (input$speciesSelect != "All") filters <- c(filters, input$speciesSelect)
    if (input$oceanRegion != "All") filters <- c(filters, input$oceanRegion)
    
    if (length(filters) == 0) {
      "All Shark Species & Regions"
    } else {
      paste(filters, collapse = " + ")
    }
  })
  
  # Prediction settings display
  output$predictionSettings <- renderText({
    paste(
      "Species:", input$predictionSpecies, "\n",
      "Region:", input$predictionRegion, "\n", 
      "Starting Behavior:", input$startBehavior, "\n",
      "Steps:", input$predictionSteps, "\n",
      "Simulations:", input$simulationCount
    )
  })
  
  # Enhanced movement prediction functions
  predictNextState <- function(current_state) {
    # Use Markov chain to predict next state
    probs <- mat[current_state, ]
    sample(names(probs), size = 1, prob = probs)
  }
  
  predictMovementDirection <- function(current_lat, current_lng, next_state) {
    # Enhanced movement logic based on state and environment
    if (next_state == "resting") {
      # Small random movements
      lat_delta <- runif(1, -0.1, 0.1)
      lng_delta <- runif(1, -0.1, 0.1)
    } else if (next_state == "foraging") {
      # Medium movements, biased toward high chlorophyll areas
      lat_delta <- runif(1, -0.3, 0.3)
      lng_delta <- runif(1, -0.3, 0.3)
    } else { # migrating
      # Larger movements, potentially directional
      lat_delta <- runif(1, -0.8, 0.8)
      lng_delta <- runif(1, -0.8, 0.8)
      
      # Add some seasonal migration bias
      if (current_lat > 0) { # Northern hemisphere
        lat_delta <- lat_delta - 0.2 # Tend south
      } else { # Southern hemisphere  
        lat_delta <- lat_delta + 0.2 # Tend north
      }
    }
    
    list(lat_delta = lat_delta, lng_delta = lng_delta)
  }
  
  # Multi-step movement prediction
  predictMultiStepMovement <- function(start_lat, start_lng, start_state, steps = 5) {
    current_state <- start_state
    current_lat <- start_lat
    current_lng <- start_lng
    path <- data.frame(step = 0, lat = current_lat, lng = current_lng, state = current_state)
    
    for (step in 1:steps) {
      # 1. Predict next behavioral state using Markov chain
      next_state <- predictNextState(current_state)
      
      # 2. Predict movement based on state + environmental rewards
      movement <- predictMovementDirection(current_lat, current_lng, next_state)
      
      # 3. Update position (with boundary checks)
      new_lat <- current_lat + movement$lat_delta
      new_lng <- current_lng + movement$lng_delta
      
      # Keep within reasonable bounds
      new_lat <- pmin(pmax(new_lat, -85), 85)
      new_lng <- ((new_lng + 180) %% 360) - 180
      
      current_lat <- new_lat
      current_lng <- new_lng
      current_state <- next_state
      
      path <- rbind(path, data.frame(step = step, lat = current_lat, lng = current_lng, state = next_state))
    }
    
    return(path)
  }
  
  # Generate probability heatmap
  generateProbabilityHeatmap <- function(start_point, steps = 3, simulations = 100) {
    all_positions <- data.frame()
    
    for (i in 1:simulations) {
      path <- predictMultiStepMovement(start_point$lat, start_point$lng, 
                                      start_point$state, steps)
      all_positions <- rbind(all_positions, path %>% select(lat, lng, step))
    }
    
    # Create simple density data
    heatmap_data <- all_positions %>%
      group_by(step, lat = round(lat, 1), lng = round(lng, 1)) %>%
      summarise(probability = n() / simulations, .groups = 'drop')
    
    return(heatmap_data)
  }
  
  # Run prediction when button is clicked
  observeEvent(input$runPrediction, {
    # Use average position from filtered data as starting point
    pred_data <- predictionData()
    
    if (nrow(pred_data) == 0) {
      # If no data, use a default starting point
      start_point <- list(lat = 0, lng = 0, state = input$startBehavior)
    } else {
      start_point <- list(
        lat = mean(pred_data$latitude, na.rm = TRUE),
        lng = mean(pred_data$longitude, na.rm = TRUE), 
        state = input$startBehavior
      )
    }
    
    # Generate multiple paths for heatmap
    all_paths <- list()
    for (i in 1:input$simulationCount) {
      path <- predictMultiStepMovement(start_point$lat, start_point$lng,
                                     start_point$state, input$predictionSteps)
      path$simulation <- i
      all_paths[[i]] <- path
    }
    
    prediction_results$paths <- bind_rows(all_paths)
    prediction_results$heatmap_data <- generateProbabilityHeatmap(
      start_point, input$predictionSteps, input$simulationCount
    )
    prediction_results$last_prediction <- start_point
    
    # Update prediction map
    leafletProxy("predictionMap") %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearHeatmap() %>%
      addMarkers(
        lng = start_point$lng, lat = start_point$lat,
        popup = paste("Start: ", start_point$state)
      ) %>%
      addHeatmap(
        data = prediction_results$heatmap_data,
        lng = ~lng, lat = ~lat, intensity = ~probability,
        blur = 20, max = 0.3, radius = 15,
        gradient = c("blue", "cyan", "green", "yellow", "red"),
        group = "Probability Heatmap"
      )
    
    # Add sample paths
    sample_paths <- prediction_results$paths %>%
      filter(simulation %in% sample(1:input$simulationCount, min(10, input$simulationCount)))
    
    for (sim in unique(sample_paths$simulation)) {
      path_data <- sample_paths %>% filter(simulation == sim)
      leafletProxy("predictionMap") %>%
        addPolylines(
          data = path_data, lng = ~lng, lat = ~lat,
          color = "#FF6B6B", weight = 2, opacity = 0.6,
          group = "Sample Paths"
        )
    }
  })
  
  # Prediction Map
  output$predictionMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      addLayersControl(
        overlayGroups = c("Probability Heatmap", "Sample Paths"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("blue", "cyan", "green", "yellow", "red"),
        labels = c("Low", "", "Medium", "", "High"),
        title = "Position Probability"
      )
  })
  
  # Path Analytics Plot
  output$pathAnalytics <- renderPlotly({
    if (is.null(prediction_results$paths)) {
      return(plotly_empty() %>% 
               layout(title = "Run prediction to see analytics"))
    }
    
    path_stats <- prediction_results$paths %>%
      group_by(step) %>%
      summarise(
        avg_lat = mean(lat),
        avg_lng = mean(lng),
        state_diversity = n_distinct(state),
        .groups = 'drop'
      )
    
    plot_ly(path_stats, x = ~step, y = ~state_diversity, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#3498db'), marker = list(color = '#3498db')) %>%
      layout(
        title = "State Diversity Over Time",
        xaxis = list(title = "Step"),
        yaxis = list(title = "Number of Unique States")
      )
  })
  
  # State Transitions Plot
  output$stateTransitions <- renderPlotly({
    if (is.null(prediction_results$paths)) {
      return(plotly_empty() %>% 
               layout(title = "Run prediction to see transitions"))
    }
    
    state_transitions <- prediction_results$paths %>%
      group_by(step, state) %>%
      summarise(count = n(), .groups = 'drop') %>%
      group_by(step) %>%
      mutate(percentage = count / sum(count) * 100)
    
    plot_ly(state_transitions, x = ~step, y = ~percentage, color = ~state, type = 'bar',
            colors = c("orange", "green", "dodgerblue")) %>%
      layout(
        title = "State Distribution by Step",
        xaxis = list(title = "Step"),
        yaxis = list(title = "Percentage (%)"),
        barmode = 'stack'
      )
  })
  
  # [Keep all your existing map and output functions...]
  
  # Predictive Map
  output$predictiveMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      addLegend(
        position = "bottomright",
        colors = c("blue", "cyan", "green", "yellow", "red"),
        labels = c("Low", "", "Medium", "", "High"),
        title = "Foraging Hotspot Intensity"
      )
  })
  
  # Update predictive map with overlays
  observeEvent(input$overlays, {
    leafletProxy("predictiveMap") %>%
      clearHeatmap() %>%
      clearShapes()
    
    if ("foraging" %in% input$overlays) {
      leafletProxy("predictiveMap") %>%
        addHeatmap(
          data = foraging_hotspots,
          lng = ~lon_rounded, lat = ~lat_rounded, intensity = ~hotspot_score,
          blur = 20, max = 0.5, radius = 15,
          gradient = c("blue", "cyan", "green", "yellow", "red")
        )
    }
    
    if ("rewards" %in% input$overlays && exists("Ocean_grid_rewards")) {
      reward_pal <- colorNumeric("viridis", domain = Ocean_grid_rewards$reward)
      
      leafletProxy("predictiveMap") %>%
        addPolygons(
          data = Ocean_grid_rewards,
          fillColor = ~reward_pal(reward),
          fillOpacity = 0.3,
          stroke = FALSE,
          group = "rewards"
        )
    }
    
    if ("observations" %in% input$overlays) {
      shark_sf <- st_as_sf(processedData(), coords = c("longitude", "latitude"), crs = 4326)
      pal <- colorFactor(c("orange", "green", "dodgerblue"), domain = c("resting", "foraging", "migrating"))
      
      leafletProxy("predictiveMap") %>%
        addCircleMarkers(
          data = shark_sf,
          radius = 2,
          color = ~pal(state),
          stroke = FALSE,
          fillOpacity = 0.5,
          group = "observations"
        )
    }
  })
  
  # Movement Map
  output$movementMap <- renderLeaflet({
    filtered_data <- filteredData()
    shark_sf <- st_as_sf(filtered_data, coords = c("longitude", "latitude"), crs = 4326)
    
    pal <- colorFactor(c("orange", "green", "dodgerblue"), domain = c("resting", "foraging", "migrating"))
    
    leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      addCircleMarkers(
        data = shark_sf,
        radius = 3,
        color = ~pal(state),
        stroke = FALSE,
        fillOpacity = 0.7,
        popup = ~paste(
          "Species:", species_simple, "<br>",
          "State:", state, "<br>",
          "Date:", date, "<br>",
          "SST:", round(sst, 1), "°C<br>",
          "Chlorophyll:", round(chlorophyll, 3), "mg/m³"
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = shark_sf$state,
        title = "Behavior State"
      )
  })
  
  # FIXED: Better movement prediction logic
  predictSharkBehavior <- function(lat, lng) {
    # Find nearest environmental data point
    distances <- sqrt((Shark_states_daily$latitude - lat)^2 + (Shark_states_daily$longitude - lng)^2)
    nearest_idx <- which.min(distances)
    nearest_data <- Shark_states_daily[nearest_idx, ]
    
    # Improved behavior prediction with more realistic movement
    if (nearest_data$chlorophyll > 0.6) {
      behavior <- "foraging"
      # Foraging: small, random movements in any direction
      move_lat <- runif(1, -0.3, 0.3)
      move_lng <- runif(1, -0.3, 0.3)
    } else if (nearest_data$sst < 10 | nearest_data$sst > 25) {
      behavior <- "migrating"
      # Migrating: larger movements, generally toward more temperate waters (toward equator if too cold, away if too hot)
      if (nearest_data$sst < 10) {
        # Too cold - move toward equator (south if in northern hemisphere, north if in southern)
        move_lat <- ifelse(lat > 0, -0.8, 0.8) + runif(1, -0.2, 0.2)
      } else {
        # Too hot - move away from equator (north if in northern hemisphere, south if in southern)  
        move_lat <- ifelse(lat > 0, 0.8, -0.8) + runif(1, -0.2, 0.2)
      }
      move_lng <- runif(1, -0.5, 0.5)
    } else {
      behavior <- "resting"
      # Resting: very small, mostly random movements
      move_lat <- runif(1, -0.1, 0.1)
      move_lng <- runif(1, -0.1, 0.1)
    }
    
    # Environmental score
    env_score <- (nearest_data$chlorophyll * 0.6) + ((25 - abs(nearest_data$sst - 18)) / 25 * 0.4)
    
    # Common species in area
    nearby_species <- Shark_states_daily %>%
       
      filter(sqrt((latitude - lat)^2 + (longitude - lng)^2) < 5) %>%
      count(species_simple) %>%
      arrange(desc(n)) %>%
      pull(species_simple) %>%
      head(2)
    
    list(
      behavior = behavior,
      move_lat = move_lat,
      move_lng = move_lng,
      env_score = round(env_score, 3),
      common_species = if(length(nearby_species) > 0) nearby_species else "Unknown",
      sst = round(nearest_data$sst, 1),
      chlorophyll = round(nearest_data$chlorophyll, 3)
    )
  }
  
  # Handle map clicks for predictions
  observeEvent(input$predictiveMap_click, {
    click <- input$predictiveMap_click
    lat <- click$lat
    lng <- click$lng
    
    click_data$last_click <- list(lat = lat, lng = lng)
    
    # Predict behavior and movement
    prediction <- predictSharkBehavior(lat, lng)
    click_data$prediction <- prediction
    
    # Update map with prediction - FIXED: Correct arrow direction
    leafletProxy("predictiveMap") %>%
      clearMarkers() %>%
      clearShapes() %>%
      addMarkers(
        lng = lng, lat = lat,
        popup = paste("Predicted Behavior:", prediction$behavior)
      ) %>%
      # FIX: Correct arrow direction - from current position to predicted position
      addPolylines(
        lng = c(lng, lng + prediction$move_lng), 
        lat = c(lat, lat + prediction$move_lat),
        color = "red", weight = 3,
        opacity = 0.8
      )
  })
  
  # FIXED: Movement arrow plot with proper margins
  output$movementArrow <- renderPlot({
    # Set very small margins to prevent "figure margins too large" error
    par(mar = c(0.5, 0.5, 1.5, 0.5), mgp = c(0, 0, 0))
    
    if (!is.null(click_data$prediction)) {
      pred <- click_data$prediction
      
      # Create a simple directional plot
      plot(0, 0, type = "n", 
           xlim = c(-1, 1), ylim = c(-1, 1), 
           axes = FALSE, xlab = "", ylab = "", 
           main = "Movement Direction")
      
      # Draw arrow from center to predicted direction
      arrows(0, 0, pred$move_lng, pred$move_lat, 
             col = "red", lwd = 4, length = 0.15)
      
      # Add compass indicators
      text(0, 0.9, "N", cex = 0.8, col = "gray")
      text(0, -0.9, "S", cex = 0.8, col = "gray")
      text(0.9, 0, "E", cex = 0.8, col = "gray")
      text(-0.9, 0, "W", cex = 0.8, col = "gray")
      
    } else {
      # Empty plot with instructions
      plot(0, 0, type = "n", 
           xlim = c(-1, 1), ylim = c(-1, 1), 
           axes = FALSE, xlab = "", ylab = "", 
           main = "Movement Direction")
      text(0, 0, "Click on map\nfor prediction", cex = 0.9, col = "gray")
    }
  })
  
  # Prediction outputs
  output$clickInfo <- renderText({
    if (!is.null(click_data$last_click)) {
      paste("Clicked at:\nLat:", round(click_data$last_click$lat, 4), 
            "\nLng:", round(click_data$last_click$lng, 4))
    } else {
      "Click on the map to get predictions"
    }
  })
  
  output$predictedBehavior <- renderText({
    if (!is.null(click_data$prediction)) {
      click_data$prediction$behavior
    } else {
      "No prediction yet"
    }
  })
  
  output$envScore <- renderText({
    if (!is.null(click_data$prediction)) {
      paste("Score:", click_data$prediction$env_score, 
            "\nSST:", click_data$prediction$sst, "°C",
            "\nChlorophyll:", click_data$prediction$chlorophyll, "mg/m³")
    } else {
      "Click on map"
    }
  })
  
  output$commonSpecies <- renderText({
    if (!is.null(click_data$prediction)) {
      paste(click_data$prediction$common_species, collapse = ", ")
    } else {
      "Click on map"
    }
  })
  
  # [Keep all your existing outputs and functions exactly the same...]
  # Reactive Markov Prediction
  predicted_state <- reactive({
    req(input$currentState, input$steps)
    
    if (!exists("mc") || is.null(mc)) {
      return(NULL)
    }
    
    p_steps <- mc@transitionMatrix %^% input$steps
    
    start_state_vec <- rep(0, length(all_states))
    names(start_state_vec) <- all_states
    start_state_vec[input$currentState] <- 1
    
    next_dist <- start_state_vec %*% p_steps
    
    data.frame(
      state = colnames(next_dist),
      probability = as.vector(next_dist)
    ) %>%
      mutate(state = factor(state, levels = all_states))
  })
  
  # Metrics
  output$total_obs <- renderText({
    nrow(Full_Shark_Data)
  })
  
  output$species_count <- renderText({
    length(unique(Shark_states_daily$species_simple))
  })
  
  output$time_span <- renderText({
    dates <- range(Shark_states_daily$date, na.rm = TRUE)
    paste(round(as.numeric(difftime(dates[2], dates[1], units = "days"))), "days")
  })
  
  # Behavior Distribution - UPDATED to use filtered data
  output$behaviorPlot <- renderPlotly({
    behavior_summary <- filteredData() %>%
      count(state) %>%
      mutate(percentage = n / sum(n) * 100)
    
    plot_ly(behavior_summary, labels = ~state, values = ~n, type = 'pie',
            marker = list(colors = c("orange", "green", "dodgerblue"))) %>%
      layout(title = "Shark Behavior Distribution",
             showlegend = TRUE)
  })
  
  # Markov Chain Visualization
  output$markovPlot <- renderPlot({
    mat_melt <- melt(mat)
    ggplot(mat_melt, aes(Var1, Var2, fill = value)) +
      geom_tile(color = "black") +
      geom_text(aes(label = round(value, 2)), size = 5) +
      scale_fill_gradient(low = "white", high = "dodgerblue", name = "Probability") +
      labs(title = "Shark State Transition Probabilities",
           x = "Current State", y = "Next State") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Prediction Plot
  output$predictionPlot <- renderPlotly({
    pred_data <- predicted_state()
    if (is.null(pred_data)) {
      return(NULL)
    }
    
    state_colors <- c("resting" = "orange", "foraging" = "green", "migrating" = "dodgerblue")
    
    plot_ly(pred_data, x = ~state, y = ~probability, type = 'bar',
            color = ~state, colors = state_colors,
            text = ~paste0(round(probability * 100, 1), "%"),
            textposition = 'auto') %>%
      layout(title = paste("State Probability After", input$steps, "Steps"),
             xaxis = list(title = ""),
             yaxis = list(title = "Probability", tickformat = '.0%', range = c(0, 1)),
             showlegend = FALSE)
  })
  
  # Transition Matrix Table
  output$transitionMatrix <- renderTable({
    transition_prob_df <- as.data.frame(mat)
    transition_prob_df$Current_State <- rownames(transition_prob_df)
    transition_prob_df %>%
      select(Current_State, everything()) %>%
      rename(
        "To Resting" = resting,
        "To Foraging" = foraging,
        "To Migrating" = migrating
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, digits = 3)
  
  # Species Behavior Comparison
  output$speciesBehavior <- renderPlotly({
    species_behavior <- processedData() %>%
      count(species_simple, state) %>%
      group_by(species_simple) %>%
      mutate(percentage = n / sum(n) * 100)
    
    plot_ly(species_behavior, x = ~state, y = ~percentage, color = ~species_simple, type = 'bar') %>%
      layout(title = "Behavior Distribution by Species",
             xaxis = list(title = "Behavior State"),
             yaxis = list(title = "Percentage (%)"),
             barmode = 'group')
  })
  
  # Reward Distribution
  output$rewardDistribution <- renderPlotly({
    reward_data <- Ocean_grid_rewards %>%
      as_tibble() %>%
      filter(!is.na(reward))
    
    plot_ly(reward_data, x = ~reward, type = 'histogram',
            marker = list(color = '#9b59b6')) %>%
      layout(title = "Environmental Reward Distribution",
             xaxis = list(title = "Reward Score"),
             yaxis = list(title = "Frequency"))
  })
  
  # Habitat Preferences - UPDATED to use filtered data
  output$habitatPrefs <- renderPlotly({
    filtered_data <- filteredData()
    
    habitat_summary <- filtered_data %>%
      group_by(state) %>%
      summarise(
        avg_sst = mean(sst, na.rm = TRUE),
        avg_chloro = mean(chlorophyll, na.rm = TRUE)
      )
    
    plot_ly(habitat_summary) %>%
      add_trace(x = ~state, y = ~avg_sst, type = 'bar', name = 'Avg SST (°C)',
                marker = list(color = '#e74c3c')) %>%
      add_trace(x = ~state, y = ~avg_chloro * 10, type = 'bar', name = 'Avg Chlorophyll (x10)',
                marker = list(color = '#27ae60'), yaxis = 'y2') %>%
      layout(title = paste("Habitat Preferences for", input$speciesSelect),
             xaxis = list(title = "Behavior State"),
             yaxis = list(title = "Temperature (°C)", side = 'left'),
             yaxis2 = list(title = "Chlorophyll (mg/m³)", side = 'right', overlaying = "y", showgrid = FALSE),
             barmode = 'group',
             legend = list(orientation = 'h', x = 0.5, y = 1.1))
  })
  
  # Reward Map
  output$rewardMap <- renderLeaflet({
    req(Ocean_grid_rewards)
    
    reward_pal <- colorNumeric("viridis", domain = Ocean_grid_rewards$reward)
    
    leaflet(Ocean_grid_rewards) %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      addPolygons(
        fillColor = ~reward_pal(reward),
        fillOpacity = 0.7,
        stroke = FALSE,
        popup = ~paste(
          "Reward Score:", round(reward, 3), "<br>",
          "Mean Chlorophyll:", round(mean_chloro, 3), "<br>",
          "Mean SST:", round(mean_SST, 1), "°C"
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = reward_pal,
        values = ~reward,
        title = "Environmental Reward Score"
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)