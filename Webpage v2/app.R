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

# Load and preprocess your actual data
# NOTE: This line requires 'shark_data_fully_imputed_complete.csv' to exist
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

# Conditional check for assignment file. Ensure ocean_grid is created.
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
    # Normalize chlorophyll
    chloro_norm = (mean_chloro - min(mean_chloro)) / (max(mean_chloro) - min(mean_chloro)),
    # SST preference (closer to 16C is better)
    sst_norm = 1 - abs(mean_SST - 16) / (max(mean_SST) - 16),
    # Combined reward score
    reward = (0.6 * chloro_norm) + (0.4 * sst_norm)
  ) %>%
  # Join with spatial grid for mapping
  right_join(ocean_grid, by = "grid_id") %>%
  st_as_sf() %>%
  filter(!is.na(reward))

# Get all unique species names for the dropdown
all_species_choices <- sort(unique(Shark_states_daily$species_simple))
species_dropdown_choices <- c("All", all_species_choices)

# UI Definition
ui <- fluidPage(
  # 1. Custom Styles for a Modern Look (MODIFIED)
  tags$head(
    tags$style(HTML("
            /* Google Font: Montserrat */
            @import url('https://fonts.googleapis.com/css2?family=Montserrat:wght@400;600;700&display=swap');
            
            body {
                font-family: 'Montserrat', sans-serif;
                background-color: #f4f7f6; /* Light gray background */
            }
            .sidebar {
                background-color: #2c3e50; /* Dark Navy Blue for Sidebar */
                color: white; /* Default text color for the sidebar */
                padding: 15px;
                box-shadow: 2px 0 5px rgba(0,0,0,0.1);
            }
            .title-panel {
                background-color: #34495e; /* Slightly lighter shade for title */
                padding: 20px 0;
                margin-bottom: 20px;
                text-align: center;
                border-bottom: 3px solid #1abc9c; /* Teal accent */
            }
            .title-panel h1 {
                color: white;
                font-weight: 700;
                margin: 0;
            }
            .metric-card {
                background: white;
                border-left: 5px solid; /* Placeholder for color */
                border-radius: 8px;
                padding: 15px;
                margin-bottom: 20px;
                box-shadow: 0 2px 4px rgba(0,0,0,0.05);
            }
            
            /* FIX 1: Set the metric labels (p, h5) to black */
            .metric-card p, .metric-card h5 { 
                color: #333333; /* Dark gray/black for labels inside white cards */
            }
            
            /* FIX 2: Set the metric values (the numbers/text output) to black */
            .metric-value {
                font-weight: 700;
                font-size: 2.0em;
                margin: 5px 0 5px;
                color: #2c3e50; /* Dark navy color for strong contrast against white background */
            }
            
            /* Metric Card Colors */
            #card-obs { border-left-color: #e74c3c; } /* Red */
            #card-species { border-left-color: #3498db; } /* Blue */
            #card-coverage { border-left-color: #27ae60; } /* Green */
            #card-time { border-left-color: #f39c12; } /* Orange */
            
            /* Tab Styling */
            .nav-tabs > li > a {
                color: #2c3e50;
                font-weight: 600;
            }
            .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
                color: #1abc9c !important;
                border-top: 3px solid #1abc9c;
            }
            
        "))
  ),
  
  # 2. Main Title Header (UNCHANGED)
  div(class = "title-panel",
      h1("🦈 Shark Tracker Dashboard")
  ),
  
  # 3. Main Dashboard Layout: Sidebar + Body
  sidebarLayout(
    # 3a. Sidebar for Metrics and Global Filters (UNCHANGED)
    sidebarPanel(
      class = "sidebar",
      width = 3,
      
      # Application Metrics
      h3(icon("tachometer-alt"), "Key Metrics"),
      hr(),
      
      # Total Observations
      div(id = "card-obs", class = "metric-card",
          p("Total Observations"),
          div(class = "metric-value", textOutput("total_obs"))
      ),
      
      # Species Tracked
      div(id = "card-species", class = "metric-card",
          p("Species Tracked"),
          div(class = "metric-value", textOutput("species_count"))
      ),
      
      # Data Coverage
      div(id = "card-coverage", class = "metric-card",
          p("Data Coverage"),
          div(class = "metric-value", "100%")
      ),
      
      # Time Span
      div(id = "card-time", class = "metric-card",
          p("Tracking Duration"),
          div(class = "metric-value", textOutput("time_span"))
      ),
      
      hr(),
      
      # Global Controls 
      h3(icon("sliders-h"), "Controls"),
      
      selectInput("speciesSelect", "Filter Habitat Preferences:",
                  choices = species_dropdown_choices,
                  selected = "All"),
      
      hr(),
      
      # Markov Prediction Controls
      h4(icon("bullseye"), "Behavior Predictor"),
      selectInput("currentState", "Current State:",
                  choices = all_states, selected = "foraging"),
      numericInput("steps", "Prediction Steps (Days):",
                   value = 1, min = 1, max = 10, step = 1),
      
      # The output for the prediction
      div(class = "metric-card",
          style = "border-left-color: #1abc9c;", 
          h5("Predicted Distribution:"),
          plotlyOutput("predictionPlot", height = "120px")
      )
      
    ), # End sidebarPanel
    
    # 3b. Main Content Body with Tabs (UNCHANGED)
    mainPanel(
      width = 9,
      
      tabsetPanel(
        id = "main_tabs",
        
        # Tab 1: Geographical and Movement Analysis
        tabPanel(title = div(icon("globe-americas"), "Movement & Habitat"),
                 br(),
                 
                 fluidRow(
                   column(8, 
                          div(class = "metric-card",
                              h4(icon("map-marked-alt"), "Shark Movement & Behavior"),
                              leafletOutput("movementMap", height = "600px")
                          )
                   ),
                   column(4,
                          div(class = "metric-card",
                              h4(icon("chart-pie"), "Behavior Distribution"),
                              plotlyOutput("behaviorPlot", height = "300px")
                          ),
                          div(class = "metric-card",
                              h4(icon("sun"), "Habitat Preferences"),
                              plotlyOutput("habitatPrefs", height = "280px")
                          )
                   )
                 ),
                 
                 div(class = "metric-card",
                     h4(icon("chart-bar"), "Behavior by Species"),
                     plotlyOutput("speciesBehavior", height = "300px")
                 )
        ),
        
        # Tab 2: Markov and Reward Analysis
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
                 
                 # Markov Chain Plot (moved to the bottom for space)
                 div(class = "metric-card",
                     h4(icon("code-branch"), "Markov Chain Transition Probabilities"),
                     plotOutput("markovPlot", height = "400px")
                 )
        )
      ) # End tabsetPanel
    ) # End mainPanel
  ) # End sidebarLayout
) # End fluidPage

# Server Logic
server <- function(input, output, session) {
  
  # Reactive data processing
  processedData <- reactive({
    Shark_states_daily
  })
  
  # Reactive Markov Prediction
  predicted_state <- reactive({
    req(input$currentState, input$steps)
    
    # Check if the markov chain object exists and has data
    if (!exists("mc") || is.null(mc)) {
      return(NULL)
    }
    
    # FIX: Use the correct matrix power operator from the 'expm' package: %^%
    # This replaces the erroneous 'matpow(mc@transitionMatrix, input$steps)'
    p_steps <- mc@transitionMatrix %^% input$steps
    
    # The starting state is a vector with 1 in the starting state index
    start_state_vec <- rep(0, length(all_states))
    names(start_state_vec) <- all_states
    start_state_vec[input$currentState] <- 1
    
    # Calculate the distribution after n steps
    next_dist <- start_state_vec %*% p_steps
    
    # Format the result for plotting
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
  
  # Movement Map
  output$movementMap <- renderLeaflet({
    shark_sf <- st_as_sf(processedData(), coords = c("longitude", "latitude"), crs = 4326)
    
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
  
  # Behavior Distribution
  output$behaviorPlot <- renderPlotly({
    behavior_summary <- processedData() %>%
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
  
  # NEW: Prediction Plot
  output$predictionPlot <- renderPlotly({
    pred_data <- predicted_state()
    if (is.null(pred_data)) {
      return(NULL)
    }
    
    # Custom color mapping for states
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
  
  # Habitat Preferences
  output$habitatPrefs <- renderPlotly({
    filtered_data <- if(input$speciesSelect == "All") {
      processedData()
    } else {
      processedData() %>% filter(species_simple == input$speciesSelect)
    }
    
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
  
  # Reward Map (FIXED/REFINED)
  output$rewardMap <- renderLeaflet({
    # Ensure data is reactive and valid
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