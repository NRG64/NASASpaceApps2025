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
library(raster)
library(leaflet)
library(viridis)
library(RColorBrewer)
library(rnaturalearth)

select <- dplyr::select
# --- NEW: Load Global Land Polygon ONCE ---
# This will download the shapefile the first time it's run and then use the local copy.
# Use a smaller scale for faster loading if extreme detail isn't needed.
# 'large' provides good detail. 'medium' or 'small' can be used if performance is an issue.
world_land <- ne_countries(scale = "medium", type = "map_units", returnclass = "sf") %>%
  st_geometry() # We only need the geometry (polygons)

# Ensure the CRS is WGS84 (EPSG:4326) for consistency with Leaflet
world_land <- st_transform(world_land, crs = 4326)
# Load and preprocess your actual data
# Assuming shark_data_fully_imputed_complete.csv is available
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
      grepl("Tiger", species, ignore.case = TRUE) ~ "Tiger Shark",
      grepl("Hammerhead", species, ignore.case = TRUE) ~ "Hammerhead Shark",
      grepl("Blue", species, ignore.case = TRUE) ~ "Blue Shark",
      TRUE ~ species  # Keep original if no match
    )
  )

# Shark species educational information
shark_species_info <- list(
  "White Shark" = list(
    scientific_name = "Carcharodon carcharias",
    size = "Up to 6.4 m (21 ft)",
    weight = "Up to 1,100 kg (2,400 lb)",
    lifespan = "70+ years",
    habitat = "Coastal and offshore waters worldwide",
    diet = "Marine mammals, fish, seabirds",
    conservation_status = "Vulnerable",
    fun_facts = c(
      "Can detect a single drop of blood in 25 gallons of water",
      "Has about 300 serrated teeth arranged in 7 rows",
      "Body temperature can be 10-15°C warmer than surrounding water",
      "Known for breaching completely out of the water when hunting seals"
    ),
    image_url = "https://upload.wikimedia.org/wikipedia/commons/5/56/White_shark.jpg"
  ),
  "Mako Shark" = list(
    scientific_name = "Isurus oxyrinchus",
    size = "Up to 4 m (13 ft)",
    weight = "Up to 570 kg (1,260 lb)",
    lifespan = "30-35 years",
    habitat = "Tropical and temperate waters worldwide",
    diet = "Fish, squid, other sharks",
    conservation_status = "Endangered",
    fun_facts = c(
      "Fastest shark species - can swim up to 60 mph",
      "Can leap up to 9 meters (30 feet) out of the water",
      "Has one of the largest brain-to-body ratios of all sharks",
      "Known for their incredible agility and speed"
    ),
    image_url = "https://upload.wikimedia.org/wikipedia/commons/9/9e/Isurus_oxyrinchus2.jpg"
  ),
  "Tiger Shark" = list(
    scientific_name = "Galeocerdo cuvier",
    size = "Up to 5.5 m (18 ft)",
    weight = "Up to 900 kg (2,000 lb)",
    lifespan = "30-40 years",
    habitat = "Tropical and subtropical waters worldwide",
    diet = "Anything - fish, seals, birds, dolphins, turtles, garbage",
    conservation_status = "Near Threatened",
    fun_facts = c(
      "Known as the 'garbage can of the sea' - eats almost anything",
      "Has distinctive tiger-like stripes that fade with age",
      "One of the few shark species that hunts sea turtles",
      "Has serrated teeth that can slice through turtle shells"
    ),
    image_url = "https://upload.wikimedia.org/wikipedia/commons/3/39/Tiger_shark.jpg"
  ),
  "Hammerhead Shark" = list(
    scientific_name = "Sphyrna spp.",
    size = "Up to 6 m (20 ft)",
    weight = "Up to 580 kg (1,280 lb)",
    lifespan = "20-30 years",
    habitat = "Warm tropical waters worldwide",
    diet = "Fish, squid, octopus, crustaceans",
    conservation_status = "Endangered",
    fun_facts = c(
      "Hammer-shaped head provides 360-degree vision",
      "Uses head to pin stingrays to the seafloor while eating",
      "Schools of up to 100 individuals during migration",
      "Has specialized electroreceptors in its head to detect prey"
    ),
    image_url = "https://upload.wikimedia.org/wikipedia/commons/5/5f/Hammerhead_shark.jpg"
  ),
  "Blue Shark" = list(
    scientific_name = "Prionace glauca",
    size = "Up to 3.8 m (12.5 ft)",
    weight = "Up to 205 kg (450 lb)",
    lifespan = "15-20 years",
    habitat = "Deep temperate and tropical waters worldwide",
    diet = "Fish, squid, seabirds",
    conservation_status = "Near Threatened",
    fun_facts = c(
      "One of the most widespread shark species",
      "Can migrate across entire ocean basins",
      "Slender body allows for efficient long-distance swimming",
      "Gives birth to live young - up to 135 pups at once"
    ),
    image_url = "https://upload.wikimedia.org/wikipedia/commons/9/9e/Blue_shark.jpg"
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

# Shark Habitat analysis
grid_cell_size <- 0.5
assignment_file_path <- "shark_grid_assignment.rds"

shark_sf_daily <- st_as_sf(Shark_states_daily, coords = c("longitude", "latitude"), crs = 4326)

# The file.exists check might be problematic in some execution environments.
# For demonstration purposes, we'll ensure ocean_grid is always created.
# In a real app, you'd handle file existence carefully.

# Ensure that ocean_grid is always defined for further operations
ocean_grid <- st_make_grid(shark_sf_daily, cellsize = c(grid_cell_size, grid_cell_size)) %>%
  st_as_sf() %>%
  mutate(grid_id = 1:n())

# If the assignment file exists, load it, otherwise create and save.
if (file.exists(assignment_file_path)) {
  shark_grid_assignment <- readRDS(assignment_file_path)
} else {
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
    chloro_norm = (mean_chloro - min(mean_chloro, na.rm=TRUE)) / (max(mean_chloro, na.rm=TRUE) - min(mean_chloro, na.rm=TRUE)),
    sst_norm = 1 - abs(mean_SST - 16) / (max(mean_SST, na.rm=TRUE) - min(mean_SST, na.rm=TRUE)), # Normalized distance from 16C
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

# Function to check if a point is in ocean - NOW ALWAYS TRUE
# This allows clicking anywhere, including land, and the app will process it.
is_ocean_point <- function(lat, lng, land_polygons) {
  clicked_point <- st_point(c(lng, lat)) %>%
    st_sfc(crs = 4326)
  intersects_land <- st_intersects(clicked_point, land_polygons, sparse = FALSE)[1]
  return(!intersects_land) # Return TRUE if it is an ocean point
}


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
      
      .btn-warning {
          background-color: #f39c12;
          border-color: #f39c12;
          width: 100%;
      }
      
      .status-message {
          padding: 10px;
          border-radius: 5px;
          margin: 10px 0;
          text-align: center;
          font-weight: bold;
      }
      
      .status-success {
          background-color: #d4edda;
          color: #155724;
          border: 1px solid #c3e6cb;
      }
      
      .status-error {
          background-color: #f8d7da;
          color: #721c24;
          border: 1px solid #f5c6cb;
      }
      
      .status-info {
          background-color: #d1ecf1;
          color: #0c5460;
          border: 1px solid #bee5eb;
      }
      
      /* Shark Species Card Styles */
      .species-card {
          background: white;
          border-radius: 10px;
          padding: 20px;
          margin-bottom: 20px;
          box-shadow: 0 4px 6px rgba(0,0,0,0.1);
          border-top: 5px solid #3498db;
      }
      
      .species-header {
          display: flex;
          align-items: center;
          margin-bottom: 15px;
          border-bottom: 2px solid #ecf0f1;
          padding-bottom: 10px;
      }
      
      .species-image {
          width: 120px;
          height: 90px;
          object-fit: cover;
          border-radius: 8px;
          margin-right: 15px;
      }
      
      .species-title {
          flex: 1;
      }
      
      .species-title h3 {
          color: #2c3e50;
          margin: 0 0 5px 0;
      }
      
      .species-title .scientific-name {
          color: #7f8c8d;
          font-style: italic;
          margin: 0;
      }
      
      .species-stats {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
          gap: 15px;
          margin-bottom: 20px;
      }
      
      .stat-item {
          background: #f8f9fa;
          padding: 10px;
          border-radius: 5px;
          border-left: 3px solid #3498db;
      }
      
      .stat-label {
          font-weight: 600;
          color: #2c3e50;
          font-size: 0.9em;
      }
      
      .stat-value {
          color: #34495e;
          font-weight: 700;
      }
      
      .fun-facts {
          background: #e8f4f8;
          padding: 15px;
          border-radius: 8px;
          margin-top: 15px;
      }
      
      .fun-facts h5 {
          color: #2980b9;
          margin-top: 0;
      }
      
      .fun-facts ul {
          margin-bottom: 0;
      }
      
      .fun-facts li {
          margin-bottom: 8px;
          color: #2c3e50;
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
                   ),column(4,
                            div(class = "prediction-panel",
                                h4("📍 Prediction Results"),
                                
                                div(id = "interactiveClickStatus", class = "status-message status-info",
                                    "Click on the map to predict shark behavior"
                                ),
                                
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
                                                               "Shark Observations" = "observations"),
                                                   selected = "foraging")
                            )
                   )
                 )
        ),
        
        # UPDATED: Movement Predictor Tab with click-based simulation
        tabPanel(title = div(icon("route"), "Movement Predictor"),
                 br(),
                 
                 fluidRow(
                   column(4,
                          div(class = "prediction-panel",
                              h4(icon("mouse-pointer"), "Click to Start Prediction"),
                              p("Click on any location to set the starting point for the simulation"),
                              
                              div(id = "clickStatus", class = "status-message status-info",
                                  "Click on the map to select a starting location"
                              ),
                              
                              hr(),
                              
                              h4(icon("sliders-h"), "Prediction Controls"),
                              
                              selectInput("predictionSpecies", "Species for Prediction:",
                                          choices = species_dropdown_choices,
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
                              h4(icon("map-marked-alt"), "Click on Map to Start Prediction"),
                              p("Selected location: ", strong(textOutput("selectedLocation", inline = TRUE))),
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
        
        # NEW: Shark Species Educational Tab
        tabPanel(title = div(icon("fish"), "Shark Species"),
                 br(),
                 
                 div(class = "metric-card",
                     h4(icon("book"), "Shark Species Educational Guide"),
                     p("Learn about the amazing shark species tracked in this dashboard. Each species has unique adaptations that make them perfect ocean predators.")
                 ),
                 
                 # Generate species cards dynamically
                 uiOutput("speciesCards")
        ),
        
        tabPanel(title = div(icon("brain"), "Advanced Analysis"),
                 br(),
                 fluidRow(
                   
                   column(5,
                          div(class = "metric-card",
                              h4(icon("table"), "State Transition Matrix"),
                              tableOutput("transitionMatrix")
                          ),
                          div(class = "metric-card",
                              h4(icon("chart-area"), "Shark Habitat Score"),
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
    
    return(data)
  })
  
  # Reactive values for predictive map
  click_data <- reactiveValues(
    last_click = NULL,
    prediction = NULL
  )
  
  # Reactive values for movement predictor
  prediction_results <- reactiveValues(
    start_point = NULL,
    paths = NULL,
    heatmap_data = NULL,
    last_prediction = NULL
  )
  
  # NEW: Generate species cards UI
  output$speciesCards <- renderUI({
    species_list <- lapply(names(shark_species_info), function(species_name) {
      info <- shark_species_info[[species_name]]
      
      # Check if this species exists in our data
      species_in_data <- species_name %in% Shark_states_daily$species_simple
      data_indicator <- if(species_in_data) {
        tags$span(icon("check-circle"), " Tracked in this dataset", 
                  style = "color: #27ae60; font-weight: bold;")
      } else {
        tags$span(icon("info-circle"), " Species reference", 
                  style = "color: #f39c12;")
      }
      
      div(class = "species-card",
          div(class = "species-header",
              img(src = info$image_url, class = "species-image", 
                  alt = paste(species_name, "image")),
              div(class = "species-title",
                  h3(species_name),
                  p(class = "scientific-name", info$scientific_name),
                  data_indicator
              )
          ),
          
          div(class = "species-stats",
              div(class = "stat-item",
                  div(class = "stat-label", "Maximum Size"),
                  div(class = "stat-value", info$size)
              ),
              div(class = "stat-item",
                  div(class = "stat-label", "Maximum Weight"),
                  div(class = "stat-value", info$weight)
              ),
              div(class = "stat-item",
                  div(class = "stat-label", "Lifespan"),
                  div(class = "stat-value", info$lifespan)
              ),
              div(class = "stat-item",
                  div(class = "stat-label", "Conservation Status"),
                  div(class = "stat-value", 
                      style = paste0("color: ", 
                                     ifelse(info$conservation_status %in% c("Endangered", "Critically Endangered"), "#e74c3c",
                                            ifelse(info$conservation_status == "Vulnerable", "#f39c12", "#27ae60")),
                                     "; font-weight: bold;"),
                      info$conservation_status)
              )
          ),
          
          div(class = "stat-item",
              div(class = "stat-label", "Primary Habitat"),
              div(class = "stat-value", info$habitat)
          ),
          
          div(class = "stat-item",
              div(class = "stat-label", "Diet"),
              div(class = "stat-value", info$diet)
          ),
          
          div(class = "fun-facts",
              h5(icon("star"), " Amazing Facts"),
              tags$ul(
                lapply(info$fun_facts, function(fact) {
                  tags$li(fact)
                })
              )
          )
      )
    })
    
    do.call(tagList, species_list)
  })
  
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
  
  # Selected location display
  output$selectedLocation <- renderText({
    if (!is.null(prediction_results$start_point)) {
      paste("Lat:", round(prediction_results$start_point$lat, 4), 
            "Lng:", round(prediction_results$start_point$lng, 4))
    } else {
      "No location selected"
    }
  })
  
  # Prediction settings display
  output$predictionSettings <- renderText({
    paste(
      "Species:", input$predictionSpecies, "\n",
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
      
      # 2. Predict movement based on state + shark habitat
      movement <- predictMovementDirection(current_lat, current_lng, next_state)
      
      # 3. Update position (with boundary checks and ocean constraints)
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
  observeEvent(input$predictionMap_click, {
    click <- input$predictionMap_click
    lat <- click$lat
    lng <- click$lng
    
    # If the click is on land, just stop. Do nothing.
    if (!is_ocean_point(lat, lng, world_land)) {
      return() # Silently exit the observer.
    }
    
    # If the click is in the ocean, proceed as normal.
    prediction_results$start_point <- list(
      lat = lat,
      lng = lng,
      state = input$startBehavior
    )
    
    # Update status message to show success
    shinyjs::html("clickStatus",
                  paste0("<div class='status-message status-success'>",
                         "✓ Location selected: ", round(lat, 4), ", ", round(lng, 4),
                         "</div>"))
    
    # Update map with a marker for the valid start point
    leafletProxy("predictionMap") %>%
      clearMarkers() %>%
      addMarkers(
        lng = lng, lat = lat,
        popup = paste("Start Point:", input$startBehavior)
      )
  })
  
  # Run prediction when button is clicked
  observeEvent(input$runPrediction, {
    # Check if a start point has been selected
    if (is.null(prediction_results$start_point)) {
      shinyjs::html("clickStatus", 
                    paste0("<div class='status-message status-error'>",
                           "✗ Please select a starting location first by clicking on the map",
                           "</div>"))
      return()
    }
    
    start_point <- prediction_results$start_point
    
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
    
    # Update status message
    shinyjs::html("clickStatus", 
                  paste0("<div class='status-message status-success'>",
                         "✓ Prediction completed with ", input$simulationCount, " simulations",
                         "</div>"))
    
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
      ) %>%
      htmlwidgets::onRender("
        function(el, x) {
          this.on('click', function(e) {
            Shiny.setInputValue('predictionMap_click', {
              lat: e.latlng.lat,
              lng: e.latlng.lng
            });
          });
        }
      ")
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
      clearShapes() %>%
      # Ensure other markers/lines from prediction are cleared to avoid accumulation
      clearMarkers() %>% 
      clearPopups() %>%
      removeMarker(layerId = "prediction_marker") %>%
      removeShape(layerId = "prediction_arrow")
    
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
  
  # FIXED: Better movement prediction logic (assumes ocean location)
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
  # Handle map clicks for predictions - NO LAND PREVENTION
  observeEvent(input$predictiveMap_click, {
    click <- input$predictiveMap_click
    lat <- click$lat
    lng <- click$lng
    
    # If the click is on land, just stop. Do nothing.
    if (!is_ocean_point(lat, lng, world_land)) {
      return() # Silently exit the observer.
    }
    
    # If the click is in the ocean, proceed as normal.
    click_data$last_click <- list(lat = lat, lng = lng)
    
    shinyjs::html("interactiveClickStatus",
                  paste0("<div class='status-message status-success'>",
                         "✓ Location selected: ", round(lat, 4), ", ", round(lng, 4),
                         "</div>"))
    
    # Predict behavior and movement
    prediction <- predictSharkBehavior(lat, lng)
    click_data$prediction <- prediction
    
    # Update map with prediction marker and arrow
    leafletProxy("predictiveMap") %>%
      clearMarkers() %>%
      clearShapes() %>%
      addMarkers(
        lng = lng, lat = lat,
        popup = paste("Predicted Behavior:", prediction$behavior),
        layerId = "prediction_marker"
      ) %>%
      addPolylines(
        lng = c(lng, lng + prediction$move_lng),
        lat = c(lat, lat + prediction$move_lat),
        color = "red", weight = 3,
        opacity = 0.8,
        layerId = "prediction_arrow"
      )
  })
  
  # Movement arrow plot
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
      "Select a location first"
    }
  })
  
  output$envScore <- renderText({
    if (!is.null(click_data$prediction)) {
      paste("Score:", click_data$prediction$env_score, 
            "\nSST:", click_data$prediction$sst, "°C",
            "\nChlorophyll:", click_data$prediction$chlorophyll, "mg/m³")
    } else {
      "Select a location first"
    }
  })
  
  output$commonSpecies <- renderText({
    if (!is.null(click_data$prediction)) {
      paste(click_data$prediction$common_species, collapse = ", ")
    } else {
      "Select a location first"
    }
  })
  
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
      layout(title = "Shark Habitat Score",
             xaxis = list(title = "Habitat Score"),
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
}

# Run the application
shinyApp(ui = ui, server = server)