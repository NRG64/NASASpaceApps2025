
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(geosphere)
library(markovchain)
library(leafem)


Full_Shark_Data <- read.csv("~/Documents/GitHub/NASASpaceApps2025/shark_data_fully_imputed_complete.csv")

-
print("[GLOBAL] Building behavioral model...")

correct_depth_col <- "depth"
correct_chloro_col <- "chlorophyll"
correct_sst_col <- "sst"


all_states <- c("resting", "foraging", "migrating")
Shark_states_daily <- Full_Shark_Data %>%
  filter(latitude >= -90 & latitude <= 90 & longitude >= -180 & longitude <= 180) %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(id, date) %>%
  summarise(
    latitude = mean(latitude),
    longitude = mean(longitude),
    depth = mean(bathymetry, na.rm = TRUE),
    chlorophyll = mean(chlorophyll, na.rm = TRUE),
    sst = mean(sst, na.rm = TRUE)
  ) %>%
  arrange(id, date) %>%
  group_by(id) %>%
  mutate(
    next_lat = lead(latitude),
    next_lon = lead(longitude),
    next_date = lead(date),
    time_diff_days = as.numeric(difftime(next_date, date, units = "days")),
    dist_m = distHaversine(cbind(longitude, latitude), cbind(next_lon, next_lat)),
    speed_mpd = dist_m / time_diff_days,  # meters per day
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

transition_counts <- Shark_states_daily %>% filter(!is.na(next_state)) %>% count(state, next_state) %>%
  right_join(expand.grid(state=all_states, next_state=all_states), by = c("state", "next_state")) %>%
  mutate(n = ifelse(is.na(n), 0, n))
transition_prob <- transition_counts %>% group_by(state) %>%
  mutate(prob = n / sum(n), prob = ifelse(is.nan(prob), 0, prob)) %>%
  ungroup() %>% select(state, next_state, prob) %>%
  pivot_wider(names_from = next_state, values_from = prob)
behavior_matrix <- as.matrix(transition_prob[, -1]); rownames(behavior_matrix) <- transition_prob$state
if (any(rowSums(behavior_matrix) == 0)) {
  for (i in which(rowSums(behavior_matrix) == 0)) { behavior_matrix[i, i] <- 1 }
}

print("[GLOBAL] Building spatial models...")
grid_cell_size <- 0.5; assignment_file_path <- "shark_grid_assignment.rds"
if (file.exists(assignment_file_path)) {
  shark_grid_assignment <- readRDS(assignment_file_path)
  ocean_grid <- st_make_grid(st_as_sf(Shark_states_daily, coords=c("longitude","latitude"), crs=4326), cellsize=grid_cell_size) %>% st_as_sf() %>% mutate(grid_id = 1:n())
} else {
  shark_sf_daily <- st_as_sf(Shark_states_daily, coords=c("longitude","latitude"), crs=4326)
  ocean_grid <- st_make_grid(shark_sf_daily, cellsize=grid_cell_size) %>% st_as_sf() %>% mutate(grid_id = 1:n())
  shark_grid_assignment <- st_join(shark_sf_daily, ocean_grid, join = st_within)
  saveRDS(shark_grid_assignment, file = assignment_file_path)
}
Ocean_grid_rewards <- shark_grid_assignment %>% as_tibble() %>% group_by(grid_id) %>%
  summarise(mean_chloro = mean(chlorophyll, na.rm=TRUE), mean_SST = mean(sst, na.rm=TRUE)) %>%
  filter(!is.na(mean_chloro) & !is.na(mean_SST)) %>%
  mutate(
    chloro_norm = (mean_chloro - min(mean_chloro))/(max(mean_chloro)-min(mean_chloro)),
    sst_norm = 1 - abs(mean_SST - 16)/(max(mean_SST) - 16),
    reward = (0.6 * chloro_norm) + (0.4 * sst_norm)
  ) %>% right_join(ocean_grid, by="grid_id") %>% st_as_sf() %>% filter(!is.na(reward))

print("--- [GLOBAL] Pre-computation complete. Starting Shiny app. ---")

# ===================================================================
# --- SHINY UI (USER INTERFACE) ---
# ===================================================================
ui <- fluidPage(
  titlePanel("Unified Shark Prediction & Behavior Analysis App"),
  div(class="outer",
      tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0;}"),
      leafletOutput("map", width="100%", height="100%"),
      absolutePanel(id="controls", class="panel panel-default", fixed=TRUE, draggable=TRUE,
                    top=60, left="auto", right=20, bottom="auto", width=350, height="auto",
                    h3("Analysis Panel"),
                    p("Click on the map to analyze a location."), uiOutput("analysis_tabs")
      )
  )
)


  server <- function(input, output, session) {
    
    # --- Initial Map Rendering ---
    output$map <- renderLeaflet({
      leaflet() %>% addProviderTiles(providers$Esri.OceanBasemap, group = "Ocean Basemap") %>%
        addProviderTiles(providers$CartoDB.Positron, group = "Light Basemap") %>%
        
        addPolygons(
          data = Ocean_grid_rewards, fillColor = ~colorNumeric("Greens", mean_chloro)(mean_chloro),
          fillOpacity = 0.7, stroke = FALSE, group = "Chlorophyll Overlay",
          popup = ~paste("Mean Chlorophyll:", round(mean_chloro, 2))
        ) %>%
        
        addPolygons(
          data = Ocean_grid_rewards, fillColor = ~colorNumeric("viridis", reward)(reward),
          fillOpacity = 0.7, stroke = FALSE, group = "Reward Grid",
          popup = ~paste("Reward Score:", round(reward, 2))
        ) %>%
        
        # <<< THE FIX IS IN THIS FUNCTION CALL >>>
        addCircleMarkers(
          data = st_as_sf(Shark_states_daily, coords = c("longitude", "latitude"), crs = 4326),
          
          # Use 'fillColor' for the main, dynamic color of the circle
          fillColor = ~case_when(
            state == "resting" ~ "orange",
            state == "foraging" ~ "green",
            state == "migrating" ~ "dodgerblue",
            TRUE ~ "grey"
          ),
          
          radius = 3,
          stroke = TRUE,       # This enables the outline
          weight = 0.5,
          color = "black",     # This now correctly and uniquely sets the outline color
          fillOpacity = 0.8,
          group = "Historical Tracks",
          popup = ~paste("Date:", date, "<br>Behavior:", state)
        ) %>%
        
        addLayersControl(
          baseGroups = c("Ocean Basemap", "Light Basemap"),
          overlayGroups = c("Reward Grid", "Chlorophyll Overlay", "Historical Tracks"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        hideGroup("Chlorophyll Overlay")
    })
    
    # --- Event Observer for Map Clicks (This part is already correct) ---
    observeEvent(input$map_click, {
      click <- input$map_click
      click_point <- st_as_sf(data.frame(lon = click$lng, lat = click$lat), coords = c("lon", "lat"), crs = 4326)
      
      clicked_cell_index <- st_nearest_feature(click_point, Ocean_grid_rewards)
      clicked_cell <- Ocean_grid_rewards[clicked_cell_index, ]
      clicked_cell_id <- clicked_cell$grid_id
      
      neighbor_indices <- st_touches(clicked_cell, Ocean_grid_rewards)[[1]]
      predicted_cell_sf <- NULL
      if (length(neighbor_indices) > 0) {
        neighbor_data <- Ocean_grid_rewards[neighbor_indices, ]
        if (sum(neighbor_data$reward) > 0) {
          predicted_cell_id <- sample(neighbor_data$grid_id, size = 1, prob = neighbor_data$reward)
          predicted_cell_sf <- Ocean_grid_rewards %>% filter(grid_id == predicted_cell_id)
        }
      }
      
      behavior_in_cell <- shark_grid_assignment %>% filter(grid_id == clicked_cell_id) %>% count(state) %>% mutate(prob = n / sum(n))
      current_state_probs <- tibble(state = all_states) %>% left_join(behavior_in_cell, by = "state") %>% mutate(prob = ifelse(is.na(prob), 0, prob)) %>% pull(prob)
      next_state_probs <- current_state_probs %*% behavior_matrix
      
      output$analysis_tabs <- renderUI({
        pred_text <- if (!is.null(predicted_cell_sf)) "The predicted next location is shown on the map with a purple cross." else "No valid neighbor cells to predict a move."
        current_behavior_html <- if(nrow(behavior_in_cell) > 0) paste(sprintf("<b>%s:</b> %.1f%%", behavior_in_cell$state, behavior_in_cell$prob * 100), collapse="<br/>") else "No historical data."
        next_behavior_html <- paste(sprintf("<b>%s:</b> %.1f%%", all_states, next_state_probs * 100), collapse="<br/>")
        tabsetPanel( tabPanel("Prediction", h4("Next Location Prediction"), p(pred_text)),
                     tabPanel("Behavior Analysis", h4("Current Behavior Profile"), HTML(current_behavior_html), hr(), h4("Predicted Next State Probabilities"), HTML(next_behavior_html)))
      })
      
      proxy <- leafletProxy("map") %>% clearGroup("analysis_group") %>%
        addAwesomeMarkers(lng=click$lng, lat=click$lat, icon=awesomeIcons(icon="info-circle", library="fa", markerColor="blue"), group="analysis_group")
      
      if (!is.null(predicted_cell_sf)) {
        pred_coords <- st_coordinates(st_centroid(predicted_cell_sf))
        proxy %>% addAwesomeMarkers(lng=pred_coords[1], lat=pred_coords[2], icon=awesomeIcons(icon="plus", library="fa", markerColor="purple"), group="analysis_group") %>%
          addArrowhead(lng0=click$lng, lat0=click$lat, lng1=pred_coords[1], lat1=pred_coords[2], color="black", weight=2, group="analysis_group")
      }
    })
  }
shinyApp(ui, server)