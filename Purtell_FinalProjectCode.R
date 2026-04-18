##############################
##### FINAL PROJECT CODE #####
##############################

############ Imports ############
setwd("C:/Users/cpurt/OneDrive/Documents/DS_STUFF/Urban Informatics/Final")
library(tidycensus)
library(tidyverse)
library(sf)
library(arrow)
library(lubridate)
library(tidytransit)
library(leaflet)
library(ggplot2)
library(htmlwidgets)

############ Data Acquisition ############
##########################################

########## MBTA Data ##########

### Small test to make sure everything works ###

index <- read_csv("https://performancedata.mbta.com/lamp/subway-on-time-performance-v1/index.csv")
head(index)

dates <- seq(as.Date("2024-03-04"), as.Date("2024-03-08"), by = "day")

# Filter index to our date range and weekdays only
target_dates <- index %>%
  filter(
    service_date >= as.Date("2024-04-01"),
    service_date <= as.Date("2025-03-31"),
    !weekdays(service_date) %in% c("Saturday", "Sunday")
  )

# Check how many days we're working with
nrow(target_dates)

# Test with just the first date to check column names
test <- read_parquet(target_dates$file_url[1])
glimpse(test)


### Full data pipeline ###
# Filter index to weekdays in our date range
target_dates <- index %>%
  filter(
    service_date >= as.Date("2024-04-01"),
    service_date <= as.Date("2025-03-31"),
    !weekdays(service_date) %in% c("Saturday", "Sunday")
  )

message("Total days to process: ", nrow(target_dates))

# Fetch, filter to peak hours, select key columns
fetch_and_filter <- function(url) {
  tryCatch({
    df <- read_parquet(url)
    
    df %>%
      filter(
        route_id %in% c("Red", "Orange", "Blue", 
                        "Green-B", "Green-C", "Green-D", "Green-E"),
        !is.na(headway_trunk_seconds),
        !is.na(scheduled_headway_trunk)
      ) %>%
      mutate(
        # Convert stop_timestamp to hour
        hour = hour(as_datetime(stop_timestamp))
      ) %>%
      filter(
        # AM peak 7-9am or PM peak 4-7pm
        (hour >= 7 & hour <= 9) | (hour >= 16 & hour <= 19)
      ) %>%
      select(
        service_date,
        route_id,
        trunk_route_id,
        parent_station,
        direction,
        headway_trunk_seconds,
        scheduled_headway_trunk
      )
    
  }, error = function(e) {
    message("Failed: ", url)
    NULL
  })
}

# Process month by month and save
target_dates <- target_dates %>%
  mutate(month = format(service_date, "%Y-%m"))

for (m in unique(target_dates$month)) {
  message("Processing ", m)
  
  month_urls <- target_dates %>% 
    filter(month == m) %>% 
    pull(file_url)
  
  month_data <- map_dfr(month_urls, fetch_and_filter)
  
  if (!is.null(month_data) && nrow(month_data) > 0) {
    saveRDS(month_data, paste0("mbta_", m, ".rds"))
    message("Saved ", m, " — ", nrow(month_data), " rows")
  }
  
  Sys.sleep(1)
}

# Load and combine
files <- list.files(pattern = "mbta_202", full.names = TRUE)
all_data <- map_dfr(files, readRDS)

# Aggregate to station level
station_headways <- all_data %>%
  group_by(parent_station, trunk_route_id) %>%
  summarise(
    avg_actual_headway_sec    = mean(headway_trunk_seconds, na.rm = TRUE),
    avg_scheduled_headway_sec = mean(scheduled_headway_trunk, na.rm = TRUE),
    n_observations            = n(),
    .groups = "drop"
  ) %>%
  mutate(
    headway_gap_sec = avg_actual_headway_sec - avg_scheduled_headway_sec,
    headway_gap_min = headway_gap_sec / 60
  ) %>%
  # Filter to stations with enough observations to be meaningful
  filter(n_observations >= 100)

extreme_station_stats <- all_data %>%
  mutate(
    headway_gap_sec = headway_trunk_seconds - scheduled_headway_trunk,
    headway_gap_min = headway_gap_sec / 60
  ) %>%
  group_by(parent_station, trunk_route_id) %>%
  summarise(
    p90_gap = quantile(headway_gap_min, 0.9, na.rm = TRUE),
    max_gap = max(headway_gap_min, na.rm = TRUE),
    extreme_share = mean(headway_gap_min > 3, na.rm = TRUE), # % of delays > 3 min
    .groups = "drop"
  )

saveRDS(station_headways, "mbta_station_headways_final.rds")
glimpse(station_headways)

# Quick summary of the gap distribution
summary(station_headways$headway_gap_min)

# Get station names
stops <- read_parquet(
  "https://performancedata.mbta.com/lamp/tableau/rail/LAMP_static_stops.parquet"
)

# Get one representative row per parent_station
# using the most recent static_version_key
station_names <- stops %>%
  filter(grepl("^place-", stop_id)) %>%
  group_by(stop_id) %>%
  slice_max(static_version_key, n = 1) %>%
  ungroup() %>%
  select(stop_id, stop_name)

# Check it
glimpse(station_names)
nrow(station_names)

# Join to headway data
station_headways <- station_headways %>%
  left_join(station_names, by = c("parent_station" = "stop_id"))

# Fix by removing the old failed column and renaming
station_headways <- station_headways %>%
  select(-stop_name.x) %>%
  rename(stop_name = stop_name.y)

# Now double-check if any are unmatched
station_headways %>% 
  filter(is.na(stop_name)) %>% 
  select(parent_station, trunk_route_id)

# Preview top 10
station_headways %>%
  select(stop_name, trunk_route_id, headway_gap_min) %>%
  arrange(desc(headway_gap_min)) %>%
  head(10)

# Retrieve coordinates for the stations for mapping purposes
gtfs <- read_gtfs("https://cdn.mbta.com/MBTA_GTFS.zip")

# Get metro lines
subway_routes <- gtfs$trips %>%
  left_join(gtfs$routes, by = "route_id") %>%
  filter(route_id %in% c(
    "Red", "Orange", "Blue",
    "Green-B", "Green-C", "Green-D", "Green-E"
  )) %>%
  distinct(shape_id, route_id)

t_lines <- gtfs$shapes %>%
  inner_join(subway_routes, by = "shape_id") %>%
  arrange(shape_id, shape_pt_sequence) %>%
  group_by(shape_id, route_id) %>%
  summarise(
    geometry = st_combine(
      st_as_sf(
        cur_data(),
        coords = c("shape_pt_lon", "shape_pt_lat"),
        crs = 4326
      )$geometry
    ),
    .groups = "drop"
  ) %>%
  st_as_sf() %>%
  st_cast("LINESTRING")

# Extract stops with coordinates
gtfs_stops <- gtfs$stops %>%
  filter(grepl("^place-", stop_id)) %>%
  select(stop_id, stop_lat, stop_lon) %>%
  mutate(
    stop_lat = as.numeric(stop_lat),
    stop_lon = as.numeric(stop_lon)
  )

# Join to headway data
station_headways <- station_headways %>%
  left_join(gtfs_stops, by = c("parent_station" = "stop_id"))

# Check for missing coordinates
station_headways %>% filter(is.na(stop_lat))

# Save the final dataset
saveRDS(station_headways, "station_headways_final.rds")

# Quick preview
station_headways %>%
  select(stop_name, trunk_route_id, headway_gap_min, stop_lat, stop_lon) %>%
  arrange(desc(headway_gap_min)) %>%
  head(10)

### Census Data
census_api_key("349523a2cb53d5737745ee9a1a2192ca9d185722")

variables <- c(
  total_households = "B08201_001",
  zero_vehicle_hh  = "B08201_002",
  median_income    = "B19013_001",
  poverty_total    = "B17001_001",
  poverty_below    = "B17001_002",
  total_pop        = "B03002_001",
  white_pop        = "B03002_003",
  black_pop        = "B03002_004",
  hispanic_pop     = "B03002_012"
)

census_ma <- get_acs(
  geography = "tract",
  variables = variables,
  state     = "MA",
  year      = 2023,
  geometry  = TRUE,
  output    = "wide"
)

tract_names <- get_acs(
  geography = "tract",
  variables = "B19013_001",  # any valid variable works
  state = "MA",
  year = 2023,
  geometry = FALSE,
  output = "tidy"
) %>%
  select(GEOID, NAME) %>%
  distinct()

# Clean up and derive percentages
census_clean <- census_ma %>%
  rename_with(~str_remove(., "E$"), ends_with("E")) %>%
  select(
    GEOID,
    total_households,
    zero_vehicle_hh,
    median_income,
    poverty_total,
    poverty_below,
    total_pop,
    white_pop,
    black_pop,
    hispanic_pop,
    geometry
  ) %>%
  mutate(
    pct_zero_vehicle = zero_vehicle_hh / total_households,
    pct_poverty = poverty_below / poverty_total,
    pct_white = white_pop / total_pop,
    pct_black = black_pop / total_pop,
    pct_hispanic = hispanic_pop / total_pop
  )

census_clean <- census_clean %>%
  left_join(tract_names, by = "GEOID")

census_clean <- census_clean %>%
  separate(
    NAME,
    into = c("tract_name", "county", "state"),
    sep = "; ",
    remove = FALSE
  )

saveRDS(census_clean, "census_ma_2023.rds")
glimpse(census_clean)

### Combine the two data sets through spatial joins ###

# Convert station headways to spatial object
stations_sf <- station_headways %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)

# Make sure census data is same CRS
census_clean <- st_transform(census_clean, 4326)

# Spatial join - find which tract each station falls in
stations_with_census <- st_join(stations_sf, census_clean, join = st_within)

# Check result
glimpse(stations_with_census)

# Check for any stations that didn't match a tract
stations_with_census %>% 
  filter(is.na(GEOID)) %>%
  select(stop_name, trunk_route_id)

saveRDS(stations_with_census, "stations_final.rds")

############ Data Loading ################
##########################################

stations_final <- readRDS("stations_final.rds")
census_clean   <- readRDS("census_ma_2023.rds")

############ Map Building ################
##########################################

# Extract coordinates from geometry for leaflet
stations_map <- stations_final %>%
  mutate(
    lon = st_coordinates(geometry)[,1],
    lat = st_coordinates(geometry)[,2]
  )


# Get some more statistics
stations_map <- stations_map %>%
  left_join(extreme_station_stats,
            by = c("parent_station", "trunk_route_id"))

# Fix the gaps for the symbology sizing
stations_map <- stations_map %>%
  mutate(
    scaled_gap = scales::rescale(
      headway_gap_min,
      to = c(4, 14)  # min and max radius
    )
  )

# Newer color palette
p90_pal <- colorNumeric(
  palette = "RdYlGn",
  domain = stations_map$p90_gap,
  reverse = TRUE
)

# Metro line colors
t_lines <- t_lines %>%
  mutate(
    line_color = case_when(
      route_id == "Red" ~ "#DA291C",
      route_id == "Orange" ~ "#ED8B00",
      route_id == "Blue" ~ "#003DA5",
      grepl("Green", route_id) ~ "#00843D",
      TRUE ~ "gray50"
    )
  )

# Add transit vulnerability index
census_clean <- census_clean %>%
  mutate(
    income_inverse = max(median_income, na.rm = TRUE) - median_income,
    
    vulnerability_index =
      scales::rescale(pct_zero_vehicle) +
      scales::rescale(pct_poverty) +
      scales::rescale(income_inverse)
  )

vuln_pal <- colorNumeric(
  palette = "BuPu",
  domain = census_clean$vulnerability_index
)

# Filter census tracts to around the metro area
stations_sf <- st_as_sf(
  stations_map,
  coords = c("lon", "lat"),
  crs = 4326
)
stations_sf <- st_transform(stations_sf, 26986)
census_clean_proj <- st_transform(census_clean, 26986)

station_buffer <- st_buffer(
  stations_sf,
  dist = 3200
)

station_buffer_union <- st_union(station_buffer)

census_clean_filtered <- census_clean_proj %>%
  filter(
    st_intersects(geometry, station_buffer_union, sparse = FALSE)
  )

census_clean_filtered <- st_transform(
  census_clean_filtered,
  4326
)

# Transform to a uniform datum
stations_map <- st_transform(stations_map, 4326)
census_clean <- st_transform(census_clean, 4326)
t_lines <- st_transform(t_lines, 4326)

# Build map
map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Base Map") %>%
  
  # Census tract layer - zero vehicle households
  addPolygons(
    data        = census_clean_filtered,
    fillColor = ~vuln_pal(vulnerability_index),
    fillOpacity = 0.30,
    color       = "gray50",
    weight      = 1,
    opacity = 0.8,
    dashArray = NULL,
    group       = "Transit Vulnerability",
    label = ~paste0(tract_name),
    popup = ~paste0(
      "<b>", tract_name, "</b><br>",
      county, "<br>",
      "<hr style='margin:4px 0;'>",
      "<b>Transit Vulnerability</b><br>",
      "Zero-vehicle households: ",
      scales::percent(pct_zero_vehicle, accuracy = 1), "<br>",
      "Poverty rate: ",
      scales::percent(pct_poverty, accuracy = 1), "<br>",
      "Median income: ",
      scales::dollar(median_income)
    )
  ) %>%
  
  # Add routes
  addPolylines(
    data = t_lines,
    color = ~line_color,
    weight = 4,
    opacity = 0.8,
    group = "T Lines"
  ) %>%  
  
  # Station circles sized and colored by headway gap
  addCircleMarkers(
    data = stations_map,
    lng = ~lon,
    lat = ~lat,
    radius = 7,
    fillColor = ~p90_pal(p90_gap),
    fillOpacity = 0.9,
    color = "white",
    weight = 1.5,
    stroke = TRUE,
    popup = ~paste0(
      "<b>", stop_name, "</b><br>",
      "<hr style='margin:4px 0;'>",
      "<b>Scheduled wait:</b> ",
      round(avg_scheduled_headway_sec / 60, 1), " min<br>",
      "<b>Average actual wait:</b> ",
      round(avg_actual_headway_sec / 60, 1), " min<br>",
      "<b style='color:#b30000;'>Average extra wait:</b> ",
      round(headway_gap_min, 2), " min<br>",
      "<hr style='margin:4px 0;'>",
      "<b>High-end delay (90th percentile):</b> ",
      round(p90_gap, 2), " min<br>",
      "<i>Represents delays riders regularly risk encountering</i>"
    ),
    label = ~paste0(
      stop_name, ": +",
      round(headway_gap_min, 1),
      " min"
    ),
    group = "Reliability Gaps"
  ) %>% 
  
  # Layer controls
  # addLayersControl(
  #   overlayGroups = c(
  #     "T Lines",
  #     "Transit Vulnerability",
  #     "Reliability Gaps"
  #   ),
  #   options = layersControlOptions(collapsed = FALSE)
  # ) %>%  
  
  # addLegend(
  #   pal = p90_pal,
  #   values = stations_map$p90_gap,
  #   title = "Risk of Long Delays",
  #   labFormat = function(type, cuts, p) {
  #     c("Low", "", "", "", "High")
  #   },
  #   position = "bottomright"
  # ) %>% 
  
  # addLegend(
  #   pal = vuln_pal,
  #   values = census_clean$vulnerability_index[!is.na(census_clean$vulnerability_index)],
  #   title = "Neighborhood Vulnerability to Transit Disruption",
  #   labFormat = function(type, cuts, p) {
  #     c("Lower", "", "Moderate", "", "Higher")
  #   },
  #   position = "bottomleft",
  #   opacity = 0.7
  # ) %>%
  
  addControl(
    html = "
  <div style='
    background: white;
    padding: 16px;
    border-radius: 10px;
    box-shadow: 0 0 15px rgba(0,0,0,0.2);
    width: 300px;
    font-size: 14px;
    line-height: 1.5;
  '>

  <h3 style='margin-top:0;'>Boston MBTA Transit Vulnerability Map</h3>

  <b>Station Color</b><br>
  Green → Lower delay risk<br>
  Red → Higher delay risk<br><br>

  <b>Neighborhood Shading</b><br>
  Darker tracts indicate greater vulnerability to transit disruption<br><br>

  <b>How to Read</b><br>
  Stations with high delay risk inside darker neighborhoods suggest places where transit unreliability may carry greater social cost.<br><br>

  <b>Core Question</b><br>
  When transit data says service is functioning, whose lived experience is still missing?

  </div>
  ",
    position = "topright"
  ) %>% 
  
  setView(
    lng = -71.0589,
    lat = 42.3601,
    zoom = 11
  )

map

############ Data Analysis ################
##########################################

line_summary <- stations_map %>%
  st_drop_geometry() %>%
  group_by(trunk_route_id) %>%
  summarise(
    avg_gap = mean(headway_gap_min, na.rm = TRUE),
    median_gap = median(headway_gap_min, na.rm = TRUE),
    max_gap = max(headway_gap_min, na.rm = TRUE),
    n_stations = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_gap))
line_summary

# Visualization of above
ggplot(line_summary, aes(
  x = reorder(trunk_route_id, avg_gap),
  y = avg_gap
)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Average Headway Gap by MBTA Line",
    x = "Line",
    y = "Average Extra Wait (minutes)"
  ) +
  theme_minimal()

# Exporting files for additional analysis
write.csv(
  sf::st_drop_geometry(stations_map),
  "stations_map.csv",
  row.names = FALSE
)

write.csv(
  line_summary,
  "line_summary.csv",
  "line_summary.csv",
  row.names = FALSE
)

write.csv(
  sf::st_drop_geometry(census_clean_filtered),
  "census_clean_filtered.csv",
  row.names = FALSE
)

### SAVE MAP AS HTML FOR UPLOADING


saveWidget(
  map,
  file = "index.html",
  selfcontained = TRUE
)