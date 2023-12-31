

# TRAFFIC ASSIGNMENT
# Glasgow example

# Date: 2023-10-16
# Author: Rafael Verduzco

library(sf)
library(dodgr)
library(tidyverse)
library(cppRouting)
library(mapview)

# References
# https://github.com/vlarmet/cppRouting
# https://cran.r-project.org/web/packages/dodgr/vignettes/flows.html


# Read data ---------------------------------------------------------------


# Travel to work areas
ttwa <- st_read('data/Travel_to_Work_Areas_Dec_2011/TTWA_2011_UK_BFE_V3.shp')
# Glasgow
glasgow <- ttwa %>% 
  filter(grepl('Glasgow', TTWA11NM))
# OD flows
flows <- read_csv('data/driving_flows_oa/driving_flows_oa.csv')
# Centroids
centroids <- st_read('data/centroids/gb_oa_pwc2011.gpkg')

# Read road network
# Source: https://download.geofabrik.de/europe/great-britain/scotland.html
roads <- osmextract::oe_read('data/osm/scotland-latest.osm.pbf')

# Define orign and destination points -------------------------------------

# Subset centroids in Glasgow
centroids <- centroids %>% 
  filter(st_intersects(., glasgow, sparse = FALSE)) %>% 
  st_transform(4326)
# Subset flows in glasgow
flows <- flows %>% 
  filter(origin %in% centroids$geo_code & destination %in% centroids$geo_code )


# OSM network to weighted street network ----------------------------------


# PBF to network
graph <- dodgr::weight_streetnet(roads, wt_profile = "motorcar", left_side = TRUE)

# Remove weight/time is NA
graph <- graph[!is.na(graph$time_weighted),]

# Make cppRouting graph
sgr <- cppRouting::makegraph(
  df = graph[,c("from_id", "to_id", "time_weighted")], 
  directed = TRUE
) 


# Format flows ------------------------------------------------------------

# This is needed to represent the OD with node's ID codes

# Node to SF
nodes <- data.frame(
  from_id = graph$from_id,
  from_lon = graph$from_lon,
  from_lat = graph$from_lat
)
nodes <- nodes %>% 
  st_as_sf(coords = c('from_lon', 'from_lat'), crs = 4326)
# Filter nodes within Glasgow
nodes <- nodes %>% 
  st_transform(st_crs(glasgow)) %>% 
  filter(st_intersects(., glasgow, sparse = FALSE))

# Transform centroids' CRS
centroids <- st_transform(centroids, st_crs(glasgow))

# Find nearest node associated to centroid
nearest_vertex <- st_nearest_feature(centroids, nodes)
# Set from ID
centroids$from_id <- nodes$from_id[nearest_vertex]

# lookup
lookup <- centroids %>% 
  st_drop_geometry() %>% 
  select(geo_code, from_id)

# Join from_id to flows
flows <- flows %>% 
  left_join(lookup, by = c('origin' = 'geo_code')) %>% 
  rename(origin_id = from_id) %>% 
  left_join(lookup, by = c('destination' = 'geo_code')) %>% 
  rename(destination_id = from_id)

# Reaggregate flows using node code
flows_node <- flows %>% 
  group_by(origin_id, destination_id) %>% 
  summarise(car_driver =  sum(car_driver))

# All-or-Nothing assignment (AON)  ----------------------------------------


# Compute AON assignment using OD trips
aon <- cppRouting::get_aon(
  Graph = sgr, 
  from = flows_node$origin_id, 
  to = flows_node$destination_id, 
  demand = flows_node$car_driver
)


# Process assignment in dodgr ---------------------------------------------


# Join AON result to dodgr graph
graph_aon <- graph %>% 
  left_join(aon, by = c('from_id' = 'from', 'to_id' = 'to'))

# Merging directed flows
graph_undir <- merge_directed_graph(graph_aon)
# Keep flows with at leas one flow
graph_undir <- graph_undir[graph_undir$flow > 0,]

# Transform to SF
graph_undir_sf <- dodgr_to_sf(graph_undir)


# Plot flows  -------------------------------------------------------------

# Output
dir.create('output')

# Static plot
static_plot <- graph_undir_sf %>% 
  arrange(flow) %>% 
  ggplot() +
  geom_sf(aes(linewidth = flow, col = flow)) +
  scale_linewidth_continuous(range = c(0.01, 2)) +
  scale_color_viridis_c(option = 'plasma') +
  theme_void()
# Save map
ggsave('output/glasgow_assignment.jpg', static_plot, height = 6, width = 9, dpi = 400)

# Mapview
mapview(graph_undir_sf, zcol = 'flow', lwd = 'flow')

# Save SF
st_write(graph_undir_sf, 'output/glasgow_assignment.gpkg')
