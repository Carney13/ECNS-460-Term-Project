#Delete this
setwd("C:/Users/ethan/OneDrive/Documents/GitHub/ECNS-460-Term-Project")

#Libraries
library(sf)
library(dplyr)
library(ggplot2)
library(spdep)
library(sp)
library(rgdal)
library(spatialreg)
library(glmnet)
library(ape)
library(MASS)
library(tmap) 
library(leaflet)
library(RColorBrewer)
library(raster)
library(tidymodels)
#Load data
load("Cleaned Data/property_data.RData")

zones <- read_sf("Raw Data/ZONE_2021_Parcels/ZONE_2021_Parcels.shp")
assets <- read_sf("Raw Data/Neighborhood_Assets/Neighborhood Assets.shp")
build_types <- read.csv("Raw Data/Zones_Building_Types.csv")

# --
#More Processing

#Clean asset (facility data), remove duplicated IDs and NAs
assets <- st_transform(assets, crs = 4326)
assets_clean <- assets %>%
  filter(!is.na(Parcel_ID)) |>
  filter(!duplicated(assets_clean$Parcel_ID))

#calculate distance (in meters) from all property locations to facility locations
dist_matrix <- st_distance(properties, assets_clean)   

# create data matrix of distances, make column names the id of the facilities,
#add property id's
distance_df <- as.data.frame(dist_matrix)
colnames(distance_df) <- assets_clean$Parcel_ID
distance_df$property_id <- properties$id

#pivot to long format to find the distance of each property from each facility
distance_long_df <- distance_df %>%
  pivot_longer(cols = -property_id, names_to = "facility_id", values_to = "distance_m") %>%
  mutate(facility_type = assets$Type[match(facility_id, assets$Parcel_ID)]) %>%
  select(property_id, facility_id, facility_type, distance_m)

#find the minimum distance for each property to each type of facility
min_distance_per_type <- distance_long_df %>%
  group_by(property_id, facility_type) %>%
  summarise(min_distance_m = min(distance_m, na.rm = TRUE)) %>%
  ungroup()

#Want one column per facility type, so pivot wide, values are minimum distance to
#that type of facility
min_distance_wide <- min_distance_per_type %>%
  pivot_wider(names_from = facility_type, values_from = min_distance_m, values_fill = list(min_distance_m = NA))

#rename id variable to be consistent with property data set
min_distance_wide$id = min_distance_wide$property_id

#merge property and facility distance data
data_clean <- properties %>%
  left_join(min_distance_wide, by = "id")

#merge permitted zone building data with property (and zone and facility) data
data_clean <- left_join(data_clean, build_types, by = c("Zone.Classification" = "Zone"))# --

#only want non-NA sale amount values for this analysis
data_clean <- data_clean[!is.na(data_clean$Sale.Amount), ]


# SPATIAL AUTOCORRELATION TEST
coordinates <- st_coordinates(properties_clean$Location)
#properties_clean$longitude <- coordinates[, 1]
#properties_clean$latitude <- coordinates[, 2]

neighbors_clean <- dnearneigh(coordinates, 0, 0.01)
weights_clean <- nb2listw(neighbors_clean)
listw <- nb2listw(neighbors_clean, style = "W")

moran.test(log(properties_clean$Sale.Amount), weights_clean)

lisa <- localmoran(log(properties_clean$Sale.Amount), listw)

# TIME SERIES ANALYSIS

time_trends <- data_clean |>
  group_by(List.Year, Zone.Classification) |>
  summarize(avg_price = mean(log(Sale.Amount)))

overall_avg_price <- data_clean |>
  group_by(List.Year) |>
  summarize(overall_avg = mean(log(Sale.Amount)))

ggplot(time_trends) +
  geom_line(aes(x = List.Year, y = avg_price, color = Zone.Classification)) +
  labs(title = "Average Housing Price by Zoning Class Over Time", x = "Year", y = "Average Price (log scale)") +
  geom_line(data = overall_avg_price, aes(x = List.Year, y = overall_avg), color = "black", size = 1) 

# KERNAL DENSITY ESTIMATION
coordinates <- st_coordinates(data_clean$Location)
#properties_clean$longitude <- coordinates[, 1]
#properties_clean$latitude <- coordinates[, 2]
kde <- kde2d(coordinates[, 1], coordinates[, 2], n = 100)
kde_df <- data.frame(expand.grid(x = kde$x, y = kde$y), z = as.vector(kde$z))
ggplot() +
  geom_tile(data = kde_df, aes(x = x, y = y, fill = z)) +
  scale_fill_viridis_c() +
  labs(title = "Housing Price Density", fill = "Density") +
  theme_minimal()

kde_raster <- raster::rasterFromXYZ(cbind(expand.grid(x = kde$x, y = kde$y), z = as.vector(kde$z)))
crs(kde_raster) <- CRS("+init=epsg:4326")
m <- leaflet() |>
  addTiles() |>
  setView(lat = 41.1865, lng = -73.1952, zoom = 12) |>
  addRasterImage(
    kde_raster, 
    colors = my_palette,  # Use the KDE palette
    opacity = 0.7,        # Adjust opacity to make the map visible underneath
    project = TRUE) |> 
  addPolygons(
    data = properties,  # Add the zoning polygons
    color = "black",           # Set the border color
    weight = 0.5,                # Set border width
    fillColor = "transparent", # Set fill color to transparent
    fillOpacity = 1)   |>
  addCircleMarkers(
    data = assets_clean,
    color = "red",          # Color of the points
    radius = 10*10^10000000,            # Size of the points
    fillOpacity = 1,
    stroke = TRUE,         # Add outline to the circles
    weight = 2,            # Thickness of the outline
    opacity = 5) |>
  addLegend(
    pal = my_palette, 
    values = kde_raster[], 
    title = "Density", 
    opacity = 0.7
  )

# Display the map
m

# MACHINE LEARNING TO ESTIMATE SIGNIFICANCE OF ZONING CLASSIFICATIONS
