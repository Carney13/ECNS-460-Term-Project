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
#Load data
load("Cleaned Data/property_data.RData")
zones <- read_sf("Raw Data/ZONE_2021_Parcels/ZONE_2021_Parcels.shp")
# --

#V
ggplot() +
  geom_sf(data = properties, fill = "lightgrey", color = "black") +
  geom_sf(data = properties, aes(color = Sale.Amount), size = 1) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "House Prices Across Zones", color = "Sale Amount")


# --

properties_clean <- properties[!is.na(properties$Sale.Amount), ]


# SPATIAL AUTOCORRELATION TEST
coordinates <- st_coordinates(properties_clean$Location)
#properties_clean$longitude <- coordinates[, 1]
#properties_clean$latitude <- coordinates[, 2]

neighbors_clean <- dnearneigh(coordinates, 0, 0.01)
weights_clean <- nb2listw(neighbors_clean)
listw <- nb2listw(neighbors_clean, style = "W")

moran.test(log(properties_clean$Sale.Amount), weights_clean)

lisa <- localmoran(log(properties_clean$Sale.Amount), listw)

# SPATIAL LAG MODEL
sar_model <- lagsarlm(log(Sale.Amount) ~ log(Sales.Ratio) + factor(Zone.Classification) + 
                        List.Year + log(Assessed.Value), data = properties_clean, listw = listw)

# TIME SERIES ANALYSIS

time_trends <- properties_clean |>
  group_by(List.Year, Zone.Classification) |>
  summarize(avg_price = mean(log(Sale.Amount)))

overall_avg_price <- properties_clean |>
  group_by(List.Year) |>
  summarize(overall_avg = mean(log(Sale.Amount)))

ggplot(time_trends) +
  geom_line(aes(x = List.Year, y = avg_price, color = Zone.Classification)) +
  labs(title = "Average Housing Price by Zoning Class Over Time", x = "Year", y = "Average Price (log scale)") +
  geom_line(data = overall_avg_price, aes(x = List.Year, y = overall_avg), color = "black", size = 1) 

# KERNAL DENSITY ESTIMATION
kde <- kde2d(properties_clean$longitude, properties_clean$latitude, n = 100)
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
  addLegend(
    pal = my_palette, 
    values = kde_raster[], 
    title = "Density", 
    opacity = 0.7
  )

# Display the map
m



# SPATIAL HOTSPOTS
gi_stats <- localG(properties_clean$Sale.Amount, listw)
