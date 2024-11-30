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
#Load data
load("Cleaned Data/property_data.RData")


ggplot() +
  geom_sf(data = properties, fill = "lightgrey", color = "black") +
  geom_sf(data = properties, aes(color = Sale.Amount), size = 1) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "House Prices Across Zones", color = "Sale Amount")

#Spatial Autocorrelaiton test
coords <- properties$Location  # Get coordinates for each point
neighbors <- dnearneigh(coords, 0, 1000)  # Neighbors within 1000 meters

# Create spatial weight matrix
weights <- nb2listw(neighbors)

# Calculate Moran's I
moran.test(properties$Sale.Amount, weights)

# --

properties_clean <- properties[!is.na(properties$Sale.Amount), ]

# Ensure that the weights matrix is consistent with the cleaned data
coords_clean <- properties_clean$Location  # Get coordinates for the cleaned data
neighbors_clean <- dnearneigh(coords_clean, 0, 1000)  # Create neighbors with cleaned data

# Create the new spatial weights matrix
weights_clean <- nb2listw(neighbors_clean)

# Run Moran's I on cleaned data
moran.test(properties_clean$Sale.Amount, weights_clean)

# --
sem_model <- errorsarlm(Sale.Amount ~ tmax + factor(Zone.Name), data = data_sf, listw = weights)

sar_model <- lm(Sale.Amount ~ Sales.Ratio + factor(Zone.Classification), data = properties_clean)
# Get summary
summary(sar_model)

# -----
dist_matrix <- distVincentySphere(cords_clean)
listw <- nb2listw(neighbors_clean, style = "W")
listw_sparse <- as(listw, "CsparseMatrix")
# Fit Spatial Lag Model (SAR)
sar_model_parallel <- lagsarlm(Sale.Amount ~ tmax + factor(Zone.Name), data = properties, listw = listw, parallel = TRUE)
sar_model <- lagsarlm(Sale.Amount ~ Sales.Ratio + factor(Zone.Classification), data = properties_clean, listw = listw)
summary(sar_model)
# ---
properties_subset <- properties_clean[sample(1:nrow(properties_clean), 1000), ]
neighbors_clean <- dnearneigh(properties_subset$Location, 0, 100)  # Create neighbors with cleaned data

# Create the new spatial weights matrix
weights_clean <- nb2listw(neighbors_clean)

listw <- nb2listw(neighbors_clean, style = "W")

# Run the SAR model on the smaller dataset
sar_model_subset <- lagsarlm(log(Sale.Amount) ~ Sales.Ratio + factor(Zone.Classification), data = properties_subset, listw = listw)

moran.test(log(properties_subset$Sale.Amount), weights_clean)

# ---
coordinates <- st_coordinates(properties_clean$Location)
properties_clean$longitude <- coordinates[, 1]
properties_clean$latitude <- coordinates[, 2]

properties_dists <- as.matrix(dist(cbind(properties_subset$longitude,
                                         properties_subset$latitude)))
neighbors_clean <- dnearneigh(coordinates, 0, 0.01)
weights_clean <- nb2listw(neighbors_clean)
listw <- nb2listw(neighbors_clean, style = "W")
moran.test(log(properties_clean$Sale.Amount), weights_clean)

sar_model_subset <- lagsarlm(log(Sale.Amount) ~ Sales.Ratio + factor(Zone.Classification), data = properties_subset, listw = listw)


properties_dists_inv <- 1/properties_dists
diag(properties_dists_inv) <- 0

properties_dists_inv[1:5, 1:5]

Moran.I(properties_subset$Sale.Amount, properties_dists_inv)
