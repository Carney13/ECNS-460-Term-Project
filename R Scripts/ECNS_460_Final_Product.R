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
library(randomForest)
library(parralel)
library(furrr)
#Load data
load("Cleaned Data/property_data.RData")

zones <- read_sf("Raw Data/ZONE_2021_Parcels/ZONE_2021_Parcels.shp")
assets <- read_sf("Raw Data/Neighborhood_Assets/Neighborhood Assets.shp")
build_types <- read.csv("Raw Data/Zones_Building_Types.csv")

# -----------------------------------------
#More Processing

#Clean asset (facility data), remove duplicated IDs and NAs
assets <- st_transform(assets, crs = 4326)
assets_clean <- assets %>%
  filter(!is.na(Parcel_ID)) %>%
  filter(!duplicated(Parcel_ID))

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
  mutate(facility_type = assets_clean$Type[match(facility_id, assets_clean$Parcel_ID)]) %>%
  dplyr::select(property_id, facility_id, facility_type, distance_m)

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

#create factors for these indicator variables
data_clean <- data_clean |>
  mutate(across(c("Storefront_Building", "Commercial_Center", "Commercial_House",
                  "General_Building", "Small_General_Building", "Row_Building", "Double_House_A", 
                  "House_A", "House_B", "House_C", "House_D", "Workshop", "Civic_Building", 
                  "Patio_Outdoor_Site", "Open_Outdoor_Site"), as.factor))

data_clean <- data_clean %>%
  mutate_at(vars("Storefront_Building", "Commercial_Center", "Commercial_House",
                 "General_Building", "Small_General_Building", "Row_Building", "Double_House_A", 
                 "House_A", "House_B", "House_C", "House_D", "Workshop", "Civic_Building", 
                 "Patio_Outdoor_Site", "Open_Outdoor_Site"), ~replace(., is.na(.), 0))

data_clean <- data_clean |>
  dplyr::select(-property_id)

save(data_clean, file = file.path("Cleaned Data", "final_data.RData"))
# ------------------------------------------

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

# Zone Case --------------------------------------------- 
#Test case where Zone data is included
set.seed(1013)


data_ml <- as.data.frame(data_clean)
data_ml$Sale.Amount = log(data_ml$Sale.Amount)
# Now, try to select the columns
data_ml <- data_ml %>%
  dplyr::select(-geometry, -Location)

data_split = data_ml |> initial_split(prop = 0.8)
# Grab each subset
data_train = data_split |> training()
data_test  = data_split |> testing()

house_recipe = recipe(Sale.Amount ~ ., data = data_train) |>
  # Set aside ID variable so we don't use it as a predictor
  update_role(id, new_role = "id") |>
  update_role(Address, new_role = "none") |>
  update_role(Property.Type, new_role = "none") |>
  update_role(Zone.Name, new_role = "none") |>
  update_role(Non.Use.Code, new_role = "none") |>
  update_role(Serial.Number, new_role = "none")|>
  step_log(Assessed.Value) |>
  #create dummy variables from building types
  step_dummy(all_nominal_predictors()) |>
  # Remove predictors with ~0 variance
  step_nzv(all_predictors())


data_clean = house_recipe |> prep() |> juice()

model_tree <- rand_forest(mode = "regression", mtry = tune(), min_n = tune()) |> 
  set_engine("ranger", importance = "impurity")

resamples <- vfold_cv(data_train, v = 3)

workflow_tree = workflow() |>
  add_model(model_tree) |>
  add_recipe(house_recipe)

plan(multisession, workers = parallel::detectCores() - 1)  # Use all cores except one

# Parallelize the tuning process
workflow_tree_cv <- workflow_tree |>
  tune_grid(resamples = resamples, control = control_grid(parallel_over = "everything"))

# Don't forget to reset the parallel plan to the default at the end
plan(sequential)

best_params <- workflow_tree_cv |> show_best(metric = "rmse")

final_workflow <- workflow_tree |>
  finalize_workflow(select_best(workflow_tree_cv))

# Step 6: Fit the final model using the best hyperparameters
fit_tree <- final_workflow |> fit(data_train)

# -- Check variable importance
rf_model <- fit_tree$fit$fit$fit

# Check the variable importance (using ranger's importance function)
rf_importance <- rf_model$variable.importance

importance_df <- data.frame(
  Variable = names(rf_importance),
  Importance = as.numeric(rf_importance)
)

importance_df <- importance_df[order(-importance_df$Importance), ]

ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +  # This flips the axes so the variables are listed vertically
  labs(title = "Variable Importance",
       x = "Variables",
       y = "Importance") +
  theme_minimal()

importance_df_rev <- importance_df[importance_df$Variable != "Assessed.Value", ]

ggplot(importance_df_rev, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +  # This flips the axes so the variables are listed vertically
  labs(title = "Variable Importance",
       x = "Variables",
       y = "Importance") +
  theme_minimal()
# ---

train_predictions <- fit_tree |> predict(data_train)

train_predictions <- fit_tree |> predict(new_data = data_train)
train_results <- bind_cols(data_train, train_predictions)

train_results$pred_exp <- exp(train_results$.pred)
train_results$truth_exp <- exp(train_results$Sale.Amount)

train_rmse_dollars <- rmse(train_results, truth = truth_exp, estimate = pred_exp)

# Print RMSE in dollars for the training data
train_rmse_dollars

test_predictions <- fit_tree |> predict(new_data = data_test)

test_results <- bind_cols(data_test, test_predictions)

test_results$pred_exp <- exp(test_results$.pred)
test_results$truth_exp <- exp(test_results$Sale.Amount)

test_rmse_dollars <- rmse(test_results, truth = truth_exp, estimate = pred_exp)

# Print RMSE in dollars for the training data
test_rmse_dollars

# No Zones Case ------------------------------------------

#Test case where NO Zone data is included

data_ml2 <- data_ml |>
  dplyr::select(-Zone.Name, -Zone.Classification, -Storefront_Building, 
                -Commercial_Center, -Commercial_House, -General_Building, 
                -Small_General_Building, -Row_Building, -Double_House_A, -House_A, 
                -House_B, -House_C, -House_D, -Workshop, -Civic_Building, -Patio_Outdoor_Site, 
               -Open_Outdoor_Site)
  
data_split2 = data_ml2 |> initial_split(prop = 0.8)
# Grab each subset
data_train2 = data_split2 |> training()
data_test2  = data_split2 |> testing()

house_recipe2 = recipe(Sale.Amount ~ ., data = data_train2) |>
  # Set aside ID variable so we don't use it as a predictor
  update_role(id, new_role = "id") |>
  update_role(Address, new_role = "none") |>
  update_role(Property.Type, new_role = "none") |>
  update_role(Non.Use.Code, new_role = "none") |>
  update_role(Serial.Number, new_role = "none")|>
  step_log(Assessed.Value) |>
  #create dummy variables from building types
  step_dummy(all_nominal_predictors()) |>
  # Remove predictors with ~0 variance
  step_nzv(all_predictors())


data_clean2 = house_recipe2 |> prep() |> juice()

model_tree2 <- rand_forest(mode = "regression", mtry = tune(), min_n = tune()) |> 
  set_engine("ranger",  importance = "impurity")

resamples2 <- vfold_cv(data_train, v = 3)

workflow_tree2 = workflow() |>
  add_model(model_tree2) |>
  add_recipe(house_recipe2)

plan(multisession, workers = parallel::detectCores() - 1)  # Use all cores except one

# Parallelize the tuning process
workflow_tree_cv2 <- workflow_tree2 |>
  tune_grid(resamples = resamples2, control = control_grid(parallel_over = "everything"))

# Don't forget to reset the parallel plan to the default at the end
plan(sequential)

best_params <- workflow_tree_cv2 |> show_best(metric = "rmse")

final_workflow2 <- workflow_tree2 |>
  finalize_workflow(select_best(workflow_tree_cv2))

# Step 6: Fit the final model using the best hyperparameters
fit_tree2 <- final_workflow2 |> fit(data_train2)

# -- Check variable importance
rf_model2 <- fit_tree2$fit$fit$fit

# Check the variable importance (using ranger's importance function)
rf_importance2 <- rf_model2$variable.importance

importance_df2 <- data.frame(
  Variable = names(rf_importance2),
  Importance = as.numeric(rf_importance2)
)

importance_df2 <- importance_df2[order(-importance_df2$Importance), ]

ggplot(importance_df2, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +  # This flips the axes so the variables are listed vertically
  labs(title = "Variable Importance",
       x = "Variables",
       y = "Importance") +
  theme_minimal()


importance_df2_rev <- importance_df2[importance_df2$Variable != "Assessed.Value", ]

ggplot(importance_df2_rev, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +  # This flips the axes so the variables are listed vertically
  labs(title = "Variable Importance",
       x = "Variables",
       y = "Importance") +
  theme_minimal()

# ---

train_predictions_nz <- fit_tree2 |> predict(new_data = data_train2)
train_results_nz <- bind_cols(data_train2, train_predictions_nz)

train_results_nz$pred_exp <- exp(train_results_nz$.pred)
train_results_nz$truth_exp <- exp(train_results_nz$Sale.Amount)

train_rmse_dollars_nz <- rmse(train_results_nz, truth = truth_exp, estimate = pred_exp)

# Print RMSE in dollars for the training data
train_rmse_dollars_nz

test_predictions_nz <- fit_tree2 |> predict(new_data = data_test2)

test_results_nz <- bind_cols(data_test2, test_predictions_nz)

test_results_nz$pred_exp <- exp(test_results_nz$.pred)
test_results_nz$truth_exp <- exp(test_results_nz$Sale.Amount)

test_rmse_dollars_nz <- rmse(test_results_nz, truth = truth_exp, estimate = pred_exp)

# Print RMSE in dollars for the training data
test_rmse_dollars_nz

# --------------
#save results
model_comparison_df <- data.frame(
  Model = rep(c("RF with zone data", "RF no zone data"), each = 2),
  Dataset = rep(c("Train", "Test"), times = 2),
  RMSE = c(train_rmse_dollars$.estimate, test_rmse_dollars$.estimate, 
           train_rmse_dollars_nz$.estimate, test_rmse_dollars_nz$.estimate)
)

write.csv(model_comparison_df, "Cleaned Data", row.names = FALSE)

