#Data processing and cleaning

library(sp)
library(sf)
library(st)
library(leaflet)
library(RColorBrewer)
library(viridis)
library(dplyr, warn.conflicts = FALSE)
library(tidygeocoder)
library(furrr)
library(ggmap)
library(tidyr)
library(purrr)
library(stringr)

data <- read.csv("Real_Estate_Sales_2001-2022_GL.csv")


most_frequent <- data |>
  count(Town) |>        
  arrange(desc(n))    

#Use Bridgetown as city for analysis, as it has the most observations

data_br <- data |>
  filter(Town == "Bridgeport")

#unzip("ZONE_2021_Parcels.zip", junkpaths = FALSE)

zones <- read_sf("ZONE_2021_Parcels.shp")

par(mar = c(0, 0, 0, 0))
plot(st_geometry(zones), col = "#f2f2f2", bg = "skyblue", lwd = 0.25, border = 0)

#plot zones in Bridgeport using Geo-spatial locations

zones <- st_zm(zones, drop = TRUE, what = "ZM")
zones <- st_transform(zones, crs = 4326)

n <- length(unique(zones$ZONE_2021))  

my_palette <- viridis(n)

m <- leaflet(zones) |>
  addTiles() |>
  setView(lat = 41.1865, lng = -73.1952, zoom = 12) |>
  addPolygons(
    fillColor = ~ my_palette,  
    stroke = FALSE,
    fillOpacity = 0.7
  ) 


m

# -------------------------
#Get geo-spatial locations for properties that don't have them.
town <- "BRIDGEPORT"
state <- "CT"

#Add city and State to address so google can access the correct locations
data_br <- data_br %>%
  mutate(Full_Address = paste(Address, town, state, sep = ", "))

#split data into two data frames: one with properties that already have a location
#and one with properties that don't
data_br1 <- data_br |>
  filter(Location == "")

data_br2 <- data_br |>
  filter(Location != "")


# ---------
#Use parralelization since there are many observations
key = read.table("Google Geo-Spatial API.txt", stringsAsFactors = FALSE)
register_google(key = key)
plan(multisession, workers = 8)

# Define a function for geo-coding with error handling (since Google can't find
#every address value if it was entered incorrectly, or has multiple addresses found)
#observations may have not specified an apartment or unit number, which would
#lead to a non-specific address.
geocode_address <- safely(function(address) {
  result <- geocode(address, method = 'google')  # or method = 'osm'
  if (nrow(result) == 1) {
    return(result)  # Return the result if there's only one
  } else {
    return(data.frame(lat = NA, lon = NA))  # Return NA for non-unique results
  }
})

# Use future_map to parallelize the geocoding
geocode_results <- data_br1 %>%
  mutate(geocode_results = future_map(Full_Address, ~ geocode_address(.x)))


# Extract successful results and handle errors
lat_longs <- geocode_results %>%
  # Extract the result from safely
  mutate(geocode_results = map(geocode_results, "result")) %>%
  unnest(geocode_results) %>%
  filter(!is.na(lat) & !is.na(lon)) %>%  # Ensure both lat and lon are not NA
  rename(latitude = lat, longitude = lon)


# ---------

data_br2 <- data_br2 %>%
  mutate(
    coords = stringr::str_remove(Location, "POINT \\("),  
    coords = stringr::str_remove(coords, "\\)"),            
    longitude = as.numeric(str_split(coords, " ", simplify = TRUE)[, 1]),  
    latitude = as.numeric(str_split(coords, " ", simplify = TRUE)[, 2])    
  ) %>%
  select(-coords)

data_cl <- bind_rows(data_br2, lat_longs)

# --------

data_points_sf <- st_as_sf(data_cl, coords = c("longitude", "latitude"), crs = 4326)

m <- leaflet(zones) |>
  addTiles() |>
  setView(lat = 41.1865, lng = -73.1952, zoom = 12) |>
  addPolygons(
    fillColor = ~ my_palette,  
    stroke = FALSE,
    fillOpacity = 0.7
  ) |>
  addCircleMarkers(
    data = data_points_sf,
    color = "black",          # Color of the points
    radius = 10*10^10000000,            # Size of the points
    fillOpacity = 1,
    stroke = TRUE,         # Add outline to the circles
    weight = 2,            # Thickness of the outline
    opacity = 5
  )
m

# --------

houses_with_zones <- st_join(data_points_sf, zones, join = st_within)

properties <- houses_with_zones %>%
  filter(!is.na(ZONE_2021))

m <- leaflet(zones) |>
  addTiles() |>
  setView(lat = 41.1865, lng = -73.1952, zoom = 12) |>
  addPolygons(
    fillColor = ~ my_palette,  
    stroke = FALSE,
    fillOpacity = 0.7
  ) |>
  addCircleMarkers(
    data = properties,
    color = "black",          # Color of the points
    radius = 10*10^10000000,            # Size of the points
    fillOpacity = 1,
    stroke = TRUE,         # Add outline to the circles
    weight = 2,            # Thickness of the outline
    opacity = 5
  )
m

#Clean merged data frame; Data Cleaning Checklist

#1. 
#Data is already imported

#2. 
#Data is in tidy format, with each observation representing a property sale
#in Bridgetown, CT between 2001 and 2022

#3.

#Remove Assessor.Remarks and OPM.remarks as these variables provide comments on the
#properties that are not relevant to this analysis. Remove Full_Address as it was 
#a temporary variable that was used to assign a Geo-spatial location to the properties
#in a previous step. Remove Location variable as the true location is now stored
#in the geometry variable. Remove Residential.Type variable as it is redundant. It 
#indicates if a property is residential or commercial, but a property is commercial
#if it doesn't have a value in the Property.Type variable, and is residential if it
#does (by inspection). Don't need the Town variable, as all observations are from
#Bridgeport.

properties <- subset(properties, select = -c(Assessor.Remarks, OPM.remarks, Full_Address,
                                             Location, Residential.Type, Town))

#4.
#There is currently no primary key in the data set. Create one based on the property's
#serial number and date recorded for the sale
properties <- properties |>
  arrange(Serial.Number, Date.Recorded) |>
  mutate(id = row_number()) |>
  relocate(id)

#5.
#No duplicates, each row represents a specific sale of a property

#6.
#Variables:

#id: primary key created for data

#Serial.Number: number used to identify a property, assigned by the Municiple Corportaiton
#office

#List.Year: Year the property was listed on the market

#Date.Recorded: The date the sale was recorded (locally)

#Town: Town where property is located

#Address: Street address of property

#Assesed.Value: Value of the property from local tax assesment

#Sale.Amount: Amount of money the property sold for

#Sales.Ratio: Ratio of the property sales price to its assesed value

#Property.Type: Type of property. Classified as Residential, Commercial, Industrial,
#Apartments, Vacant, etc.

#Residential.Type: Categorical variable that indicates whether property is single 
#or multifamily residential

#Non.Use.Code: Non usable sales code that indicates the sale price is not reliable
#for use in the determination of the property value (additional documentation on these
#codes linked on GitHub repository).


#Name: Name of zone property is within

#ZONE_2021: Zone classification of the zone the property is in

#geometry: Geo-spatial representation of property's location

#7.
#Rename variables to match common naming structure
properties <- properties |>
  rename(Location = geometry, Zone.Name = Name, Zone.Classification = ZONE_2021)

#8.
#Convert sale dates from character to date object
properties$Date.Recorded <- as.Date(properties$Date.Recorded, format = "%m/%d/%Y")

#Convert categorical variables to factors
properties$Property.Type <- as.factor(properties$Property.Type)

levels(properties$Property.Type)

#levels are consistent for property types

properties$Zone.Classification <- as.factor(properties$Zone.Classification)

levels(properties$Zone.Classification)

#levels are consistent for Zones

#some observations in the Non.Use.Code variable have a short description of the code type 
#after the code, which is redundant since the code is all that is needed
#(for example, "14 - Foreclosure"). Remove the 
#descriptions and keep only the codes by keeping the first two characters in the string:

properties$Non.Use.Code <- str_sub(properties$Non.Use.Code, 1, 2)

#Some in Non.Use.Code have values that are in the 1-9 range are preceded by a zero, such as 
#08 instead of 8. Thus, we removed the first digit if it was a 0:

properties$Non.Use.Code <- sub("^0", "", properties$Non.Use.Code)

#Finally, convert this to a factor:

properties$Non.Use.Code <- as.factor(properties$Non.Use.Code)

levels(properties$Non.Use.Code)

#Levels are consistent for this variable

#9.
#Property.Type, and Non.Use.Code both have missing values.
#Property.Type variable is only relevant for residential properties, so it is
#expected to have missing values for commercial properties. Non.Use.Code only has
#a value if there was an extraneous reason that the sales price was not accurate.

#Give missing values a 'NA' designation
properties$Property.Type <- replace(properties$Property.Type, properties$Property.Type == "", NA)
properties$Non.Use.Code <- replace(properties$Non.Use.Code, properties$Non.Use.Code == "", NA)

#10.
#Assessed.Value and Sale.Amount variables have consistent units ($s); other variables
#have unique factors or values based on what they represent, so all units are 
#consistent

#11.
range(properties$Assessed.Value)

range(properties$Sale.Amount)

#both the Assessed.Value and Sale.Amount variables have values of 0 for certain
#properties. It is likely that the preasense of a 0 for the Assessed.Value variable
#means that no assessment was performed (since many observations with a 0 in this
#variable have a non-zero value in the Sale.Amount variable). The same reasoning
#can be applied to the Sale.Amount variable since a property would not sell for 
#0$ unless it was transferred in ownership and not sold. Thus, it makes more 
#sense to use NA's instead of zeros for these values.

properties$Assessed.Value <- replace(properties$Assessed.Value, properties$Assessed.Value == 0, NA)
properties$Sale.Amount <- replace(properties$Sale.Amount, properties$Sale.Amount == 0, NA)

#12.
#Address variable uses consistent naming conventions, street abbreviations, Apt. or
#Unit number, etc. These addresses are valid as they were assigned a geo-spatial location 
#using the Google's API, and were filtered to make sure the address was within a zone
#in Bridgeport. Thus, the addresses are consistent with google's naming conventions. The Zone.Name
#scheme was devised by the City of Bridgeport. The Zone.Classifications were as well. Categorical
#variables and their levels were checked in part 8. Strings are clean.


#13.
write.csv(properties, file = "CT_properties.csv")
save(properties, file = "property_data.RData")
save(zones, file = "zones_data.RData")

# --------





