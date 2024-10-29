# Visualization of Data
library(ggspatial)
library(prettymapr)
library(scales)
library(ggplot2)
library(ggspatial)

load("property_data.RData")


temp <- properties

# Count number of observations for each Zone Classification
counts <- properties |>
  group_by(Zone.Classification) |>  
  summarise(count = n()) |>
  arrange(desc(count))

# Reorder the Zone Classification in descending order
temp$Zone.Classification <- factor(temp$Zone.Classification, levels = counts$Zone.Classification)


(p1 <- ggplot(temp, aes(x = Zone.Classification, y = ..count.., fill = Zone.Classification)) + 
  geom_bar(stat = "count") +
  labs(title = "Property Sales by Zone Type",
       x = "Zone Type",
       y = "Number of Properties Sold") +
  theme_minimal() +
  theme(legend.position = "none"))

# --------

(p2 <- ggplot(data, aes(x = log(Sale.Amount))) +
  geom_histogram(binwidth = 0.2, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Logged Housing Sales in Bridgeport, CT",
       x = "Sale Price (log scale)",
       y = "Frequency") +
  theme_minimal())

# ---------

data <- properties |> filter(id != 17807 & id != 17809)

mean_prices <- data |>
  group_by(List.Year) |>
  summarize(
    Mean_Sale_Price = mean(Sale.Amount, na.rm = TRUE),
    Mean_Assessed_Price = mean(Assessed.Value, na.rm = TRUE)
  ) 
  
(p3 <- ggplot(mean_prices, aes(x = List.Year)) +
  geom_line(aes(y = Mean_Assessed_Price, color = "Appraised Value"), size = 1) +
  geom_line(aes(y = Mean_Sale_Price, color = "Sale Price"), size = 1) +
  scale_color_manual(values = c("Sale Price" = "green", "Appraised Value" = "blue")) +
  labs(title = "Housing Prices Over Time",
       x = "Date",
       y = "Price",
       color = "Price Type")+ 
  scale_y_continuous(labels = scales::number_format()) +
  theme_minimal())

# -----------

(p4 <- ggplot(data = properties) +
  geom_sf(aes(fill = log(Sale.Amount)), color = NA) +  
  scale_fill_viridis_c(option = "plasma", na.value = "transparent") +
  labs(title = "Heat Map of Housing Prices by Zone",
       fill = "Housing Price") +
  theme_minimal() +
  theme(legend.position = "right"))


# ----------

ggplot() +
  annotation_map_tile(zoom = 12) +  
  geom_sf(data = properties, aes(fill = log(Sale.Amount)), color = NA, alpha = 0.75, size = 0.001) +  # Fill by sales price
  scale_fill_viridis_c(option = "plasma", na.value = "transparent", 
                       labels = label_number(accuracy = 0.01)) +  
  labs(title = "Heat Map of Logged Housing Prices by Zone",
       fill = "Housing Price (log scale)") +
  theme_minimal() +  
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 16))

# ---------

subset_property_types <- c("Two Family", "Three Family", "Condo", "Apartments"
                          ,"Four Family" ) 

# Filter the properties data frame
filtered_properties <- properties %>%
  filter(Property.Type %in% subset_property_types)

mean_ratio <- mean(filtered_properties$Sales.Ratio, na.rm = TRUE)
sd_ratio <- sd(filtered_properties$Sales.Ratio, na.rm = TRUE)

# Define limits (mean ± standard deviation)
x_min <- mean_ratio - 2*sd_ratio
x_max <- mean_ratio + 2*sd_ratio

# Create the ridgeline plot centered around the mean

(p5 <- ggplot(filtered_properties, aes(x = Sales.Ratio, y = Property.Type, fill = Property.Type)) +
  geom_density_ridges(alpha = 0.7, scale = 2) +
  labs(title = "Sales Ratio Distribution by Property Type",
       x = "Sales Ratio",
       y = "Property Type") +
  theme_minimal() +
  scale_fill_viridis_d() +
  scale_x_continuous(limits = c(x_min, x_max),  # Center around mean
                     breaks = seq(x_min, x_max, by = 3)) )

# ---------
#Save the generated plots:
plots <- c(p1, p2, p3, p4, p5)

ggsave(
  filename = "Property Sales by Zone Type.pdf",
  plot = p1
)

ggsave(
  filename = "Histogram of Logged Housing Sales in Bridgeport, CT.pdf",
  plot = p2
)

ggsave(
  filename = "Housing Prices Over Time.pdf",
  plot = p3
)

ggsave(
  filename = "Heat Map of Housing Prices by Zone.pdf",
  plot = p4
)

ggsave(
  filename = "Sales Ratio Distribution by Property Type.pdf",
  plot = p5
)