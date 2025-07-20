library(ggplot2)
library(dplyr)
library(maps)
library(ggpubr)
library(RColorBrewer)
library(viridis)

# Read your CSV file (replace 'your_file.csv' with the actual file path)
# Assuming the CSV has columns 'FIPS' (state FIPS code) and 'Value' (for coloring)
data <- read.csv("water_data_states_2012.csv")
data$States <- data$State

us_map <- map_data("state")

fips_map <- data.frame(
  State = c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
  region = c(
    "alabama", "alaska", "arizona", "arkansas", "california", "colorado", "connecticut", "delaware", "district of columbia",
    "florida", "georgia", "idaho", "illinois", "indiana", "iowa", "kansas", "kentucky", "louisiana", "maine", "maryland",
    "massachusetts", "michigan", "minnesota", "mississippi", "missouri", "montana", "nebraska", "nevada", "new hampshire",
    "new jersey", "new mexico", "new york", "north carolina", "north dakota", "ohio", "oklahoma", "oregon", "pennsylvania",
    "rhode island", "south carolina", "south dakota", "tennessee", "texas", "utah", "vermont", "virginia", "washington",
    "west virginia", "wisconsin", "wyoming"
  )
)

# Filter to match CONUS states
fips_map_conus <- fips_map %>%
  filter(region %in% unique(us_map$region))  # Keep only CONUS states

# Join data with FIPS mapping to add the `region` column
data_conus <- data %>%
  left_join(fips_map_conus, by = c("States" = "State"))

# Join the map data with the filtered and mapped data
map_data <- us_map %>%
  left_join(data_conus, by = "region")

# Plot the map
plot1<-ggplot(map_data, aes(x = long, y = lat, group = group, fill = next_yr_Irrigated_water)) +
  theme_bw()+
  geom_polygon(color = "white") +  # Add state borders
  # geom_polygon() +
  # coord_fixed(1.3) +  # Fix aspect ratio
  labs(fill = "Total irrigation water withdrawals (1000 gal)", x = NULL, y = NULL) +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    legend.position = "bottom",         # Move legend to the bottom
    legend.direction = "horizontal",    # Make legend horizontal
    legend.title = element_text(size = 10,
                                margin = margin(r = 15)),  # Adjust legend title size (optional)
    legend.text = element_text(size = 8)     # Adjust legend text size (optional)
  )+
  #scale_fill_continuous(limits = c(0, 2300), palette = "YlOrRd")+
  labs(caption = "(a) Total irrigation water withdrawals in 2013")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))+
  #scale_fill_viridis(option = "A", limits = c(0, 2300))
  #scale_fill_gradientn(colors = c("mistyrose", "red"), limits = c(0, 2300))
  scale_fill_gradientn(colors = c("palegreen", "darkgreen"), limits = c(0, 24000000),
                       breaks=c(0, 12000000, 24000000))
#plot1 <- plot1 + theme(legend.position = "none")

########

plot2<-ggplot(map_data, aes(x = long, y = lat, group = group, fill = next_yr_Irrigated_water_gw)) +
  theme_bw()+
  geom_polygon(color = "white") +  # Add state borders
  # geom_polygon() +
  #scale_fill_viridis(option = "D", limits = c(0, 400))+
  scale_fill_gradientn(colors = c("darkgreen", "yellow", "red"), limits = c(0, 10000000),
                       breaks=c(0, 5000000, 10000000))+
  labs(fill = "Irrigation groundwater withdrawals (1000 gal)", x = NULL, y = NULL) +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    legend.position = "bottom",         # Move legend to the bottom
    legend.direction = "horizontal",    # Make legend horizontal
    legend.title = element_text(size = 10,
                                margin = margin(r = 15)),  # Adjust legend title size (optional)
    legend.text = element_text(size = 8)     # Adjust legend text size (optional)
  )+
  labs(caption = "(b) Irrigation groundwater withdrawals in 2013")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))

plot2<-plot2+
  labs(fill = "Irrigation groundwater\nwithdrawals (1000 gal)", x = NULL, y = NULL)

plot1<-plot1+
  labs(fill = "Total irrigation water\nwithdrawals (1000 gal)", x = NULL, y = NULL)

# Plot the map
plot2_new<-ggplot(map_data, aes(x = long, y = lat, group = group, fill = next_yr_Irrigated_water_gw*100/next_yr_Irrigated_water)) +
  theme_bw()+
  geom_polygon(color = "white") +  # Add state borders
  # geom_polygon() +
  #scale_fill_viridis(option = "D", limits = c(0, 400))+
  scale_fill_gradientn(colors = c("darkgreen", "yellow", "red"), limits = c(0, 100),
                       breaks=c(0, 50, 100))+
  labs(fill = "Irrigation groundwater withdrawals\n(% of Total irrigation withdrawals)", x = NULL, y = NULL) +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    legend.position = "bottom",         # Move legend to the bottom
    legend.direction = "horizontal",    # Make legend horizontal
    legend.title = element_text(size = 10,
                                margin = margin(r = 15)),  # Adjust legend title size (optional)
    legend.text = element_text(size = 8)     # Adjust legend text size (optional)
  )+
  labs(caption = "(c) Irrigation groundwater withdrawals as percent in 2013")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))

#leg <- get_legend(plot2)

#plot2 <- plot2 + theme(legend.position = "none")

#combined_plot <- ggarrange(
#  ggarrange(plot1, plot2,
#            ncol=2), plot2_new,
#  ncol = 1, # One column (vertical arrangement)
#  nrow = 2 , # Two rows
#  heights = c(1, 1, 1), # Optional: Adjust relative heights if needed,
#  widths = c(1, 1, 0.5)
#)+ bgcolor("white")

top_row <- ggarrange(plot1, plot2, ncol = 2, widths = c(1, 1))

# Create a bottom row with plot2_new centered (empty space left and right)
bottom_row <- ggarrange(NULL, plot2_new, NULL, 
                        ncol = 3, 
                        widths = c(1, 2, 1))  # adjust 2 for plot2_new width

# Combine the two rows vertically
combined_plot <- ggarrange(top_row, bottom_row, 
                           ncol = 1, 
                           heights = c(1, 1)) +
  bgcolor("white")

ggsave('irr_wu_combined.png', plot = last_plot(),
       width=7, height = 6.5, bg="white",
       units = 'in')
ggsave('irr_wu_combined.pdf', plot = last_plot(),
       width=7, height = 6.5, bg="white",
       units = 'in')
ggsave('irr_wu_combined.jpg', plot = last_plot(),
       width=7, height = 6.5, bg="white",
       units = 'in')

# Plot the map
plot2<-ggplot(map_data, aes(x = long, y = lat, group = group, fill = next_yr_Irrigated_water_gw)) +
  theme_bw()+
  geom_polygon(color = "white") +  # Add state borders
  # geom_polygon() +
  #scale_fill_viridis(option = "D", limits = c(0, 400))+
  scale_fill_gradientn(colors = c("darkgreen", "yellow", "red"), limits = c(0, 10000000),
                       breaks=c(0, 5000000, 10000000))+
  labs(fill = "Irrigation groundwater withdrawals (1000 gal)", x = NULL, y = NULL) +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    legend.position = "bottom",         # Move legend to the bottom
    legend.direction = "horizontal",    # Make legend horizontal
    legend.title = element_text(size = 10,
                                margin = margin(r = 15)),  # Adjust legend title size (optional)
    legend.text = element_text(size = 8)     # Adjust legend text size (optional)
  )+
  labs(caption = "(b) Irrigation groundwater withdrawals in 2013")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))

plot2<-plot2+
  labs(fill = "Irrigation groundwater\nwithdrawals (1000 gal)", x = NULL, y = NULL)

plot1<-plot1+
  labs(fill = "Total irrigation water\nwithdrawals (1000 gal)", x = NULL, y = NULL)

plot3<-ggplot(map_data, aes(x = long, y = lat, group = group, fill = next_yr_Irrigated_water_sw)) +
  theme_bw()+
  geom_polygon(color = "white") +  # Add state borders
  # geom_polygon() +
  #scale_fill_viridis(option = "D", limits = c(0, 400))+
  scale_fill_gradientn(colors = c("lightyellow", "darkred"), limits = c(0, 10000000),
                       breaks=c(0, 5000000, 10000000))+
  labs(fill = "Irrigation surface water\nwithdrawals (1000 gal)", x = NULL, y = NULL) +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    legend.position = "bottom",         # Move legend to the bottom
    legend.direction = "horizontal",    # Make legend horizontal
    legend.title = element_text(size = 10,
                                margin = margin(r = 15)),  # Adjust legend title size (optional)
    legend.text = element_text(size = 8)     # Adjust legend text size (optional)
  )+
  labs(caption = "(c) Irrigation surface water withdrawals in 2013")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))


plot4<-ggplot(map_data, aes(x = long, y = lat, group = group, fill = next_yr_Irrigated_area)) +
  theme_bw()+
  geom_polygon(color = "white") +  # Add state borders
  # geom_polygon() +
  #scale_fill_viridis(option = "D", limits = c(0, 400))+
  scale_fill_gradientn(colors = c("white", "darkorange"), limits = c(0, 9000000),
                       breaks=c(0, 4500000, 9000000))+
  labs(fill = "Total irrigated\narea (acres)", x = NULL, y = NULL) +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    legend.position = "bottom",         # Move legend to the bottom
    legend.direction = "horizontal",    # Make legend horizontal
    legend.title = element_text(size = 10,
                                margin = margin(r = 15)),  # Adjust legend title size (optional)
    legend.text = element_text(size = 8)     # Adjust legend text size (optional)
  )+
  labs(caption = "(c) Total irrigated area in 2013")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))

plot5<-ggplot(map_data, aes(x = long, y = lat, group = group, fill = next_yr_Irrigated_area_gw)) +
  theme_bw()+
  geom_polygon(color = "white") +  # Add state borders
  # geom_polygon() +
  #scale_fill_viridis(option = "D", limits = c(0, 400))+
  scale_fill_gradientn(colors = c("blue", "lightgrey", "red"), limits = c(0, 8000000),
                       breaks=c(0, 4000000, 8000000))+
  labs(fill = "Groundwater-irrigated\narea (acres)", x = NULL, y = NULL) +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    legend.position = "bottom",         # Move legend to the bottom
    legend.direction = "horizontal",    # Make legend horizontal
    legend.title = element_text(size = 10,
                                margin = margin(r = 15)),  # Adjust legend title size (optional)
    legend.text = element_text(size = 8)     # Adjust legend text size (optional)
  )+
  labs(caption = "(d) Groundwater-irrigated area in 2013")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))

plot6<-ggplot(map_data, aes(x = long, y = lat, group = group, fill = next_yr_Irrigated_area_sw)) +
  theme_bw()+
  geom_polygon(color = "white") +  # Add state borders
  # geom_polygon() +
  #scale_fill_viridis(option = "D", limits = c(0, 400))+
  scale_fill_gradientn(colors = c("lightgrey", "brown"), limits = c(0, 1500000),
                       breaks=c(0, 750000, 1500000))+
  labs(fill = "Surface water-irrigated\narea (acres)", x = NULL, y = NULL) +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    legend.position = "bottom",         # Move legend to the bottom
    legend.direction = "horizontal",    # Make legend horizontal
    legend.title = element_text(size = 10,
                                margin = margin(r = 15)),  # Adjust legend title size (optional)
    legend.text = element_text(size = 8)     # Adjust legend text size (optional)
  )+
  labs(caption = "(e) Surface water-irrigated area in 2013")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))

combined_plot <- ggarrange(
  plot1, plot2, plot3,
  plot4, plot5, plot6,
  ncol = 2, # One column (vertical arrangement)
  nrow = 3 , # Two rows
  heights = c(1, 1, 1, 1, 1, 1) # Optional: Adjust relative heights if needed
)+ bgcolor("white")

ggsave('irr_wu_combined_2013.png', plot = last_plot(),
       width=7, height = 9,
       units = 'in')
ggsave('irr_wu_combined_2013.pdf', plot = last_plot(),
       width=7, height = 9,
       units = 'in')
ggsave('irr_wu_combined_2013.jpg', plot = last_plot(),
       width=7, height = 9,
       units = 'in')

############################################

### 2018

# Read your CSV file (replace 'your_file.csv' with the actual file path)
# Assuming the CSV has columns 'FIPS' (state FIPS code) and 'Value' (for coloring)
data <- read.csv("water_data_states_2017.csv")
data$States <- data$State

fips_map <- data.frame(
  State = c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
  region = c(
    "alabama", "alaska", "arizona", "arkansas", "california", "colorado", "connecticut", "delaware", "district of columbia",
    "florida", "georgia", "idaho", "illinois", "indiana", "iowa", "kansas", "kentucky", "louisiana", "maine", "maryland",
    "massachusetts", "michigan", "minnesota", "mississippi", "missouri", "montana", "nebraska", "nevada", "new hampshire",
    "new jersey", "new mexico", "new york", "north carolina", "north dakota", "ohio", "oklahoma", "oregon", "pennsylvania",
    "rhode island", "south carolina", "south dakota", "tennessee", "texas", "utah", "vermont", "virginia", "washington",
    "west virginia", "wisconsin", "wyoming"
  )
)

# Filter to match CONUS states
fips_map_conus <- fips_map %>%
  filter(region %in% unique(us_map$region))  # Keep only CONUS states

# Join data with FIPS mapping to add the `region` column
data_conus <- data %>%
  left_join(fips_map_conus, by = c("States" = "State"))

# Join the map data with the filtered and mapped data
map_data <- us_map %>%
  left_join(data_conus, by = "region")

# Plot the map
plot1<-ggplot(map_data, aes(x = long, y = lat, group = group, fill = next_yr_Irrigated_water)) +
  theme_bw()+
  geom_polygon(color = "white") +  # Add state borders
  # geom_polygon() +
  # coord_fixed(1.3) +  # Fix aspect ratio
  labs(fill = "Total irrigation water withdrawals (1000 gal)", x = NULL, y = NULL) +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    legend.position = "bottom",         # Move legend to the bottom
    legend.direction = "horizontal",    # Make legend horizontal
    legend.title = element_text(size = 10,
                                margin = margin(r = 15)),  # Adjust legend title size (optional)
    legend.text = element_text(size = 8)     # Adjust legend text size (optional)
  )+
  #scale_fill_continuous(limits = c(0, 2300), palette = "YlOrRd")+
  labs(caption = "(a) Total irrigation water withdrawals in 2018")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))+
  #scale_fill_viridis(option = "A", limits = c(0, 2300))
  #scale_fill_gradientn(colors = c("mistyrose", "red"), limits = c(0, 2300))
  scale_fill_gradientn(colors = c("palegreen", "darkgreen"), limits = c(0, 25000000),
                       breaks=c(0, 12000000, 25000000))
#plot1 <- plot1 + theme(legend.position = "none")

########


# Plot the map
plot2<-ggplot(map_data, aes(x = long, y = lat, group = group, fill = next_yr_Irrigated_water_gw)) +
  theme_bw()+
  geom_polygon(color = "white") +  # Add state borders
  # geom_polygon() +
  #scale_fill_viridis(option = "D", limits = c(0, 400))+
  scale_fill_gradientn(colors = c("darkgreen", "yellow", "red"), limits = c(0, 15000000),
                       breaks=c(0, 7500000, 15000000))+
  labs(fill = "Irrigation groundwater withdrawals (1000 gal)", x = NULL, y = NULL) +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    legend.position = "bottom",         # Move legend to the bottom
    legend.direction = "horizontal",    # Make legend horizontal
    legend.title = element_text(size = 10,
                                margin = margin(r = 15)),  # Adjust legend title size (optional)
    legend.text = element_text(size = 8)     # Adjust legend text size (optional)
  )+
  labs(caption = "(b) Irrigation groundwater withdrawals in 2018")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))



plot2<-plot2+
  labs(fill = "Irrigation groundwater\nwithdrawals (1000 gal)", x = NULL, y = NULL)

plot1<-plot1+
  labs(fill = "Total irrigation water\nwithdrawals (1000 gal)", x = NULL, y = NULL)

plot3<-ggplot(map_data, aes(x = long, y = lat, group = group, fill = next_yr_Irrigated_water_sw)) +
  theme_bw()+
  geom_polygon(color = "white") +  # Add state borders
  # geom_polygon() +
  #scale_fill_viridis(option = "D", limits = c(0, 400))+
  scale_fill_gradientn(colors = c("lightyellow", "darkred"), limits = c(0, 10000000),
                       breaks=c(0, 5000000, 10000000))+
  labs(fill = "Irrigation surface water\nwithdrawals (1000 gal)", x = NULL, y = NULL) +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    legend.position = "bottom",         # Move legend to the bottom
    legend.direction = "horizontal",    # Make legend horizontal
    legend.title = element_text(size = 10,
                                margin = margin(r = 15)),  # Adjust legend title size (optional)
    legend.text = element_text(size = 8)     # Adjust legend text size (optional)
  )+
  labs(caption = "(c) Irrigation surface water withdrawals in 2018")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))


plot4<-ggplot(map_data, aes(x = long, y = lat, group = group, fill = next_yr_Irrigated_area)) +
  theme_bw()+
  geom_polygon(color = "white") +  # Add state borders
  # geom_polygon() +
  #scale_fill_viridis(option = "D", limits = c(0, 400))+
  scale_fill_gradientn(colors = c("white", "darkorange"), limits = c(0, 9000000),
                       breaks=c(0, 4500000, 9000000))+
  labs(fill = "Total irrigated\narea (acres)", x = NULL, y = NULL) +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    legend.position = "bottom",         # Move legend to the bottom
    legend.direction = "horizontal",    # Make legend horizontal
    legend.title = element_text(size = 10,
                                margin = margin(r = 15)),  # Adjust legend title size (optional)
    legend.text = element_text(size = 8)     # Adjust legend text size (optional)
  )+
  labs(caption = "(c) Total irrigated area in 2018")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))

plot5<-ggplot(map_data, aes(x = long, y = lat, group = group, fill = next_yr_Irrigated_area_gw)) +
  theme_bw()+
  geom_polygon(color = "white") +  # Add state borders
  # geom_polygon() +
  #scale_fill_viridis(option = "D", limits = c(0, 400))+
  scale_fill_gradientn(colors = c("blue", "lightgrey", "red"), limits = c(0, 8000000),
                       breaks=c(0, 4000000, 8000000))+
  labs(fill = "Groundwater-irrigated\narea (acres)", x = NULL, y = NULL) +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    legend.position = "bottom",         # Move legend to the bottom
    legend.direction = "horizontal",    # Make legend horizontal
    legend.title = element_text(size = 10,
                                margin = margin(r = 15)),  # Adjust legend title size (optional)
    legend.text = element_text(size = 8)     # Adjust legend text size (optional)
  )+
  labs(caption = "(d) Groundwater-irrigated area in 2018")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))

plot6<-ggplot(map_data, aes(x = long, y = lat, group = group, fill = next_yr_Irrigated_area_sw)) +
  theme_bw()+
  geom_polygon(color = "white") +  # Add state borders
  # geom_polygon() +
  #scale_fill_viridis(option = "D", limits = c(0, 400))+
  scale_fill_gradientn(colors = c("lightgrey", "brown"), limits = c(0, 1500000),
                       breaks=c(0, 750000, 1500000))+
  labs(fill = "Surface water-irrigated\narea (acres)", x = NULL, y = NULL) +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    legend.position = "bottom",         # Move legend to the bottom
    legend.direction = "horizontal",    # Make legend horizontal
    legend.title = element_text(size = 10,
                                margin = margin(r = 15)),  # Adjust legend title size (optional)
    legend.text = element_text(size = 8)     # Adjust legend text size (optional)
  )+
  labs(caption = "(e) Surface water-irrigated area in 2018")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))

combined_plot <- ggarrange(
  plot1, plot2, plot3,
  plot4, plot5, plot6,
  ncol = 2, # One column (vertical arrangement)
  nrow = 3 , # Two rows
  heights = c(1, 1, 1, 1, 1, 1) # Optional: Adjust relative heights if needed
)+ bgcolor("white")


ggsave('irr_wu_combined_2018.png', plot = last_plot(),
       width=7, height = 9,
       units = 'in')
ggsave('irr_wu_combined_2018.pdf', plot = last_plot(),
       width=7, height = 9,
       units = 'in')
ggsave('irr_wu_combined_2018.jpg', plot = last_plot(),
       width=7, height = 9,
       units = 'in')


##########

#######2023


# Read your CSV file (replace 'your_file.csv' with the actual file path)
# Assuming the CSV has columns 'FIPS' (state FIPS code) and 'Value' (for coloring)
data <- read.csv("water_data_states_2022.csv")
data$States <- data$State

fips_map <- data.frame(
  State = c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
  region = c(
    "alabama", "alaska", "arizona", "arkansas", "california", "colorado", "connecticut", "delaware", "district of columbia",
    "florida", "georgia", "idaho", "illinois", "indiana", "iowa", "kansas", "kentucky", "louisiana", "maine", "maryland",
    "massachusetts", "michigan", "minnesota", "mississippi", "missouri", "montana", "nebraska", "nevada", "new hampshire",
    "new jersey", "new mexico", "new york", "north carolina", "north dakota", "ohio", "oklahoma", "oregon", "pennsylvania",
    "rhode island", "south carolina", "south dakota", "tennessee", "texas", "utah", "vermont", "virginia", "washington",
    "west virginia", "wisconsin", "wyoming"
  )
)

# Filter to match CONUS states
fips_map_conus <- fips_map %>%
  filter(region %in% unique(us_map$region))  # Keep only CONUS states

# Join data with FIPS mapping to add the `region` column
data_conus <- data %>%
  left_join(fips_map_conus, by = c("States" = "State"))

# Join the map data with the filtered and mapped data
map_data <- us_map %>%
  left_join(data_conus, by = "region")


# Plot the map
plot1<-ggplot(map_data, aes(x = long, y = lat, group = group, fill = next_yr_Irrigated_water)) +
  theme_bw()+
  geom_polygon(color = "white") +  # Add state borders
  # geom_polygon() +
  # coord_fixed(1.3) +  # Fix aspect ratio
  labs(fill = "Total irrigation water withdrawals (1000 gal)", x = NULL, y = NULL) +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    legend.position = "bottom",         # Move legend to the bottom
    legend.direction = "horizontal",    # Make legend horizontal
    legend.title = element_text(size = 10,
                                margin = margin(r = 15)),  # Adjust legend title size (optional)
    legend.text = element_text(size = 8)     # Adjust legend text size (optional)
  )+
  #scale_fill_continuous(limits = c(0, 2300), palette = "YlOrRd")+
  labs(caption = "(a) Total irrigation water withdrawals in 2018")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))+
  #scale_fill_viridis(option = "A", limits = c(0, 2300))
  #scale_fill_gradientn(colors = c("mistyrose", "red"), limits = c(0, 2300))
  scale_fill_gradientn(colors = c("palegreen", "darkgreen"), limits = c(0, 24000000),
                       breaks=c(0, 12000000, 24000000))
#plot1 <- plot1 + theme(legend.position = "none")

########


# Plot the map
plot2<-ggplot(map_data, aes(x = long, y = lat, group = group, fill = next_yr_Irrigated_water_gw)) +
  theme_bw()+
  geom_polygon(color = "white") +  # Add state borders
  # geom_polygon() +
  #scale_fill_viridis(option = "D", limits = c(0, 400))+
  scale_fill_gradientn(colors = c("darkgreen", "yellow", "red"), limits = c(0, 10000000),
                       breaks=c(0, 5000000, 10000000))+
  labs(fill = "Irrigation groundwater withdrawals (1000 gal)", x = NULL, y = NULL) +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    legend.position = "bottom",         # Move legend to the bottom
    legend.direction = "horizontal",    # Make legend horizontal
    legend.title = element_text(size = 10,
                                margin = margin(r = 15)),  # Adjust legend title size (optional)
    legend.text = element_text(size = 8)     # Adjust legend text size (optional)
  )+
  labs(caption = "(b) Irrigation groundwater withdrawals in 2018")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))



plot2<-plot2+
  labs(fill = "Irrigation groundwater\nwithdrawals (1000 gal)", x = NULL, y = NULL)

plot1<-plot1+
  labs(fill = "Total irrigation water\nwithdrawals (1000 gal)", x = NULL, y = NULL)

plot3<-ggplot(map_data, aes(x = long, y = lat, group = group, fill = next_yr_Irrigated_water_sw)) +
  theme_bw()+
  geom_polygon(color = "white") +  # Add state borders
  # geom_polygon() +
  #scale_fill_viridis(option = "D", limits = c(0, 400))+
  scale_fill_gradientn(colors = c("lightyellow", "darkred"), limits = c(0, 10000000),
                       breaks=c(0, 5000000, 10000000))+
  labs(fill = "Irrigation surface water\nwithdrawals (1000 gal)", x = NULL, y = NULL) +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    legend.position = "bottom",         # Move legend to the bottom
    legend.direction = "horizontal",    # Make legend horizontal
    legend.title = element_text(size = 10,
                                margin = margin(r = 15)),  # Adjust legend title size (optional)
    legend.text = element_text(size = 8)     # Adjust legend text size (optional)
  )+
  labs(caption = "(c) Irrigation surface water withdrawals in 2018")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))


plot4<-ggplot(map_data, aes(x = long, y = lat, group = group, fill = next_yr_Irrigated_area)) +
  theme_bw()+
  geom_polygon(color = "white") +  # Add state borders
  # geom_polygon() +
  #scale_fill_viridis(option = "D", limits = c(0, 400))+
  scale_fill_gradientn(colors = c("white", "darkorange"), limits = c(0, 9000000),
                       breaks=c(0, 4500000, 9000000))+
  labs(fill = "Total irrigated\narea (acres)", x = NULL, y = NULL) +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    legend.position = "bottom",         # Move legend to the bottom
    legend.direction = "horizontal",    # Make legend horizontal
    legend.title = element_text(size = 10,
                                margin = margin(r = 15)),  # Adjust legend title size (optional)
    legend.text = element_text(size = 8)     # Adjust legend text size (optional)
  )+
  labs(caption = "(c) Total irrigated area in 2018")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))

plot5<-ggplot(map_data, aes(x = long, y = lat, group = group, fill = next_yr_Irrigated_area_gw)) +
  theme_bw()+
  geom_polygon(color = "white") +  # Add state borders
  # geom_polygon() +
  #scale_fill_viridis(option = "D", limits = c(0, 400))+
  scale_fill_gradientn(colors = c("blue", "lightgrey", "red"), limits = c(0, 8000000),
                       breaks=c(0, 4000000, 8000000))+
  labs(fill = "Groundwater-irrigated\narea (acres)", x = NULL, y = NULL) +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    legend.position = "bottom",         # Move legend to the bottom
    legend.direction = "horizontal",    # Make legend horizontal
    legend.title = element_text(size = 10,
                                margin = margin(r = 15)),  # Adjust legend title size (optional)
    legend.text = element_text(size = 8)     # Adjust legend text size (optional)
  )+
  labs(caption = "(d) Groundwater-irrigated area in 2018")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))

plot6<-ggplot(map_data, aes(x = long, y = lat, group = group, fill = next_yr_Irrigated_area_sw)) +
  theme_bw()+
  geom_polygon(color = "white") +  # Add state borders
  # geom_polygon() +
  #scale_fill_viridis(option = "D", limits = c(0, 400))+
  scale_fill_gradientn(colors = c("lightgrey", "brown"), limits = c(0, 1500000),
                       breaks=c(0, 750000, 1500000))+
  labs(fill = "Surface water-irrigated\narea (acres)", x = NULL, y = NULL) +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    legend.position = "bottom",         # Move legend to the bottom
    legend.direction = "horizontal",    # Make legend horizontal
    legend.title = element_text(size = 10,
                                margin = margin(r = 15)),  # Adjust legend title size (optional)
    legend.text = element_text(size = 8)     # Adjust legend text size (optional)
  )+
  labs(caption = "(e) Surface water-irrigated area in 2018")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))

combined_plot <- ggarrange(
  plot1, plot2, plot3,
  plot4, plot5, plot6,
  ncol = 2, # One column (vertical arrangement)
  nrow = 3 , # Two rows
  heights = c(1, 1, 1, 1, 1, 1) # Optional: Adjust relative heights if needed
)+ bgcolor("white")


ggsave('irr_wu_combined_2023.png', plot = last_plot(),
       width=7, height = 9,
       units = 'in')
ggsave('irr_wu_combined_2023.pdf', plot = last_plot(),
       width=7, height = 9,
       units = 'in')
ggsave('irr_wu_combined_2023.jpg', plot = last_plot(),
       width=7, height = 9,
       units = 'in')