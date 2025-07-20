library(ggplot2)
library(dplyr)
library(maps)
library(ggpubr)

# Read your CSV file (replace 'your_file.csv' with the actual file path)
# Assuming the CSV has columns 'FIPS' (state FIPS code) and 'Value' (for coloring)
data <- read.csv("R_openness_2012.csv")

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
  left_join(fips_map_conus, by = c("State" = "State"))

# Join the map data with the filtered and mapped data
map_data <- us_map %>%
  left_join(data_conus, by = "region")

# Plot the map
plot1<-ggplot(map_data, aes(x = long, y = lat, group = group, fill = Real_Openness)) +
  theme_bw()+
  geom_polygon(color = "white") +  # Add state borders
  # geom_polygon() +
  # coord_fixed(1.3) +  # Fix aspect ratio
  labs(fill = "Trade\nOpenness", x = NULL, y = NULL) +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    legend.position = "bottom",         # Move legend to the bottom
    legend.direction = "horizontal",    # Make legend horizontal
    legend.title = element_text(size = 10,
                                margin = margin(r = 15)),  # Adjust legend title size (optional)
    legend.text = element_text(size = 8)     # Adjust legend text size (optional)
  )+
  scale_fill_continuous(limits = c(0, 2.5))+
  labs(caption = "(a) Trade openness in 2012")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))
plot1 <- plot1 + theme(legend.position = "none")

########
data <- read.csv("R_openness_2017.csv")
fips_map_conus <- fips_map %>%
  filter(region %in% unique(us_map$region))  # Keep only CONUS states

# Join data with FIPS mapping to add the `region` column
data_conus <- data %>%
  left_join(fips_map_conus, by = c("State" = "State"))

# Join the map data with the filtered and mapped data
map_data <- us_map %>%
  left_join(data_conus, by = "region")

# Plot the map
plot2<-ggplot(map_data, aes(x = long, y = lat, group = group, fill = Real_Openness)) +
  theme_bw()+
  geom_polygon(color = "white") +  # Add state borders
  # geom_polygon() +
  # coord_fixed(1.3) +  # Fix aspect ratio
  scale_fill_continuous(limits = c(0, 2.5))+
  labs(fill = "Trade\nOpenness", x = NULL, y = NULL) +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    legend.position = "bottom",         # Move legend to the bottom
    legend.direction = "horizontal",    # Make legend horizontal
    legend.title = element_text(size = 10,
                                margin = margin(r = 15)),  # Adjust legend title size (optional)
    legend.text = element_text(size = 8)     # Adjust legend text size (optional)
  )+
  labs(caption = "(b) Trade openness in 2017")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))

leg <- get_legend(plot2)

plot2 <- plot2 + theme(legend.position = "none")

###############
########
data <- read.csv("R_openness_2022.csv")
fips_map_conus <- fips_map %>%
  filter(region %in% unique(us_map$region))  # Keep only CONUS states

# Join data with FIPS mapping to add the `region` column
data_conus <- data %>%
  left_join(fips_map_conus, by = c("State" = "State"))

# Join the map data with the filtered and mapped data
map_data <- us_map %>%
  left_join(data_conus, by = "region")

# Plot the map
plot3<-ggplot(map_data, aes(x = long, y = lat, group = group, fill = Real_Openness)) +
  theme_bw()+
  geom_polygon(color = "white") +  # Add state borders
  # geom_polygon() +
  # coord_fixed(1.3) +  # Fix aspect ratio
  scale_fill_continuous(limits = c(0, 2.5))+
  labs(fill = "Trade\nOpenness", x = NULL, y = NULL) +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    legend.position = "bottom",         # Move legend to the bottom
    legend.direction = "horizontal",    # Make legend horizontal
    legend.title = element_text(size = 10,
                                margin = margin(r = 15)),  # Adjust legend title size (optional)
    legend.text = element_text(size = 8)     # Adjust legend text size (optional)
  )+
  labs(caption = "(c) Trade openness in 2022")+
  theme(plot.caption = element_text(hjust = 0.5, size=10))

leg <- get_legend(plot3)

plot3 <- plot3 + theme(legend.position = "none")
#####################

combined_plot <- ggarrange(
  ggarrange(plot1, plot2,
            ncol=2,
            nrow=1,
            heights = c(1,1)), 
  plot3, 
  leg,
  ncol = 1, # One column (vertical arrangement)
  nrow = 3 , # Two rows
  heights = c(0.5, 0.5, 0.1), # Optional: Adjust relative heights if needed
  widths = c(1, 0.5, 1)
)+bgcolor("white")

ggsave('to_combined.png', plot = last_plot(),
       width=7, height = 8.5,
       units = 'in', bg="white")
ggsave('to_combined.pdf', plot = last_plot(),
       width=7, height = 8.5,
       units = 'in', bg="white")
ggsave('to_combined.jpg', plot = last_plot(),
       width=7, height = 8.5,
       units = 'in', bg="white")

combined_plot <- ggarrange(
  plot1, plot2, 
  plot3, 
  leg,
  ncol = 2, # One column (vertical arrangement)
  nrow = 2 , # Two rows
  heights = c(0.5, 0.5, 0.5, 0.1), # Optional: Adjust relative heights if needed
  widths = c(0.5, 0.5, 0.5, 1)
)

combined_plot

ggsave('to_combined.png', plot = last_plot(),
       width=7, height = 4.5,
       units = 'in', bg="white")
ggsave('to_combined.pdf', plot = last_plot(),
       width=7, height = 4.5,
       units = 'in', bg="white")
ggsave('to_combined.jpg', plot = last_plot(),
       width=7, height = 4.5,
       units = 'in', bg="white")
