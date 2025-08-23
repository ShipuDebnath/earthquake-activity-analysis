# Earthquake Activity Analysis (Past 30 Days)
# Purpose: Analyze and visualize earthquake data over the past 30 days
# Author: Shipu Debnath
# Date: August 22, 2025

# Install and load required packages
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("tmap", quietly = TRUE)) install.packages("tmap")
if (!requireNamespace("tmaptools", quietly = TRUE)) install.packages("tmaptools")
if (!requireNamespace("forcats", quietly = TRUE)) install.packages("forcats")
if (!requireNamespace("wordcloud", quietly = TRUE)) install.packages("wordcloud")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("lsr", quietly = TRUE)) install.packages("lsr")
if (!requireNamespace("tidycensus", quietly = TRUE)) install.packages("tidycensus")

library(tidyverse)    # Data manipulation and visualization
library(tmap)         # Thematic maps
library(tmaptools)    # Map tools
library(forcats)      # Factor manipulation
library(wordcloud)    # Word cloud visualization
library(sf)           # Spatial data handling
library(lsr)          # Effect size calculations
library(tidycensus)   # Census data (optional, ensure API key if used)

# Load and preprocess earthquake data (replace with actual source if needed)
# Assuming all_month is a data frame; e.g., all_month <- read.csv("data/earthquakes.csv")
if (!exists("all_month")) stop("all_month dataset not found. Load or define it first.")
eq_data <- all_month[, c(1:6, 14)]  # Select relevant columns

# Extract date and clean up
eq_data$Date <- as.Date(substr(eq_data$time, 1, 10), format = "%Y-%m-%d")
eq_data$time <- NULL  # Remove time column
eq_data$magType <- tolower(eq_data$magType)
eq_data$magType <- as.factor(eq_data$magType)

# Parse location from place column
loc_data <- str_split(eq_data$place, ",", simplify = TRUE) %>%
  as.data.frame()
loc_data$V2 <- trimws(loc_data$V2)  # Trim whitespace
loc_data$V2[loc_data$V2 == "Aleutian Islands"] <- "Alaska"
loc_data$V2[loc_data$V2 == "CA"] <- "California"
eq_data$location <- loc_data$V2
eq_data$place <- NULL
eq_data$location <- as.factor(eq_data$location)

# Filter out non-positive magnitudes and summarize
eq_data <- eq_data[eq_data$mag > 0, ]
summary(eq_data)

# Visualize magnitude distribution
mag_hist <- ggplot(eq_data, aes(x = mag)) + 
  geom_histogram(col = "#af33ff", fill = "#af33ff", bins = 20) +
  scale_x_continuous(breaks = seq(0, 7, by = 0.5)) +
  labs(title = "Earthquake Magnitudes for the Past 30 Days",
       x = "Magnitude", y = "Number of Earthquakes") +
  theme_light()
print(mag_hist)
ggsave("plots/magnitude_distribution.png", mag_hist, width = 8, height 6)

# Average magnitude over time
daily_avg <- aggregate(mag ~ Date, data = eq_data, mean)
time_plot <- ggplot(daily_avg, aes(x = Date, y = mag)) + 
  geom_line(color = "blue") + 
  geom_point(color = "blue") +
  labs(title = "Average Earthquake Activity over the Past 30 Days",
       y = "Average Magnitude") +
  theme_light()
print(time_plot)
ggsave("plots/average_magnitude_time.png", time_plot, width = 10, height 6)

# Locations with average magnitude > 4.7
loc_avg <- aggregate(mag ~ location, data = eq_data, mean)
high_intensity <- loc_avg[loc_avg$mag > 4.7, ]
loc_bar <- ggplot(high_intensity, aes(x = fct_reorder(location, mag), y = mag)) + 
  geom_bar(stat = "identity", color = "#c3831a", fill = "#c3831a") +
  labs(title = "Locations with the Most Intense Earthquake Activity\nover the Past 30 Days",
       x = "Location", y = "Average Magnitude") +
  coord_flip() +
  theme_light()
print(loc_bar)
ggsave("plots/high_intensity_locations.png", loc_bar, width = 8, height 6)

# Word cloud of location frequencies
loc_freq <- data.frame(table(eq_data$location))
wordcloud(loc_freq$Var1, loc_freq$Freq, scale = c(3, 1), min.freq = 5,
          colors = c("#af33ff", "#c3831a", "blue"))
ggsave("plots/location_wordcloud.png", plot = last_plot(), width = 8, height 6)

# Spatial analysis for California, Nevada, and Alaska
cn_data <- eq_data[eq_data$location %in% c("California", "Nevada", "Alaska"), ]
cn_sf <- st_as_sf(cn_data, coords = c("longitude", "latitude"))
cn_sf <- rename(cn_sf, magnitude = mag)

# Load and filter state boundaries
data(state_laea)
states <- st_as_sf(state_laea)
states_cn <- states[states$GEOID %in% c("06", "32", "02"), ]  # CA (06), NV (32), AK (02)

# Create map
cn_map <- tm_shape(states_cn) + 
  tm_polygons(col = "#f2fbd2") + 
  tm_shape(cn_sf) + 
  tm_bubbles(size = 0.1, col = "magnitude", palette = "Reds") + 
  tm_layout(main.title = "Earthquakes in California, Nevada, and Alaska\nover the Past 30 Days",
            main.title.size = 1, legend.outside = TRUE)
print(cn_map)
tmap_save(cn_map, "plots/cn_earthquake_map.png")

# Statistical tests for California vs. Nevada
cn_subset <- eq_data[eq_data$location %in% c("California", "Nevada"), ]
t_test_result <- t.test(cn_subset$mag ~ factor(cn_subset$location))
print("T-test for California vs. Nevada Magnitudes:")
print(t_test_result)
cohens_d <- cohensD(cn_subset$mag ~ factor(cn_subset$location))
print("Cohen's d for California vs. Nevada Magnitudes:")
print(cohens_d)

# Statistical tests for California vs. Alaska
cak_subset <- eq_data[eq_data$location %in% c("California", "Alaska"), ]
cak_sf <- st_as_sf(cak_subset, coords = c("longitude", "latitude"))
cak_sf <- rename(cak_sf, magnitude = mag)
cak_map <- tm_shape(states_cn) + 
  tm_polygons(col = "#f2fbd2") + 
  tm_shape(cak_sf) + 
  tm_bubbles(size = 0.1, col = "magnitude", palette = "Reds") + 
  tm_layout(main.title = "Earthquakes in California and Alaska\nover the Past 30 Days",
            main.title.size = 1, legend.outside = TRUE)
print(cak_map)
tmap_save(cak_map, "plots/cak_earthquake_map.png")

t_test_result_cak <- t.test(cak_subset$mag ~ factor(cak_subset$location))
print("T-test for California vs. Alaska Magnitudes:")
print(t_test_result_cak)
cohens_d_cak <- cohensD(cak_subset$mag ~ factor(cak_subset$location))
print("Cohen's d for California vs. Alaska Magnitudes:")
print(cohens_d_cak)

# Save statistical results
sink("results/statistical_summary.txt")
cat("T-test and Cohen's d Results:\n")
cat("California vs. Nevada:\n")
print(t_test_result)
print(cohens_d)
cat("California vs. Alaska:\n")
print(t_test_result_cak)
print(cohens_d_cak)
sink()
