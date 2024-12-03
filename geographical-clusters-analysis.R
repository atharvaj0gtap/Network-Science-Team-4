install.packages("dbscan")
library(dbscan)

data_clean <- read.csv("/Users/ishikaagarwal/Downloads/data_clean.csv")
# Extract coordinates
coords <- data_clean[, c("fire_location_longitude", "fire_location_latitude")]

# DBSCAN clustering
clusters <- dbscan(coords, eps = 0.5, minPts = 5)

# clusters to the data
data_clean$cluster <- clusters$cluster

library(ggplot2)

ggplot(data_clean, aes(x = fire_location_longitude, y = fire_location_latitude, color = factor(cluster))) +
  geom_point() +
  labs(title = "Geographical Clustering of Wildfires",
       x = "Longitude", y = "Latitude", color = "Cluster")
clusters <- dbscan(coords, eps = 0.1, minPts = 10)  

# Add new clusters to the dataset
data_clean$cluster <- clusters$cluster

# Plot updated clusters
ggplot(data_clean, aes(x = fire_location_longitude, y = fire_location_latitude, color = factor(cluster))) +
  geom_point() +
  labs(title = "Refined Geographical Clustering of Wildfires",
       x = "Longitude", y = "Latitude", color = "Cluster")

library(dplyr)

# Summarize fire attributes by cluster
cluster_summary <- data_clean %>%
  group_by(cluster) %>%
  summarise(
    avg_fire_size = mean(current_size, na.rm = TRUE),
    total_fires = n(),
    dominant_cause = names(which.max(table(general_cause_desc)))
  )

print(cluster_summary) #cluster summary table

head(cluster_summary)

# Merge cluster_summary into data_clean
data_clean <- data_clean %>%
  left_join(cluster_summary %>% select(cluster, dominant_cause), by = "cluster")

# Check if dominant_cause exists in data_clean
head(data_clean)

# Group by dominant_cause and summarize
cause_summary <- data_clean %>%
  group_by(dominant_cause) %>%
  summarise(
    total_fires = n(),
    avg_fire_size = mean(current_size, na.rm = TRUE)
  )

# data grouped by dominant cause 
print(cause_summary)

#graph of the total fires by dominant cause of wildfire dataset
ggplot(cause_summary, aes(x = reorder(dominant_cause, -total_fires), y = total_fires, fill = dominant_cause)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Fires by Dominant Cause",
       x = "Cause", y = "Number of Fires", fill = "Cause") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#graph of the average fire size by dominant cause of wildfire dataset
ggplot(cause_summary, aes(x = reorder(dominant_cause, -avg_fire_size), y = avg_fire_size, fill = dominant_cause)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Fire Size by Dominant Cause",
       x = "Cause", y = "Average Fire Size (ha)", fill = "Cause") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Count causes by cluster
cause_distribution <- data_clean %>%
  group_by(cluster, general_cause_desc) %>%
  summarise(count = n())

# Plot
ggplot(cause_distribution, aes(x = factor(cluster), y = count, fill = general_cause_desc)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Wildfire Causes by Cluster",
       x = "Cluster", y = "Number of Fires", fill = "Cause") +
  theme_minimal()


# Calculate average distance from water source by cluster
distance_analysis <- data_clean %>%
  group_by(cluster) %>%
  summarise(avg_distance_water = mean(distance_from_water_source, na.rm = TRUE))

print(distance_analysis) #fires distant from the water source analysis

plot(water_analysis)

# Summarize policies by cluster
policy_analysis <- data_clean %>%
  group_by(cluster, responsible_group_desc) %>%
  summarise(count = n())

# Plot policy distribution
ggplot(policy_analysis, aes(x = factor(cluster), y = count, fill = responsible_group_desc)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Policy Groups by Cluster",
       x = "Cluster", y = "Number of Fires", fill = "Policy Group") +
  theme_minimal()

# Analyze causes by distance to water
cause_distance_analysis <- data_clean %>%
  group_by(general_cause_desc) %>%
  summarise(
    avg_distance_to_water = mean(distance_from_water_source, na.rm = TRUE),
    avg_fire_size = mean(current_size, na.rm = TRUE)
  )

print(cause_distance_analysis)

# Visualize
ggplot(cause_distance_analysis, aes(x = general_cause_desc, y = avg_distance_to_water, fill = general_cause_desc)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Distance to Water by Cause",
       x = "Cause", y = "Average Distance to Water", fill = "Cause") +
  theme_minimal()