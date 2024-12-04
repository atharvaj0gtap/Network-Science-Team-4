# Install and load required packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("cluster")
library(ggplot2)
library(dplyr)

# Load the data
data_clean <- read.csv("/Users/ishikaagarwal/Downloads/data_clean.csv")

# Extract relevant variables for clustering (e.g., coordinates, distance from water)
clustering_features <- data_clean[, c("fire_location_longitude", "fire_location_latitude", "distance_from_water_source")]

# Standardize the features for K-Means
clustering_features_scaled <- scale(clustering_features)

# Step 1: Apply K-Means Clustering
set.seed(21368931)
kmeans_result <- kmeans(clustering_features_scaled, centers = 5, nstart = 25)  # Adjust 'centers' as needed

# Add the cluster labels to the dataset
data_clean$kmeans_cluster <- kmeans_result$cluster

# Visualize Clusters Geographically
ggplot(data_clean, aes(x = fire_location_longitude, y = fire_location_latitude, color = factor(kmeans_cluster))) +
  geom_point() +
  labs(title = "Geographical Clustering of Wildfires (K-Means)",
       x = "Longitude", y = "Latitude", color = "Cluster") +
  theme_minimal()

#  Summarize Fire Attributes by Cluster
cluster_summary <- data_clean %>%
  group_by(kmeans_cluster) %>%
  summarise(
    avg_fire_size = mean(current_size, na.rm = TRUE),
    total_fires = n(),
    avg_distance_to_water = mean(distance_from_water_source, na.rm = TRUE),
    dominant_cause = names(which.max(table(general_cause_desc)))
  )

print(cluster_summary)

#  Visualize Total Fires by Cluster
ggplot(cluster_summary, aes(x = factor(kmeans_cluster), y = total_fires, fill = factor(kmeans_cluster))) +
  geom_bar(stat = "identity") +
  labs(title = "Total Fires by K-Means Cluster",
       x = "Cluster", y = "Number of Fires", fill = "Cluster") +
  theme_minimal()

# Analyze Causes by Cluster
cause_distribution <- data_clean %>%
  group_by(kmeans_cluster, general_cause_desc) %>%
  summarise(count = n())

# Visualize Causes by Cluster
ggplot(cause_distribution, aes(x = factor(kmeans_cluster), y = count, fill = general_cause_desc)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Wildfire Causes by K-Means Cluster",
       x = "Cluster", y = "Number of Fires", fill = "Cause") +
  theme_minimal()

#  Analyze and Visualize Distance to Water by Cause
cause_distance_analysis <- data_clean %>%
  group_by(general_cause_desc) %>%
  summarise(
    avg_distance_to_water = mean(distance_from_water_source, na.rm = TRUE),
    avg_fire_size = mean(current_size, na.rm = TRUE)
  )

ggplot(cause_distance_analysis, aes(x = general_cause_desc, y = avg_distance_to_water, fill = general_cause_desc)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Distance to Water by Cause (K-Means)",
       x = "Cause", y = "Average Distance to Water", fill = "Cause") +
  theme_minimal()

