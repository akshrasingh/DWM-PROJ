# Load required libraries
library(tidyverse)
library(cluster)
library(fpc)

# Set seed for reproducibility
set.seed(123)

# Load the data
customer <- read.csv("C:/Users/Akshra_/Downloads/Mall_Customers (2).csv", stringsAsFactors = TRUE)

# Select relevant columns (Annual Income and Spending Score)
customer <- customer[, 4:5]

# Perform k-means clustering with k=3
kmeans_model <- kmeans(customer, centers = 3)

# Perform hierarchical clustering
hierarchical_model <- hclust(dist(customer))

# Cut the dendrogram to obtain clusters
hierarchical_clusters <- cutree(hierarchical_model, k = 3)

# Perform DBSCAN clustering
dbscan_model <- dbscan(customer, eps = 3, MinPts = 5)

# Compute Davies-Bouldin index using cluster.stats function
db_kmeans <- cluster.stats(dist(customer), kmeans_model$cluster)$dunn
db_hierarchical <- cluster.stats(dist(customer), hierarchical_clusters)$dunn
db_dbscan <- cluster.stats(dist(customer), dbscan_model$cluster)$dunn

# Print Davies-Bouldin index
cat("Davies-Bouldin Index:\n")
cat(paste("K-means Clustering:", db_kmeans, "\n"))
cat(paste("Hierarchical Clustering:", db_hierarchical, "\n"))
cat(paste("DBSCAN Clustering:", db_dbscan, "\n"))

# Interpretation based on Davies-Bouldin index
cat("\nInterpretation based on Davies-Bouldin Index:\n")
if (db_kmeans < db_hierarchical && db_kmeans < db_dbscan) {
  cat("K-means clustering yields the lowest Davies-Bouldin index, indicating better clustering quality.\n")
} else if (db_hierarchical < db_kmeans && db_hierarchical < db_dbscan) {
  cat("Hierarchical clustering yields the lowest Davies-Bouldin index, suggesting superior clustering performance.\n")
} else {
  cat("DBSCAN clustering yields the lowest Davies-Bouldin index, indicating better clustering quality.\n")
}

# Plot cluster visualizations
par(mfrow=c(2, 2)) # Set up the plotting layout

# Plot for K-means Clustering
plot(customer, col=kmeans_model$cluster, main="K-means Clustering")

# Plot for Hierarchical Clustering
plot(customer, col=hierarchical_clusters, main="Hierarchical Clustering")

# Plot for DBSCAN Clustering
plot(customer, col=dbscan_model$cluster+1, main="DBSCAN Clustering")

# Plot a silhouette plot for DBSCAN clustering
plot(silhouette(dbscan_model$cluster, dist(customer)), main="Silhouette Plot for DBSCAN Clustering")
# Compute silhouette for each clustering method
silhouette_kmeans <- silhouette(kmeans_model$cluster, dist(customer))
silhouette_hierarchical <- silhouette(hierarchical_clusters, dist(customer))
silhouette_dbscan <- silhouette(dbscan_model$cluster, dist(customer))
# Compute silhouette for each clustering method
silhouette_kmeans <- silhouette(kmeans_model$cluster, dist(customer))
silhouette_hierarchical <- silhouette(hierarchical_clusters, dist(customer))
silhouette_dbscan <- silhouette(dbscan_model$cluster, dist(customer))

# Calculate mean silhouette width
sil_width_kmeans <- mean(silhouette_kmeans[, 3])
sil_width_hierarchical <- mean(silhouette_hierarchical[, 3])
sil_width_dbscan <- mean(silhouette_dbscan[, 3])

# Print silhouette width
cat("Silhouette Width:\n")
cat(paste("K-means Clustering:", sil_width_kmeans, "\n"))
cat(paste("Hierarchical Clustering:", sil_width_hierarchical, "\n"))
cat(paste("DBSCAN Clustering:", sil_width_dbscan, "\n"))

# Interpretation based on silhouette width
cat("\nInterpretation based on Silhouette Width:\n")
if (sil_width_kmeans > sil_width_hierarchical && sil_width_kmeans > sil_width_dbscan) {
  cat("K-means clustering yields the highest silhouette width, indicating better clustering quality.\n")
} else if (sil_width_hierarchical > sil_width_kmeans && sil_width_hierarchical > sil_width_dbscan) {
  cat("Hierarchical clustering yields the highest silhouette width, suggesting superior clustering performance.\n")
} else {
  cat("DBSCAN clustering yields the highest silhouette width, indicating better clustering quality.\n")
}

