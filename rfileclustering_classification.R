# Install the fpc package if it's not already installed
install.packages("fpc")

# Load required libraries
library(tidyverse) # metapackage of all tidyverse packages
library(cluster)
library(fpc) # Required for DBSCAN

# Set seed for reproducibility
set.seed(123)

# Load the data
customer <- read.csv("C:/Users/Akshra_/Downloads/Mall_Customers (2).csv", stringsAsFactors = TRUE)

# Display the first few rows of the dataset
head(customer)

# Select relevant columns (Annual Income and Spending Score)
customer <- customer[, 4:5]

# Plot the data to visualize the distribution
par(mfrow=c(1,2)) # Set up the plotting layout
plot(customer, main = "Customer Data", xlab = "Annual Income", ylab = "Spending Score")

# Perform k-means clustering with k=3
kmeans_model <- kmeans(customer, centers = 3)

# Visualize the clusters using a scatterplot
clusplot(customer, kmeans_model$cluster, lines = 0, shade = TRUE, color = TRUE, main = "K-means Clustering")

# Plot the Elbow Method to determine optimal k
wcss <- vector()
for (i in 1:10) {
  wcss[i] <- sum(kmeans(customer, centers = i)$withinss)
}
plot(1:10, wcss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters (k)", ylab = "Within-Cluster Sum of Squares (WCSS)", main = "Elbow Method")

# Perform hierarchical clustering
hierarchical_model <- hclust(dist(customer))

# Visualize the dendrogram for hierarchical clustering
plot(hierarchical_model, main = "Dendrogram for Hierarchical Clustering")

# Cut the dendrogram to obtain clusters
hierarchical_clusters <- cutree(hierarchical_model, k = 3)

# Visualize hierarchical clustering results
plot(customer, col = hierarchical_clusters, main = "Hierarchical Clustering")

# Plot a silhouette plot for hierarchical clustering
plot(silhouette(hierarchical_clusters, dist(customer)), main = "Silhouette Plot for Hierarchical Clustering")

# Perform DBSCAN clustering
dbscan_model <- dbscan(customer, eps = 3, MinPts = 5)

# Visualize DBSCAN clustering results
plot(customer, col = dbscan_model$cluster + 1, main = "DBSCAN Clustering")
legend("topright", legend = unique(dbscan_model$cluster), col = unique(dbscan_model$cluster) + 1, pch = 19, title = "Cluster")

# Plot a silhouette plot for DBSCAN clustering
plot(silhouette(dbscan_model$cluster, dist(customer)), main = "Silhouette Plot for DBSCAN Clustering")

# Additional visualizations
par(mfrow=c(2,2)) # Set up the plotting layout
# Density plot for Annual Income
plot(density(customer$Annual.Income..k..), main = "Density Plot for Annual Income", xlab = "Annual Income")

# Density plot for Spending Score
plot(density(customer$Spending.Score..1.100.), main = "Density Plot for Spending Score", xlab = "Spending Score")

# Boxplot for Annual Income
boxplot(customer$Annual.Income..k.., main = "Boxplot for Annual Income", ylab = "Annual Income")

# Boxplot for Spending Score
boxplot(customer$Spending.Score..1.100., main = "Boxplot for Spending Score", ylab = "Spending Score")
