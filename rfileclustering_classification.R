# Load required libraries
library(tidyverse)
library(cluster)
library(fpc)

# Set seed for reproducibility
set.seed(123)

# Load the data
customer <- read.csv("C:\Users\Asus\Downloads\Mall_Customers (2).csv", stringsAsFactors = TRUE)

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

# Load required libraries
library(tidyverse)
library(cluster)
library(fpc)
library(class)

# Set seed for reproducibility
set.seed(123)

customer <- read.csv("C:/Users/Asus/Downloads/Mall_Customers (2).csv", stringsAsFactors = TRUE)


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

# Combine the clustered results with the original data
customer_clustered <- cbind(customer, Kmeans_Cluster = kmeans_model$cluster)

# Split the data into training and testing sets
set.seed(123) # For reproducibility
train_index <- sample(1:nrow(customer_clustered), 0.7 * nrow(customer_clustered))
train_data <- customer_clustered[train_index, ]
test_data <- customer_clustered[-train_index, ]

# Define features (Annual Income and Spending Score) and target (Kmeans_Cluster)
features <- names(train_data)[1:2]
target <- "Kmeans_Cluster"

# Train the KNN model
k <- 5 # Number of neighbors
knn_model <- knn(train = train_data[features], test = test_data[features], cl = train_data[[target]], k = k)

# Evaluate the model
accuracy <- mean(knn_model == test_data[[target]])
cat("Accuracy of KNN model:", accuracy, "\n")

# Train the Random Forest model
rf_model <- randomForest(as.factor(Kmeans_Cluster) ~ ., data = train_data, ntree = 500, importance = TRUE)

# Predict using the Random Forest model
rf_pred <- predict(rf_model, test_data[, 1:2])

# Evaluate the Random Forest model
accuracy_rf <- mean(rf_pred == test_data[[target]])
cat("Accuracy of Random Forest model:", accuracy_rf, "\n")

# Davies-Bouldin Index for Random Forest
rf_clusters <- as.integer(rf_pred)
db_rf <- cluster.stats(dist(test_data[, 1:2]), rf_clusters)$dunn
cat("Davies-Bouldin Index for Random Forest:", db_rf, "\n")

# Davies-Bouldin Index for KNN
knn_clusters <- as.integer(knn_model)
db_knn <- cluster.stats(dist(test_data[, 1:2]), knn_clusters)$dunn
cat("Davies-Bouldin Index for KNN:", db_knn, "\n")

# Confusion Matrix for KNN
confusionMatrix(knn_model, test_data[[target]])

# Confusion Matrix for Random Forest
confusionMatrix(rf_pred, test_data[[target]])

# Plot decision boundaries for KNN and Random Forest
plot1 <- ggplot(train_data, aes(x = customer[, 1], y = customer[, 2], color = as.factor(Kmeans_Cluster))) +
  geom_point() +
  geom_point(data = test_data, aes(x = test_data[, 1], y = test_data[, 2], color = as.factor(knn_clusters + 1)), pch = 20) +
  ggtitle("KNN Decision Boundaries") +
  labs(color = "Cluster") +
  theme_minimal()

plot2 <- ggplot(train_data, aes(x = customer[, 1], y = customer[, 2], color = as.factor(Kmeans_Cluster))) +
  geom_point() +
  geom_point(data = test_data, aes(x = test_data[, 1], y = test_data[, 2], color = as.factor(rf_clusters + 1)), pch = 20) +
  ggtitle("Random Forest Decision Boundaries") +
  labs(color = "Cluster") +
  theme_minimal()

# Display the plots side by side
grid.arrange(plot1, plot2, ncol = 2)

# Print feature importance for Random Forest
importance(rf_model)

# Plot Random Forest variable importance
varImpPlot(rf_model, main = "Random Forest Variable Importance")
