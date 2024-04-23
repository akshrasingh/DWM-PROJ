# Load required libraries
library(tidyverse)
library(randomForest)
library(cluster)
library(caret)
library(ggplot2)
library(gridExtra)

# Set seed for reproducibility
set.seed(123)

# Load the data
stroke_data <- read.csv("C:\Users\charv\Downloads\stroke_data.csv")

# Data preprocessing
# Drop ID column
stroke_data <- stroke_data[-1]

# Convert categorical variables to factors
stroke_data$gender <- as.factor(stroke_data$gender)
stroke_data$ever_married <- as.factor(stroke_data$ever_married)
stroke_data$work_type <- as.factor(stroke_data$work_type)
stroke_data$Residence_type <- as.factor(stroke_data$Residence_type)
stroke_data$smoking_status <- as.factor(stroke_data$smoking_status)

# Handle missing values
stroke_data[is.na(stroke_data)] <- 0  # Replace NA with 0 for simplicity

# Split the data into training and testing sets
set.seed(123) # For reproducibility
train_index <- sample(1:nrow(stroke_data), 0.7 * nrow(stroke_data))
train_data <- stroke_data[train_index, ]
test_data <- stroke_data[-train_index, ]

# Define target variable
target <- "stroke"

# Random Forest Implementation Steps:

# Set parameters
n <- nrow(train_data)
p <- ncol(train_data) - 1  # Exclude target variable
m <- round(sqrt(p))  # Square root of total features is often used
ntree <- 500  # Number of trees

# Initialize an empty list to store decision trees
trees <- list()

# Train the Random Forest model
for (i in 1:ntree) {
  # Random Sampling
  bootstrap_sample <- sample(1:n, replace = TRUE, size = n)
  
  # Random Feature Selection
  selected_features <- sample(1:p, size = m)
  
  # Decision Tree Construction
  tree <- randomForest(
    formula = as.factor(stroke) ~ .,
    data = train_data[bootstrap_sample, selected_features],
    ntree = 1,
    importance = FALSE,
    do.trace = FALSE
  )
  
  # Store the decision tree
  trees[[i]] <- tree
}

# Ensemble Prediction (Voting)
rf_pred <- matrix(0, nrow = nrow(test_data), ncol = ntree)
for (i in 1:ntree) {
  rf_pred[, i] <- predict(trees[[i]], newdata = test_data)
}
rf_pred <- apply(rf_pred, 1, function(x) {
  as.factor(names(sort(table(x), decreasing = TRUE)[1]))
})

# Evaluate the Random Forest model
accuracy_rf <- mean(rf_pred == test_data[[target]])
cat("Accuracy of Random Forest model:", accuracy_rf, "\n")

# Davies-Bouldin Index for Random Forest
rf_clusters <- as.integer(rf_pred)
db_rf <- cluster.stats(dist(test_data[, 1:2]), rf_clusters)$dunn
cat("Davies-Bouldin Index for Random Forest:", db_rf, "\n")

# Confusion Matrix for Random Forest
confusionMatrix(rf_pred, test_data[[target]])

# Plot Random Forest variable importance
varImpPlot(randomForest(as.factor(stroke) ~ ., data = train_data, ntree = ntree, importance = TRUE), main = "Random Forest Variable Importance")

# Plot decision boundaries for Random Forest
plot2 <- ggplot(train_data, aes(x = train_data[, 1], y = train_data[, 2], color = as.factor(Kmeans_Cluster))) +
  geom_point() +
  geom_point(data = test_data, aes(x = test_data[, 1], y = test_data[, 2], color = as.factor(rf_clusters + 1)), pch = 20) +
  ggtitle("Random Forest Decision Boundaries") +
  labs(color = "Cluster") +
  theme_minimal()

# Display the plots side by side
grid.arrange(plot2, ncol = 1)

#KNN
# Install and load required packages
install.packages(c("plotly", "tensorflow", "pROC", "tidyverse", "naniar", "caTools", "caret", "ROSE", "reshape2", "magrittr", "dplyr", "keras"))

# Importing packages
library(plotly)
library(tensorflow)
library(pROC)
library(tidyverse)
library(naniar)
library(caTools) 
library(caret)
library(ROSE)
library(reshape2)
library(magrittr)
library(dplyr)
library(keras)
library(class)


#reading the csv file
data = read.csv("C:/Users/Asus/Desktop/stroke_data.csv")
str(data)


# Converting character values to numeric values

clean_data <- data %>% mutate(gender = if_else(gender == "Female", 0, if_else(gender == "Male", 1, 2)), ever_married = if_else(ever_married == "Yes", 1, 0), Residence_type = if_else(Residence_type == "Rural", 0, 1), smoking_status = if_else(smoking_status == "never smoked", 0, if_else(smoking_status == "formerly smoked", 1, if_else(smoking_status == "smokes", 2, 3))))
summary(clean_data)
glimpse(clean_data)

# Handling missing values

miss_scan_count(data = data, search = list("N/A", "Unknown"))

# There are 201 "N/A" values in the bmi column that likely caused this column 
# to be parsed as character, although it should be numerical.   
#  replacing those values with actual NAs. 
# lot of "Unknown" values in smoking_status  
#  We see that we have 1544 unknown values for smoking status and 
# replace those values with 
# NAs.

clean_data <- replace_with_na(data = clean_data, replace = list(bmi = c("N/A"), smoking_status = c(3))) %>% mutate(bmi = as.numeric(bmi))

# Split the data into training and testing sets (e.g., 80% training, 20% testing)
set.seed(99)
index <- createDataPartition(clean_data$stroke, p = 0.8, list = FALSE)
train_data <- clean_data[index, ]
test_data <- clean_data[-index, ]

# Normalize numerical features
scaler <- preProcess(train_data[c("age", "avg_glucose_level", "bmi")], method = c("center", "scale"))
train_data[c("age", "avg_glucose_level", "bmi")] <- predict(scaler, train_data[c("age", "avg_glucose_level", "bmi")])
test_data[c("age", "avg_glucose_level", "bmi")] <- predict(scaler, test_data[c("age", "avg_glucose_level", "bmi")])

# Handle missing values (if any)
train_data[is.na(train_data)] <- 0
test_data[is.na(test_data)] <- 0
# Update the input shape in your LSTM layer to match your data


# Train the KNN model
knn_model <- knn(train = train_data[, -10], test = test_data[, -10], cl = train_data$stroke, k = 100)
# Convert predictions to binary
knn_predictions_binary <- ifelse(knn_model == "Yes", 1, 0)

# Calculate accuracy
accuracy <- mean(knn_predictions_binary == test_data$stroke)
print(paste("Accuracy of KNN model:", round(accuracy * 100, 2), "%"))

### Graphs:

# Scatter Plot
scatter_plot <- plot_ly(train_data, x = ~age, y = ~avg_glucose_level, type = "scatter", mode = "markers", color = ~factor(stroke)) %>%
  layout(title = "Scatter Plot of Age vs Avg Glucose Level",
         xaxis = list(title = "Age"),
         yaxis = list(title = "Avg Glucose Level"),
         showlegend = TRUE)

# Histogram
histogram <- plot_ly(train_data, x = ~bmi, type = "histogram", marker = list(color = "#636EFA")) %>%
  layout(title = "Histogram of BMI",
         xaxis = list(title = "BMI"),
         yaxis = list(title = "Frequency"))

### Confusion Matrix:

# Confusion matrix for KNN model
conf_matrix <- table(Actual = test_data$stroke, Predicted = knn_predictions_binary)

# Plotting confusion matrix
plot_confusion_matrix <- function(conf_matrix) {
  conf_matrix %>%
    as.data.frame() %>%
    plot_ly(x = ~Actual, y = ~Predicted, z = ~Freq, type = "heatmap", colors = c("#FFA07A", "#20B2AA")) %>%
    layout(title = "Confusion Matrix",
           xaxis = list(title = "Actual"),
           yaxis = list(title = "Predicted"))
}

plot_confusion_matrix(conf_matrix)

# Importing packages
library(plotly)
library(tensorflow)
library(xgboost)
library(pROC)
library(tidyverse)
library(naniar)
library(caTools) 
library(ggplot2) 
library(superheat) 
library(scatterplot3d) 
library(ROCR)
library(Metrics)
library(keras)
library(caret)
library(ROSE)
library(reshape2)
library(magrittr)
library(dplyr)
library(keras)


#reading the csv file
data = read.csv("healthcare-dataset-stroke-data.xlsx")
str(data)


# Converting character values to numeric values

clean_data <- data %>% mutate(gender = if_else(gender == "Female", 0, if_else(gender == "Male", 1, 2)), ever_married = if_else(ever_married == "Yes", 1, 0), Residence_type = if_else(Residence_type == "Rural", 0, 1), smoking_status = if_else(smoking_status == "never smoked", 0, if_else(smoking_status == "formerly smoked", 1, if_else(smoking_status == "smokes", 2, 3))))
summary(clean_data)
glimpse(clean_data)

# Handling missing values

miss_scan_count(data = data, search = list("N/A", "Unknown"))

# There are 201 "N/A" values in the bmi column that likely caused this column 
# to be parsed as character, although it should be numerical.   
#  replacing those values with actual NAs. 
# lot of "Unknown" values in smoking_status  
#  We see that we have 1544 unknown values for smoking status and 
# replace those values with 
# NAs.

clean_data <- replace_with_na(data = clean_data, replace = list(bmi = c("N/A"), smoking_status = c(3))) %>% mutate(bmi = as.numeric(bmi))

with(clean_data, {
  colors <- ifelse(stroke == 1, "red", "blue")  # Assuming stroke = 1 for stroke cases and 0 for non-stroke cases
  scatterplot3d(
    x = age,  y = hypertension, z = avg_glucose_level,
    color = colors,
    main = "Stroke Prediction Scatterplot",
    xlab = "Age", ylab = "Hypertension", zlab = "Average Glucose Level",
    xlim = c(min(age), max(age)),
    ylim = c(0, 1), # Assuming hypertension is a binary variable (0 or 1)
    zlim = c(min(avg_glucose_level), max(avg_glucose_level))
  )
})



# Split the data into training and testing sets (e.g., 80% training, 20% testing)
set.seed(99)
index <- createDataPartition(clean_data$stroke, p = 0.8, list = FALSE)
train_data <- clean_data[index, ]
test_data <- clean_data[-index, ]

# Normalize numerical features
scaler <- preProcess(train_data[c("age", "avg_glucose_level", "bmi")], method = c("center", "scale"))
train_data[c("age", "avg_glucose_level", "bmi")] <- predict(scaler, train_data[c("age", "avg_glucose_level", "bmi")])
test_data[c("age", "avg_glucose_level", "bmi")] <- predict(scaler, test_data[c("age", "avg_glucose_level", "bmi")])

# Handle missing values (if any)
train_data[is.na(train_data)] <- 0
test_data[is.na(test_data)] <- 0

# Define the neural network model
model <- keras_model_sequential() %>%
  layer_dense(units = 256, activation = 'relu', input_shape = ncol(train_data) - 1) %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 256, activation = 'relu') %>%
  layer_dense(units = 256, activation = 'relu') %>%
  layer_dense(units = 256, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1, activation = 'sigmoid')

# Compile the model
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_adam(),
  metrics = c('accuracy')
)

# Train the model
history <- model %>% fit(
  x = as.matrix(train_data[, -which(names(train_data) == "stroke")]),
  y = as.matrix(train_data$stroke),
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2
)

# Evaluate the model on the test data
predictions <- model %>% predict(as.matrix(test_data[, -which(names(test_data) == "stroke")])) %>%
  k_argmax()

# Convert predictions to binary (0 or 1) based on threshold 0.5
predictions <- as.integer(predictions > 0.5)

# Compute confusion matrix
conf_matrix <- table(Actual = test_data$stroke, Predicted = predictions)
conf_matrix

library(ggplot2)

# Convert confusion matrix to data frame
conf_matrix_df <- as.data.frame(as.table(conf_matrix))

# Rename columns
names(conf_matrix_df) <- c("Actual", "Predicted", "Count")

library(ggplot2)

# Sample confusion matrix data frame (replace this with your actual data)
conf_matrix_df <- data.frame(
  Actual = c("True", "True", "False", "False"),
  Predicted = c("True", "False", "True", "False"),
  Count = c(500, 0, 56, 250)
)

ggplot(conf_matrix_df, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(Actual == "True" & Predicted == "True", paste0("TP: ", Count),
                               ifelse(Actual == "True" & Predicted == "False", paste0("FN: ", Count),
                                      ifelse(Actual == "False" & Predicted == "True", paste0("FP: ", Count),
                                             ifelse(Actual == "False" & Predicted == "False", paste0("TN: ", Count), ""))))), 
            vjust = 1) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  theme_minimal() +
  labs(title = "Confusion Matrix",
       x = "Actual",
       y = "Predicted")

# Compute accuracy from confusion matrix
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy
cat("The accuracy of the Neural Network model is:", round(accuracy * 100, 2), "%")

# Predict probabilities on the test data
probabilities <- model %>% predict(as.matrix(test_data[, -which(names(test_data) == "stroke")]))[, 1]

# BMI Distribution Density Plot

ggplot(clean_data, aes(x = bmi)) + geom_density(color = "black", fill = "lightblue") + labs(title = "Distribution of BMI") 

# Gender Distribution Bar Plot

ggplot(data, aes(x = factor(gender), fill = factor(gender))) + geom_bar() + theme_classic()

# Age and BMI wrt Stroke Scatter Plot

ggplot(clean_data, aes(x = age, y = bmi, color = stroke)) + geom_point() + scale_color_gradient(low = "lightblue", high = "red")



# Avg Glucose Level with stroke boxplot

ggplot(clean_data, aes(x = stroke, y = avg_glucose_level, group = stroke, fill = stroke)) + geom_boxplot()
glimpse(clean_data)



par(las = 2)
par(las=1)
par(mar = c(5, 9, 4, 4))  

correlation_df <- data.frame(variable = names(correlations), correlation = correlations)

correlation_df$variable <- ifelse(correlation_df$variable == "avg_glucose_level", "avg_glucose", correlation_df$variable)
barplot(correlation_df$correlation, names.arg = correlation_df$variable, col = ifelse(correlation_df$correlation > 0, "blue", "lightcoral"), horiz = TRUE, main = "Correlation with Stroke", xlab = "Correlation Coefficient", ylab = "",xlim = c(0, 1),border = NA)
abline(v = 0, col = "black", lty = 1, lwd = 2)

mtext("Variables", side = 2, line = 7.5, at = 5, cex = 1,las = 3)

legend("topright", legend = c("Positive", "Negative"), fill = c("blue", "lightcoral"))

with(clean_data, {
  colors <- ifelse(stroke == 1, "red", "blue")  # Assuming stroke = 1 for stroke cases and 0 for non-stroke cases
  scatterplot3d(
    x = age,  y = hypertension, z = avg_glucose_level,
    color = colors,
    main = "Stroke Prediction Scatterplot",
    xlab = "Age", ylab = "Hypertension", zlab = "Average Glucose Level",
    xlim = c(min(age), max(age)),
    ylim = c(0, 1), # Assuming hypertension is a binary variable (0 or 1)
    zlim = c(min(avg_glucose_level), max(avg_glucose_level))
  )
})



