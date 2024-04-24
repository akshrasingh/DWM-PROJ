#installing required packages
install.packages(c("plotly", "tensorflow", "pROC", "tidyverse", "naniar", "caTools", "caret", "ROSE", "reshape2", "magrittr", "dplyr", "keras", "randomForest", "Metrics", "ggplot2", "scatterplot3d", "superheat", "ROCR", "class", "skimr", "gridExtra", "corrplot", "ggcorrplot"))
install.packages("party")

#importing packages
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
library(randomForest)
library(Metrics)
library(ggplot2)
library(skimr)
library(gridExtra)
library(corrplot)
library(ggcorrplot)
library(party)


#reading csv file
data <- read.csv("C:/Users/charv/Downloads/stroke_data.csv")

summary(data)
glimpse(data)

# Converting character values to numeric values
clean_data <- data %>% 
  mutate(gender = if_else(gender == "Female", 0, if_else(gender == "Male", 1, 2)), 
         ever_married = if_else(ever_married == "Yes", 1, 0), 
         Residence_type = if_else(Residence_type == "Rural", 0, 1), 
         smoking_status = if_else(smoking_status == "never smoked", 0, 
                                  if_else(smoking_status == "formerly smoked", 1, 
                                          if_else(smoking_status == "smokes", 2, 3))))

summary(clean_data)
glimpse(clean_data)


#scanning for missing values in the dataset ( N/A and Unknown Strings to identify missing values)
miss_scan_count(data = clean_data, search = list("N/A", "Unknown"))

#replacing N/A and Unknown values with NA
#there is a value ‘N/A’ in bmi in 201 rows.
clean_data <- replace_with_na(data = clean_data, replace = list(bmi = c("N/A"), smoking_status = c(3))) %>% 
#BMI column is in character format hence onverting it to numeric
mutate(bmi = as.numeric(bmi)) 


# Convert stroke to factor
clean_data$stroke <- as.factor(clean_data$stroke)

#oversampling data to balance the dataset
oversampled_data <- ovun.sample(stroke ~ ., data = clean_data, method = "over", N = nrow(clean_data), seed = 1234)$data


#splitting the data into training and testing sets
set.seed(99)
index <- createDataPartition(oversampled_data$stroke, p = 0.8, list = FALSE)
train_data <- oversampled_data[index, ]
test_data <- oversampled_data[-index, ]

#normalization of numerical features 
scaler <- preProcess(train_data[c("age", "avg_glucose_level", "bmi")], method = c("center", "scale"))
train_data[c("age", "avg_glucose_level", "bmi")] <- predict(scaler, train_data[c("age", "avg_glucose_level", "bmi")])
test_data[c("age", "avg_glucose_level", "bmi")] <- predict(scaler, test_data[c("age", "avg_glucose_level", "bmi")])

#handling missing values
train_data[is.na(train_data)] <- 0
test_data[is.na(test_data)] <- 0

# Random Forest classification model
rf_mod <- randomForest(formula = stroke ~ ., data = train_data, ntree = 100)

# Predictions
rf_predictions <- predict(rf_mod, test_data)
rf_predictions_prob <- predict(rf_mod, test_data, type = "prob")
rf_predictions_binary <- ifelse(rf_predictions_prob[,2] > 0.5, 1, 0)

# Calculate confusion matrix
conf_matrix <- confusionMatrix(rf_predictions, test_data$stroke)
print(conf_matrix)

# Extract and print accuracy
accuracy <- conf_matrix$overall['Accuracy']
precision <- conf_matrix$byClass['Pos Pred Value']
recall <- conf_matrix$byClass['Sensitivity']
f1_score <- 2 * (precision * recall) / (precision + recall)
print(paste("Accuracy of Random Forest model:", round(accuracy * 100, 2), "%"))
print(paste("Precision:", round(precision, 3)))
print(paste("Recall:", round(recall, 2)))
print(paste("F1 Score:", round(f1_score, 2)))

# Plotting confusion matrix with TP and TN values
plot_confusion_matrix <- function(conf_matrix) {
  TP <- conf_matrix$table[2, 2]
  TN <- conf_matrix$table[1, 1]
  FP <- conf_matrix$table[1, 2]
  FN <- conf_matrix$table[2, 1]
  
  conf_matrix_df <- data.frame(
    Reference = rep(row.names(conf_matrix$table), ncol(conf_matrix$table)),
    Prediction = rep(colnames(conf_matrix$table), each = nrow(conf_matrix$table)),
    Freq = as.vector(conf_matrix$table),
    TP = ifelse(row.names(conf_matrix$table) == "1", conf_matrix$table[2, 2], 0),
    TN = ifelse(colnames(conf_matrix$table) == "1", conf_matrix$table[1, 1], 0),
    FP = ifelse(row.names(conf_matrix$table) == "1", conf_matrix$table[1, 2], 0),
    FN = ifelse(colnames(conf_matrix$table) == "1", conf_matrix$table[2, 1], 0)
  )
  
  plot_ly(data = conf_matrix_df, 
          x = ~Reference, 
          y = ~Prediction, 
          z = ~Freq, 
          type = "heatmap", 
          colors = c("#FFA07A", "#20B2AA")) %>%
    add_annotations(
      text = ~paste("Frequency: ", Freq),
      x = ~Reference,
      y = ~Prediction,
      showarrow = FALSE
    ) %>%
    layout(title = "Confusion Matrix",
           xaxis = list(title = "Actual"),
           yaxis = list(title = "Prediction"))
}

plot_confusion_matrix(conf_matrix)
# ROC curve
roc_curve <- roc(test_data$stroke, as.numeric(rf_predictions_prob[,2]))
roc_auc <- auc(roc_curve)

# Plotting ROC curve
plot(roc_curve, main = "ROC Curve for Random Forest", col = "blue", lwd = 2, print.auc = TRUE)

p1<-ggplot(data,aes(x=gender,fill=gender))+geom_bar(col="black")+geom_text(aes(label=..count..),stat = "Count", vjust= 1.5)+ggtitle("Gender Distribution")
p2<-ggplot(clean_data,aes(x=" ",fill=hypertension,group=stroke))+geom_bar(position = "fill",col="black")+coord_polar("y", start=0)+ggtitle("Distribution of Hypertension") + scale_fill_manual(values = c("#FFA07A", "#20B2AA"))
p3<-ggplot(clean_data,aes(x="",fill=heart_disease))+geom_bar(position = "fill", col="black")+coord_polar("y")+ggtitle("Distribution of Heart Disease")
p4<-ggplot(df_stroke,aes(x=ever_married,fill=ever_married))+geom_bar(col="black")+geom_text(aes(label=..count..),stat = "Count", vjust= 1.5)+ggtitle("Marriage Status")
p5<-ggplot(df_stroke,aes(x="",fill=Residence_type))+geom_bar(position = "fill")+coord_polar("y", start = 0)+ggtitle("Distribution of Residence Type")
p6<-ggplot(df_stroke,aes(x="",fill=stroke))+geom_bar(position = "fill")+coord_polar("y", start = 0)+ggtitle("Distribution of Stroke occurence")
grid.arrange(p1,p2,p3,p4,p5,p6, ncol=2)

p8 <- ggplot(data, aes(x=age, fill=stroke)) +geom_histogram(binwidth=5, alpha=0.7) +ggtitle("Age Distribution by Stroke") +theme_minimal()
p8

p9 <- ggplot(data, aes(x=avg_glucose_level, y=bmi, color=stroke)) +geom_point(alpha=0.7) +ggtitle("Avg Glucose Level vs BMI") +theme_minimal()
p9


# Feature Importance
importance <- importance(rf_mod)
varImportance <- data.frame(Variables = rownames(importance), Importance = round(importance[ , 'MeanDecreaseGini'], 2))
varImportance <- varImportance[order(-varImportance$Importance), ]
ggplot(varImportance, aes(x = reorder(Variables, Importance), y = Importance)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Variable Importance Plot (Gini impurity)", x = "Variables", y = "Importance")

# Feature Importance
importance <- importance(rf_mod)
varImportance <- data.frame(Variables = rownames(importance), Importance = round(importance[ , 'MeanDecreaseGini'], 2))
varImportance <- varImportance[order(-varImportance$Importance), ]

# Top 5 major risk factors
major_risk_factors <- head(varImportance, 5)
print("Major Risk Factors")
print(major_risk_factors)



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



