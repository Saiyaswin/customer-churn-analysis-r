# ============================================================
# WEEK 4 - MODEL BUILDING AND PREDICTIVE ANALYSIS
# Dataset: Iris Dataset
# Language: R
# ============================================================

# ------------------------------------------------------------
# 1. INSTALL / LOAD REQUIRED PACKAGES
# ------------------------------------------------------------

# Run these install commands only if a package is not installed.
# install.packages("caret")
# install.packages("rpart")
# install.packages("MASS")
# install.packages("pROC")

library(tidyverse)
library(caret)
library(rpart)
library(MASS)
library(pROC)

# ------------------------------------------------------------
# 2. LOAD THE IRIS DATASET
# ------------------------------------------------------------

data(iris)

# Create a working copy
iris_data <- iris

# View the first few rows
head(iris_data)

# Check the structure
str(iris_data)

# Check dimensions
dim(iris_data)

# ------------------------------------------------------------
# 3. CREATE A BINARY TARGET VARIABLE
# ------------------------------------------------------------

# Setosa = Yes
# Other species = No

iris_data$Target <- ifelse(
  iris_data$Species == "setosa",
  "Yes",
  "No"
)

# Convert Target into a factor
iris_data$Target <- as.factor(iris_data$Target)

# Check the target distribution
table(iris_data$Target)

# View the updated dataset
head(iris_data)

# ------------------------------------------------------------
# 4. TRAIN-TEST SPLIT
# ------------------------------------------------------------

# Set seed for reproducibility
set.seed(123)

# Create an 80:20 train-test split
train_index <- createDataPartition(
  iris_data$Target,
  p = 0.80,
  list = FALSE
)

# Create training and testing datasets
train_data <- iris_data[train_index, ]
test_data <- iris_data[-train_index, ]

# Check dimensions
dim(train_data)
dim(test_data)

# Check target distribution
table(train_data$Target)
table(test_data$Target)

# ------------------------------------------------------------
# 5. MODEL 1: LOGISTIC REGRESSION
# ------------------------------------------------------------

logistic_model <- glm(
  Target ~ Sepal.Length + Sepal.Width +
    Petal.Length + Petal.Width,
  data = train_data,
  family = binomial
)

# Display model summary
summary(logistic_model)

# Note:
# Depending on the split and data separation, logistic regression
# may produce convergence/separation warnings. The final comparison
# below uses the Decision Tree and LDA models from this workflow.

# ------------------------------------------------------------
# 6. MODEL 2: DECISION TREE
# ------------------------------------------------------------

tree_model <- rpart(
  Target ~ Sepal.Length + Sepal.Width +
    Petal.Length + Petal.Width,
  data = train_data,
  method = "class"
)

# Display model
print(tree_model)

# Model summary
summary(tree_model)

# ------------------------------------------------------------
# 7. DECISION TREE PREDICTIONS
# ------------------------------------------------------------

tree_pred <- predict(
  tree_model,
  test_data,
  type = "class"
)

# Display first predictions
head(tree_pred)

# Create confusion matrix
tree_cm <- table(
  Actual = test_data$Target,
  Predicted = tree_pred
)

tree_cm

# ------------------------------------------------------------
# 8. DECISION TREE PERFORMANCE METRICS
# ------------------------------------------------------------

TP <- tree_cm["Yes", "Yes"]
TN <- tree_cm["No", "No"]
FP <- tree_cm["No", "Yes"]
FN <- tree_cm["Yes", "No"]

accuracy <- (TP + TN) / sum(tree_cm)

precision <- ifelse(
  (TP + FP) == 0,
  0,
  TP / (TP + FP)
)

recall <- ifelse(
  (TP + FN) == 0,
  0,
  TP / (TP + FN)
)

f1_score <- ifelse(
  (precision + recall) == 0,
  0,
  2 * precision * recall / (precision + recall)
)

cat("Accuracy :", round(accuracy, 4), "\n")
cat("Precision:", round(precision, 4), "\n")
cat("Recall   :", round(recall, 4), "\n")
cat("F1 Score :", round(f1_score, 4), "\n")

# ------------------------------------------------------------
# 9. MODEL 3: LINEAR DISCRIMINANT ANALYSIS (LDA)
# ------------------------------------------------------------

lda_model <- lda(
  Target ~ Sepal.Length + Sepal.Width +
    Petal.Length + Petal.Width,
  data = train_data
)

# Display model
lda_model

# ------------------------------------------------------------
# 10. LDA PREDICTIONS
# ------------------------------------------------------------

lda_pred <- predict(
  lda_model,
  test_data
)

# View first predictions
head(lda_pred$class)

# Create confusion matrix
lda_cm <- table(
  Actual = test_data$Target,
  Predicted = lda_pred$class
)

lda_cm

# ------------------------------------------------------------
# 11. LDA PERFORMANCE METRICS
# ------------------------------------------------------------

TP <- lda_cm["Yes", "Yes"]
TN <- lda_cm["No", "No"]
FP <- lda_cm["No", "Yes"]
FN <- lda_cm["Yes", "No"]

accuracy_lda <- (TP + TN) / sum(lda_cm)

precision_lda <- ifelse(
  (TP + FP) == 0,
  0,
  TP / (TP + FP)
)

recall_lda <- ifelse(
  (TP + FN) == 0,
  0,
  TP / (TP + FN)
)

f1_lda <- ifelse(
  (precision_lda + recall_lda) == 0,
  0,
  2 * precision_lda * recall_lda /
    (precision_lda + recall_lda)
)

cat("LDA Accuracy :", round(accuracy_lda, 4), "\n")
cat("LDA Precision:", round(precision_lda, 4), "\n")
cat("LDA Recall   :", round(recall_lda, 4), "\n")
cat("LDA F1 Score :", round(f1_lda, 4), "\n")

# ------------------------------------------------------------
# 12. DECISION TREE ROC AND AUC
# ------------------------------------------------------------

# Get predicted probabilities
tree_prob <- predict(
  tree_model,
  test_data,
  type = "prob"
)

head(tree_prob)

# Create ROC curve
tree_roc <- roc(
  test_data$Target,
  tree_prob[, "Yes"],
  levels = c("No", "Yes"),
  direction = "<"
)

# Calculate AUC
tree_auc <- auc(tree_roc)

cat(
  "Decision Tree AUC:",
  round(as.numeric(tree_auc), 4),
  "\n"
)

# Plot ROC curve
plot(
  tree_roc,
  main = "Decision Tree ROC Curve",
  col = "blue",
  lwd = 2
)

abline(a = 0, b = 1, lty = 2)

legend(
  "bottomright",
  legend = paste(
    "AUC =",
    round(as.numeric(tree_auc), 4)
  ),
  lwd = 2
)

# ------------------------------------------------------------
# 13. LDA ROC AND AUC
# ------------------------------------------------------------

# Get predicted probabilities
lda_prob <- predict(
  lda_model,
  test_data
)$posterior

head(lda_prob)

# Create ROC curve
lda_roc <- roc(
  test_data$Target,
  lda_prob[, "Yes"],
  levels = c("No", "Yes"),
  direction = "<"
)

# Calculate AUC
lda_auc <- auc(lda_roc)

cat(
  "LDA AUC:",
  round(as.numeric(lda_auc), 4),
  "\n"
)

# Plot ROC curve
plot(
  lda_roc,
  main = "LDA ROC Curve",
  col = "blue",
  lwd = 2
)

abline(a = 0, b = 1, lty = 2)

legend(
  "bottomright",
  legend = paste(
    "AUC =",
    round(as.numeric(lda_auc), 4)
  ),
  lwd = 2
)

# ------------------------------------------------------------
# 14. FINAL MODEL COMPARISON TABLE
# ------------------------------------------------------------

model_comparison <- data.frame(
  Model = c("Decision Tree", "LDA"),
  Accuracy = c(accuracy, accuracy_lda),
  Precision = c(precision, precision_lda),
  Recall = c(recall, recall_lda),
  F1_Score = c(f1_score, f1_lda),
  AUC = c(
    as.numeric(tree_auc),
    as.numeric(lda_auc)
  )
)

print(model_comparison)

# ------------------------------------------------------------
# 15. MODEL PERFORMANCE COMPARISON PLOT
# ------------------------------------------------------------

comparison_matrix <- rbind(
  "Decision Tree" = c(
    accuracy,
    precision,
    recall,
    f1_score
  ),
  "LDA" = c(
    accuracy_lda,
    precision_lda,
    recall_lda,
    f1_lda
  )
)

barplot(
  comparison_matrix,
  beside = TRUE,
  names.arg = c(
    "Accuracy",
    "Precision",
    "Recall",
    "F1 Score"
  ),
  ylim = c(0, 1.15),
  main = "Decision Tree vs LDA - Model Performance",
  ylab = "Performance Score",
  xlab = "Evaluation Metrics",
  legend.text = rownames(comparison_matrix),
  args.legend = list(
    x = "bottomright",
    bty = "n"
  )
)

# ------------------------------------------------------------
# 16. OPTIONAL: SAVE MODEL COMPARISON TABLE
# ------------------------------------------------------------

# Uncomment the following line if you want a CSV file for GitHub.
# write.csv(
#   model_comparison,
#   "model_comparison.csv",
#   row.names = FALSE
# )

# ------------------------------------------------------------
# END OF WEEK 4 ANALYSIS
# ------------------------------------------------------------
