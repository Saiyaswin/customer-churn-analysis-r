# ============================================================
# WEEK 3 - DATA WRANGLING AND PREPROCESSING
# Dataset: Fisher / Anderson Iris Dataset
# Tool: R / RGui
#
# This file contains the R commands used during Week 3.
# The verified console outputs are kept below the commands as
# comments so the file documents both the process and results.
# ============================================================


# ------------------------------------------------------------
# 1. Load required package
# ------------------------------------------------------------

library(tidyverse)


# ------------------------------------------------------------
# 2. Load the Iris dataset
# ------------------------------------------------------------

data(iris)


# ------------------------------------------------------------
# 3. Create a working copy
# ------------------------------------------------------------

iris_data <- iris


# ------------------------------------------------------------
# 4. Initial inspection
# ------------------------------------------------------------

head(iris_data)

# OUTPUT:
#   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
# 1          5.1         3.5          1.4         0.2  setosa
# 2          4.9         3.0          1.4         0.2  setosa
# 3          4.7         3.2          1.3         0.2  setosa
# 4          4.6         3.1          1.5         0.2  setosa
# 5          5.0         3.6          1.4         0.2  setosa
# 6          5.4         3.9          1.7         0.4  setosa


# Structure
str(iris_data)

# OUTPUT:
# 'data.frame': 150 obs. of 5 variables:
#  $ Sepal.Length: num
#  $ Sepal.Width : num
#  $ Petal.Length: num
#  $ Petal.Width : num
#  $ Species     : Factor


# Dimensions
dim(iris_data)

# OUTPUT:
# [1] 150   5


# Summary statistics
summary(iris_data)

# OUTPUT:
# Sepal.Length    Sepal.Width     Petal.Length    Petal.Width
# Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100
# 1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600   1st Qu.:0.300
# Median :5.800   Median :3.000   Median :4.350   Median :1.300
# Mean   :5.843   Mean   :3.057   Mean   :3.758   Mean   :1.199
# 3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800
# Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500
#
# Species:
# setosa     :50
# versicolor :50
# virginica  :50


# ------------------------------------------------------------
# 5. Data quality checks
# ------------------------------------------------------------

# Missing values
colSums(is.na(iris_data))

# OUTPUT:
# Sepal.Length  Sepal.Width Petal.Length Petal.Width Species
# 0             0           0            0           0


# Duplicate rows
sum(duplicated(iris_data))

# OUTPUT:
# [1] 1


# Column names
colnames(iris_data)

# OUTPUT:
# [1] "Sepal.Length" "Sepal.Width" "Petal.Length"
# [4] "Petal.Width"  "Species"


# Data types
sapply(iris_data, class)

# OUTPUT:
# Sepal.Length  Sepal.Width Petal.Length Petal.Width Species
# "numeric"     "numeric"    "numeric"    "numeric"   "factor"


# ------------------------------------------------------------
# 6. Cleaned working dataset
# ------------------------------------------------------------

# Convert Species to factor
iris_data$Species <- as.factor(iris_data$Species)

# Create cleaned dataset
iris_clean <- iris_data

head(iris_clean)

# OUTPUT:
#   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
# 1          5.1         3.5          1.4         0.2  setosa
# 2          4.9         3.0          1.4         0.2  setosa
# 3          4.7         3.2          1.3         0.2  setosa
# 4          4.6         3.1          1.5         0.2  setosa
# 5          5.0         3.6          1.4         0.2  setosa
# 6          5.4         3.9          1.7         0.4  setosa


# Check cleaned dataset structure
str(iris_clean)


# Check dimensions
dim(iris_clean)

# OUTPUT:
# [1] 150   5


# Check invalid/non-positive values in numerical columns
colSums(iris_clean[, 1:4] <= 0)

# OUTPUT:
# Sepal.Length Sepal.Width Petal.Length Petal.Width
# 0            0           0            0


# ------------------------------------------------------------
# 7. Data wrangling
# ------------------------------------------------------------

# 7.1 Select specific columns

iris_selected <- iris_clean[, c(
  "Sepal.Length",
  "Sepal.Width",
  "Petal.Length",
  "Petal.Width",
  "Species"
)]

head(iris_selected)

# OUTPUT:
#   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
# 1          5.1         3.5          1.4         0.2  setosa
# 2          4.9         3.0          1.4         0.2  setosa
# 3          4.7         3.2          1.3         0.2  setosa
# 4          4.6         3.1          1.5         0.2  setosa
# 5          5.0         3.6          1.4         0.2  setosa
# 6          5.4         3.9          1.7         0.4  setosa


# 7.2 Filter flowers with larger petals

large_petal <- subset(iris_clean, Petal.Length > 5)

head(large_petal)

# OUTPUT:
#     Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
# 84           6.0         2.7          5.1         1.6 versicolor
# 101          6.3         3.3          6.0         2.5 virginica
# 102          5.8         2.7          5.1         1.9 virginica
# 103          7.1         3.0          5.9         2.1 virginica
# 104          6.3         2.9          5.6         1.8 virginica
# 105          6.5         3.0          5.8         2.2 virginica

nrow(large_petal)

# OUTPUT:
# [1] 42


# 7.3 Sort the dataset by Petal.Length in descending order

iris_sorted <- iris_clean[
  order(iris_clean$Petal.Length, decreasing = TRUE), ]

head(iris_sorted)

# OUTPUT:
#     Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
# 119          7.7         2.6          6.9         2.3 virginica
# 118          7.7         3.8          6.7         2.2 virginica
# 123          7.7         2.8          6.7         2.0 virginica
# 106          7.6         3.0          6.6         2.1 virginica
# 132          7.9         3.8          6.4         2.0 virginica
# 108          7.3         2.9          6.3         1.8 virginica


# ------------------------------------------------------------
# 8. Feature engineering - create Petal.Size
# ------------------------------------------------------------

iris_clean$Petal.Size <- ifelse(
  iris_clean$Petal.Length < 2,
  "Small",
  ifelse(iris_clean$Petal.Length < 5, "Medium", "Large")
)

head(iris_clean)

# OUTPUT:
#   Sepal.Length Sepal.Width Petal.Length Petal.Width Species Petal.Size
# 1          5.1         3.5          1.4         0.2  setosa      Small
# 2          4.9         3.0          1.4         0.2  setosa      Small
# 3          4.7         3.2          1.3         0.2  setosa      Small
# 4          4.6         3.1          1.5         0.2  setosa      Small
# 5          5.0         3.6          1.4         0.2  setosa      Small
# 6          5.4         3.9          1.7         0.4  setosa      Small


# Category counts
table(iris_clean$Petal.Size)

# OUTPUT:
# Large Medium Small
# 46    54    50


# ------------------------------------------------------------
# 9. Group and summarise the data
# ------------------------------------------------------------

aggregate(Petal.Length ~ Species, data = iris_clean, mean)

# OUTPUT:
#      Species Petal.Length
# 1     setosa        1.462
# 2 versicolor        4.260
# 3  virginica        5.552


aggregate(Petal.Width ~ Species, data = iris_clean, mean)

# OUTPUT:
#      Species Petal.Width
# 1     setosa       0.246
# 2 versicolor       1.326
# 3  virginica       2.026


aggregate(Sepal.Length ~ Species, data = iris_clean, mean)

# OUTPUT:
#      Species Sepal.Length
# 1     setosa        5.006
# 2 versicolor        5.936
# 3  virginica        6.588


# Complete species summary
species_summary <- aggregate(
  cbind(
    Sepal.Length,
    Sepal.Width,
    Petal.Length,
    Petal.Width
  ) ~ Species,
  data = iris_clean,
  mean
)

species_summary

# OUTPUT:
#      Species Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1     setosa        5.006       3.428        1.462       0.246
# 2 versicolor        5.936       2.770        4.260       1.326
# 3  virginica        6.588       2.974        5.552       2.026


# ------------------------------------------------------------
# 10. Export the processed dataset
# ------------------------------------------------------------

write.csv(
  iris_clean,
  "iris_cleaned_week3.csv",
  row.names = FALSE
)


# Verify that the file was created
file.exists("iris_cleaned_week3.csv")

# OUTPUT:
# [1] TRUE


# Verify final dimensions
dim(iris_clean)

# OUTPUT:
# [1] 150   6


# Display final dataset
head(iris_clean)

# OUTPUT:
#   Sepal.Length Sepal.Width Petal.Length Petal.Width Species Petal.Size
# 1          5.1         3.5          1.4         0.2  setosa      Small
# 2          4.9         3.0          1.4         0.2  setosa      Small
# 3          4.7         3.2          1.3         0.2  setosa      Small
# 4          4.6         3.1          1.5         0.2  setosa      Small
# 5          5.0         3.6          1.4         0.2  setosa      Small
# 6          5.4         3.9          1.7         0.4  setosa      Small


# ============================================================
# END OF WEEK 3
# ============================================================
