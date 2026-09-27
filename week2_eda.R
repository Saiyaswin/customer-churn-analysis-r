# Week 2 - Exploratory Data Analysis and Visualization
# Dataset: Fisher / Anderson Iris dataset

# Install packages once if needed:
# install.packages(c("tidyverse", "corrplot"))

library(tidyverse)
library(corrplot)

# 1. Load the public dataset included with R
data(iris)
iris_data <- iris

# 2. Basic inspection
head(iris_data)
str(iris_data)
dim(iris_data)
summary(iris_data)

# 3. Missing-value check
colSums(is.na(iris_data))

# 4. Check duplicates
sum(duplicated(iris_data))

# 5. Prepare the data
iris_data <- iris_data %>%
  mutate(Species = as.factor(Species))

# 6. Summary statistics
iris_data %>%
  summarise(
    Mean_Sepal_Length = mean(Sepal.Length),
    Mean_Sepal_Width  = mean(Sepal.Width),
    Mean_Petal_Length = mean(Petal.Length),
    Mean_Petal_Width  = mean(Petal.Width)
  )

# 7. Species counts
count(iris_data, Species)

# 8. Histogram
ggplot(iris_data, aes(x = Petal.Length)) +
  geom_histogram(bins = 10, color = "black") +
  labs(title = "Distribution of Petal Length",
       x = "Petal Length (cm)", y = "Number of Flowers") +
  theme_minimal()

# 9. Scatter plot
ggplot(iris_data, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point(size = 2.5) +
  labs(title = "Petal Length vs Petal Width",
       x = "Petal Length (cm)", y = "Petal Width (cm)") +
  theme_minimal()

# 10. Box plot
ggplot(iris_data, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot() +
  labs(title = "Sepal Length by Iris Species",
       x = "Species", y = "Sepal Length (cm)") +
  theme_minimal()

# 11. Species count bar chart
ggplot(iris_data, aes(x = Species)) +
  geom_bar() +
  labs(title = "Number of Observations by Species",
       x = "Species", y = "Count") +
  theme_minimal()

# 12. Correlation matrix
cor_matrix <- cor(iris_data[, 1:4])
print(cor_matrix)
corrplot(cor_matrix, method = "color", addCoef.col = "black")
