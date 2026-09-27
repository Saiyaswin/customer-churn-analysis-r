# Week 3 - Data Wrangling and Preprocessing
# Dataset: Fisher / Anderson Iris Dataset

# Load required package
library(tidyverse)

# Load the Iris dataset
data(iris)

# Create a working copy
iris_data <- iris

# Display the first few rows
head(iris_data)

# Check the structure
str(iris_data)

# Check dimensions
dim(iris_data)

# Summary statistics
summary(iris_data)






# Check missing values
colSums(is.na(iris_data))


# Check duplicate rows
sum(duplicated(iris_data))


# Check column names
colnames(iris_data)


# Check data types
sapply(iris_data, class)






#Step 1 — Convert Species to factor

iris_data$Species <- as.factor(iris_data$Species)


#Step 2 — Create a cleaned dataset

iris_clean <- iris_data

head(iris_clean)


#Step 3 — Check the cleaned dataset

str(iris_clean)

dim(iris_clean)



#Step 4 — Check for invalid values

summary(iris_clean[, 1:4])

colSums(iris_clean[, 1:4] <= 0)






Next: Data Wrangling


#1. Select specific columns

iris_selected <- iris_clean[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")]

head(iris_selected)



#2. Filter flowers with larger petals

large_petal <- subset(iris_clean, Petal.Length > 5)

head(large_petal)

nrow(large_petal)


#3. Sort the dataset

iris_sorted <- iris_clean[order(iris_clean$Petal.Length, decreasing = TRUE), ]

head(iris_sorted)



#4. Create a new variable


iris_clean$Petal.Size <- ifelse(iris_clean$Petal.Length < 2, "Small",
                                ifelse(iris_clean$Petal.Length < 5, "Medium", "Large"))


head(iris_clean)

table(iris_clean$Petal.Size)




#Next step — Group and summarise the data



aggregate(Petal.Length ~ Species, data = iris_clean, mean)


aggregate(Petal.Width ~ Species, data = iris_clean, mean)


aggregate(Sepal.Length ~ Species, data = iris_clean, mean)


species_summary <- aggregate(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) ~ Species,
                             data = iris_clean, mean)

species_summary



#Export the processed dataset


write.csv(iris_clean, "iris_cleaned_week3.csv", row.names = FALSE)

file.exists("iris_cleaned_week3.csv")

dim(iris_clean)

head(iris_clean)
