library(dplyr)
library(tidyverse)  
library(factoextra)  
library(caret)
library(cluster)
library(ggplot2)

data_oulad <- read.csv("C:\\Users\\Home\\Downloads\\oulad-students.csv")

str(data_oulad)
summary(data_oulad)

data_oulad$final_result <- as.factor(data_oulad$final_result)
data_oulad$gender <- as.numeric(data_oulad$gender)
data_oulad$id_student <- as.numeric(data_oulad$id_student)


# Generate simulated learning data
set.seed(123)
n_students <- 1000
student_id <- 1:n_students
feature1 <- rnorm(n_students, mean = 50, sd = 10)
feature2 <- rnorm(n_students, mean = 70, sd = 15)
data <- data.frame(Student_ID = student_id, Feature_1 = feature1, Feature_2 = feature2)

# Perform PCA for dimensionality reduction
pca_result <- prcomp(data[,c("Feature_1", "Feature_2")], center = TRUE, scale. = TRUE)

# Plot the PCA results
fviz_eig(pca_result) + ggtitle("Scree Plot")
fviz_pca_var(pca_result) + ggtitle("Variable Loading Plot")
fviz_pca_biplot(pca_result, repel = TRUE, title = "Biplot")

# Perform K-means clustering
set.seed(123)
result_kmeans <- kmeans(pca_result$x[,1:2], centers = 4, nstart = 30)

# Plot the clustering results
fviz_cluster(result_kmeans, data[,c("Feature_1", "Feature_2")]) + ggtitle("Clustering Results")

# Add cluster labels to the original data
data$Cluster <- as.factor(result_kmeans$cluster)

# Summary of the clustering results
summary(data$Cluster)

