#---------------------------------------------------------------#
#
# Purpose : Project 2 - Clustering 
# 
#
# Course : Statistical Machine Learning
#
# Author : Tsadimas Anargyros - f3612318
#
# Professor : D.Karlis
#
# Date : June 2024
#
#---------------------------------------------------------------#

# Install necessary packages
# Define the list of packages
packs <- c("readxl", "dplyr", "ggplot2", "cluster", "gridExtra", "reshape2", "factoextra", "mclust", "purrr", "tidyr")

# Loop through the packages
for (pkg in packs) {
  # Check if the package is installed
  if (!require(pkg, character.only = TRUE)) {
    # Install the package if it is not installed
    install.packages(pkg, dependencies = TRUE)
    # Load the package
    library(pkg, character.only = TRUE)
  }
}


getwd()
# Set working directory (update the path as needed)
setwd("C:/MsC/Statistical Machine Learning/Statistical_Machine_Learing_Project_2")

# Load the data
Sys.setlocale(category = "LC_ALL", locale = "Greek")
file_path <- "C:/MsC/Statistical Machine Learning/Statistical_Machine_Learing_Project_2/greek data.xls"
data <- read_excel(file_path, skip = 3)

# Remove the first column
data <- data[, -1]

# Define column names
zip_codes <- "Zip Codes"
municipalities <- "Municipalities"
sum_both_genders <- "Both Genders Sum"
both_genders_cols <- c("Both 0-14", "Both 15-24", "Both 25-39", "Both 40-54", "Both 55-64", "Both 65-79", "Both over 80")
sum_males <- "Males Sum"
male_genders_cols <- c("Males 0-14", "Males 15-24", "Males 25-39", "Males 40-54", "Males 55-64", "Males 65-79", "Males over 80")
sum_females <- "Females Sum"
female_genders_cols <- c("Females 0-14", "Females 15-24", "Females 25-39", "Females 40-54", "Females 55-64", "Females 65-79", "Females over 80")
colnames(data) <- c(zip_codes, municipalities, sum_both_genders, both_genders_cols, sum_males, male_genders_cols, sum_females, female_genders_cols)

# Remove the first row
data <- data[-1,]

# Convert relevant columns to numeric
data <- data %>%
  mutate_at(vars(contains("Sum"), contains("0-14"), contains("15-24"), contains("25-39"), contains("40-54"), contains("55-64"), contains("65-79"), contains("over 80")), as.numeric)

# Summary of data
str(data)
summary(data)
colSums(is.na(data))
sapply(data, class)

# Filter data for municipalities containing "ΔΗΜΟΣ"
subset_data <- data %>%
  filter(grepl("ΔΗΜΟΣ", Municipalities))

# Exclude non-relevant columns for clustering
# and convert to frequencies
subset_data = subset(subset_data,select = c(`Both 0-14`, `Both 15-24`,`Both 25-39`,
                                            `Both 40-54`,`Both 55-64`, `Both 65-79`,
                                            `Both over 80`,`Both Genders Sum`))
total_population<-subset_data$`Both Genders Sum`

subset_data <- subset_data %>%
  mutate(`Both 0-14` = `Both 0-14` / total_population,
         `Both 15-24` = `Both 15-24` / total_population,
         `Both 25-39` = `Both 25-39` / total_population,
         `Both 40-54` = `Both 40-54` / total_population,
         `Both 55-64` = `Both 55-64` / total_population,
         `Both 65-79` = `Both 65-79` / total_population,
         `Both over 80` = `Both over 80` / total_population)
subset_data<-subset(subset_data,select = c(`Both 0-14`, `Both 15-24`,`Both 25-39`,
                                           `Both 40-54`,`Both 55-64`, `Both 65-79`,
                                           `Both over 80`))
colSums(is.na(subset_data))
summary(subset_data)

x<-subset_data$`Both over 80`
subset_data<-subset(subset_data,select = c(`Both 0-14`, `Both 15-24`,`Both 25-39`,
                                           `Both 40-54`,`Both 55-64`, `Both 65-79`))
# Finding distance between clusters
# Mahalanobis Distance
x0 <- subset_data
dM2 <- as.dist(apply(x0, 1, function(i) mahalanobis(x0, i, cov = cov(x0))))


# Print distance matrix
print(dM2)

# Hierarchical clustering with Mahalanobis distance
hc_mahalanobis <- hclust(dM2, method = "ward.D2")

# Determine cluster assignments
clusters_mahalanobis <- cutree(hc_mahalanobis, k = 4)

# Calculate silhouette scores for Mahalanobis distance
silhouette_mahalanobis <- silhouette(as.numeric(cutree(hclust(dM2), k = 4)), dM2)

# Perform PCA for visualization
pca_result <- prcomp(subset_data, scale. = TRUE)

# Visualize PCA with Mahalanobis distance clusters
fviz_pca_ind(pca_result, geom.ind = "point", col.ind = as.factor(cutree(hclust(dM2), k = 4)), title = "PCA - Mahalanobis")

# Plot the dendrograms for Euclidean Distance
plot(hc_mahalanobis, main = "Dendrogram of Municipalities (Mahalanobis - Ward.D2)", hang = -1, labels = FALSE, xlab = "", sub = "", cex = 0.9)
rect.hclust(hc_mahalanobis, k = 4, border = "red")
rect.hclust(hc_mahalanobis, k = 5, border = "green") 
rect.hclust(hc_mahalanobis, k = 6, border = "orange") 

num_clusters <- 2:6
sil_list <- list()
sil_means <- NULL

# Calculate silhouette values for different numbers of clusters
for (k in num_clusters) {
  clusters <- cutree(hc_mahalanobis, k = k)
  sil_values <- silhouette(clusters, dM2)
  sil_list[[k]] <- sil_values
  sil_means <- c(sil_means, mean(sil_values[, 3]))
}

# Create a df with the siluette means of each number of cluster
sil_widths <- data.frame(clusters = num_clusters, sil_means = sil_means)

# Plot the average silhouette widths df
plot(sil_widths$clusters, sil_widths$sil_means, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters", ylab = "Average Silhouette Width", main = "Average Silhouette Width for Different Numbers of Clusters")


# Plot the silhouette plot for 4 clusters

plot(sil_list[[4]], main = "Silhouette Plot for Ward's Clustering",
     col = 1:max(sil_list[[4]][, 1]), border = NA)

# Perform PCA for visualization
pca_result <- prcomp(subset_data, scale. = TRUE)

# Visualize PCA with Mahalanobis distance clusters
fviz_pca_ind(pca_result, geom.ind = "point", col.ind = as.factor(clusters_mahalanobis), 
             title = "PCA - Mahalanobis (Hierarchical Clustering)", palette = "jco", addEllipses = TRUE, 
             legend.title = "Clusters")


# Output the results
summary(hc_mahalanobis)

# Add 'Both over 80' back to subset_data
subset_data <- subset_data %>%
  mutate(`Both over 80` = x)
# Create a new dataframe for cluster 1
cluster3_data <- subset_data[clusters_mahalanobis == 3, ]

# Calculate the mean of each demographic group for cluster 3
cluster3_means <- colMeans(cluster3_data)

# Convert to a data frame for plotting
cluster3_df <- data.frame(Demographic_Group = names(cluster3_means), Proportion = cluster3_means)

# Plot the data
ggplot(cluster3_df, aes(x = Demographic_Group, y = Proportion, fill = Demographic_Group)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Demographic Distribution in Cluster 3 (with Hierarchical Clustering)", x = "Demographic Group", y = "Proportion") +
  theme_minimal()

#######################################
#
#     model based method
#
######################################
model_subset_data<-subset_data

mc1 = Mclust(model_subset_data, G=1:20, modelNames=c("EII", "VII", "EEI", "EVI", "VEI", "VVI"))
mc1$G#The optimal number of mixture components.

mc1$BIC#All BIC values.

# VEI best model: diagonal, variable volume, equal elliptical shape

mc1$classification #map(z): The classification corresponding to z.
mc1$uncertainty#The uncertainty associated with the classification.

cbind(round(mc1$z,6), round(mc1$uncertainty,6))


################################################################################
# -------------------------------- INTERPRETATIONS -----------------------------
################################################################################

# Suppose 3 is the optimal number of clusters for hierarchical clustering
clusters_model = mc1$classification

subset_data$Cluster = clusters_model
model_subset_data$Cluster = clusters_model

table(model_subset_data$Cluster)


# Subset the data for Cluster 1
cluster1_data <- subset_data[subset_data$Cluster == 1, ]

# Ensure all columns except 'Cluster' are numeric
cluster1_data <- cluster1_data %>%
  mutate(across(-Cluster, as.numeric))

# Melt the data
cluster1_melt <- melt(cluster1_data, id.vars = "Cluster")

# Plot the data
ggplot(cluster1_melt, aes(x = variable, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Demographic Distribution in Cluster 1", x = "Demographic Group", y = "Count") +
  theme_minimal()

# Perform PCA for visualization
pca_result <- prcomp(subset_data, scale. = TRUE)

# Define a custom palette with 18 colors
custom_palette <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", 
                    "#A65628", "#F781BF", "#999999", "#66C2A5", "#FC8D62", "#8DA0CB", 
                    "#E78AC3", "#A6D854", "#FFD92F", "#A6CEE3", "#B2DF8A", "#FB9A99")

# Visualize PCA with the custom palette
fviz_pca_ind(pca_result, geom.ind = "point", col.ind = as.factor(clusters_model), 
             title = "PCA - Mahalanobis (Model Based method)", palette = custom_palette, 
             addEllipses = TRUE, legend.title = "Clusters")


#and the initial hierarchical plot
pca_result <- prcomp(subset_data, scale. = TRUE)

# Visualize PCA with Mahalanobis distance clusters
fviz_pca_ind(pca_result, geom.ind = "point", col.ind = as.factor(clusters_mahalanobis), 
             title = "PCA - Mahalanobis (Hierarchical Clustering)", palette = "jco", addEllipses = TRUE, 
             legend.title = "Clusters")

# Perform PCA for visualization
pca_result <- prcomp(subset_data, scale. = TRUE)

# Visualize PCA with Mahalanobis distance clusters
fviz_pca_ind(pca_result, geom.ind = "point", col.ind = as.factor(cutree(hclust(dM2), k = 4)), title = "PCA - Mahalanobis")

# Calculate ARI between hierarchical clustering and model-based clustering
ari_score <- adjustedRandIndex(clusters_mahalanobis, clusters_model)
cat("Adjusted Rand Index (ARI) between hierarchical and model-based clustering:", ari_score, "\n")

# Comparison of the number of clusters
cat("Number of clusters identified by hierarchical clustering:", length(unique(clusters_mahalanobis)), "\n")
cat("Number of clusters identified by model-based clustering:", length(unique(clusters_model)), "\n")

