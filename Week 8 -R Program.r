# Load necessary libraries
library(tidyverse)
library(flexclust)
library(rpart)
library(rpart.plot)
library(factoextra)
library(FactoMineR)




PATH <- "/Users/gaoshaoyun/Documents/study/big data/r-ds/HMEQ_Scrubbed"
FILE_NAME <- "HMEQ_Scrubbed.csv"
INFILE <- paste(PATH, FILE_NAME, sep = "/")
setwd(PATH)

# ! Step 1: Load and prepare the data
hmeq <- read.csv(FILE_NAME)
hmeq <- read.csv(FILE_NAME)
str(hmeq)
summary(hmeq)
head(hmeq)

# ! Step 2: PCA Analysis
# Use only the input variables. Do not use either of the target variables.
# Use only the continuous variables. Do not use any of the flag variables.
# Select at least 4 of the continuous variables. It would be preferable if there were a theme to the variables selected.
# Do a Principal Component Analysis (PCA) on the continuous variables.
# Display the Scree Plot of the PCA analysis.
# Using the Scree Plot, determine how many Principal Components you wish to use. Note, you must use at least two. You may decide to use more. Justify your decision. Note that there is no wrong answer. You will be graded on your reasoning, not your decision.
# Print the weights of the Principal Components. Use the weights to tell a story on what the Principal Components represent.
# Perform a scatter plot using the first two Principal Components. Do not color the dots. Leave them black.

# continuous variables
continuous_variables <- c(
    "LOAN", "IMP_MORTDUE", "IMP_VALUE", "IMP_YOJ", "IMP_DEROG",
    "IMP_DELINQ", "IMP_CLAGE", "IMP_NINQ", "IMP_CLNO", "IMP_DEBTINC"
)

target_variables <- "TARGET_BAD_FLAG"

# Perform PCA
pca_data <- hmeq %>%
    select(all_of(continuous_variables)) %>%
    scale()
pca_result <- PCA(pca_data, graph = FALSE)

# scree plot
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50))

# List weights of Principal Components
pca_weights <- as.data.frame(pca_result$var$coord)
print(round(pca_weights[, 1:3], 3))

pca_scores <- as.data.frame(pca_result$ind$coord[, 1:2])
pca_scores$Default <- factor(hmeq[[target_variables]], levels = c(0, 1), labels = c("Non Defaults", "Defaults"))

# * black
ggplot(pca_scores, aes(x = Dim.1, y = Dim.2)) +
    geom_point(color = "black", alpha = 0.5) +
    labs(
        title = "PCA - First Two Principal Components - by Shaoyun Gao",
        x = "PC1",
        y = "PC2"
    ) +
    theme_minimal()

# ! Step 3: Determine optimal number of clusters
# Use the principal components from Step 2 for this step.
# Using the methods presented in the lectures, complete a KMeans cluster analysis for N=1 to at least N=10. Feel free to take the number higher.
# Print a scree plot of the clusters and determine how many clusters would be optimum. Justify your decision.

pca_scores <- as.data.frame(pca_result$ind$coord[, 1:2])
wss <- sapply(1:15, function(k) {
    kmeans(pca_scores, k, nstart = 20, iter.max = 100)$tot.withinss
})

# Plot
plot(1:15, wss,
    type = "b", pch = 19, frame = FALSE,
    xlab = "Number of clusters K",
)

# ! Step 4: Cluster Analysis
# Using the number of clusters from step 3, perform a cluster analysis using the principle components from Step 2.
# Print the number of records in each cluster.
# Print the cluster center points for each cluster
# Convert the KMeans clusters into "flexclust" clusters
# Print the barplot of the cluster. Describe the clusters from the barplot.
# Score the training data using the flexclust clusters. In other words, determine which cluster they are in.
# Perform a scatter plot using the first two Principal Components. Color the plot by the cluster membership.
# Add a legend to the plot.
# Determine if the clusters predict loan default.

set.seed(123)
km <- kmeans(pca_scores, centers = 3, nstart = 20)

# Cluster sizes
print(table(km$cluster))

# Cluster centers
print(km$centers)

kf <- as.kcca(km, pca_scores)

# Plot
barplot(kf)

clus <- predict(kf)
print(clus)

# Plot
plot(pca_scores[, 1:2],
    col = km$cluster, pch = 20,
    xlab = "PC1",
    ylab = "PC2",
)
legend("topright",
    legend = paste("Cluster", 1:3),
    col = 1:3, pch = 20
)

hmeq$cluster <- km$cluster
print(table(hmeq$cluster, hmeq$TARGET_BAD_FLAG))

# ! Step 5: Describe Clusters Using Decision Trees
# Using the original data from Step 2, predict cluster membership using a Decision Tree
# Display the Decision Tree
# Using the Decision Tree plot, describe or tell a story of each cluster. Comment on whether the clusters make sense.

tm <- rpart(
    cluster ~ LOAN + IMP_VALUE + IMP_MORTDUE + IMP_YOJ +
        IMP_DELINQ + IMP_CLAGE + IMP_NINQ + IMP_CLNO + IMP_DEBTINC,
    data = hmeq, method = "class"
)

# Plot
rpart.plot(tm,
    main = "Decision Tree",
    type = 3,
    extra = 1,
    fallen.leaves = TRUE,
    box.palette = "RdYlGn",
    shadow.col = "gray",
    nn = TRUE
)

# ! Step 6: Corporate Use Discussion
# Discuss how you might use these clusters in a corporate
# ? answer
# We can use these clusters to distinguish customer segments and enable targeted marketing strategies. For example, certain segments may exhibit higher credit risk, in which case the company can adjust service fees or offer different financial products accordingly.
