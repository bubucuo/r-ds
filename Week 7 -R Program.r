# ! Load libraries
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(pROC)
library(Rtsne)
library(FactoMineR)
library(factoextra)


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
# Do a Principal Component Analysis (PCA) on the continuous variables.
# Display the Scree Plot of the PCA analysis.
# Using the Scree Plot, determine how many Principal Components you wish to use. Note, you must use at least two. You may decide to use more. Justify your decision. Note that there is no wrong answer. You will be graded on your reasoning, not your decision.
# Print the weights of the Principal Components. Use the weights to tell a story on what the Principal Components represent.
# Perform a scatter plot using the first two Principal Components. Color the scatter plot dots using the Target Flag. One color will represent "defaults" and the other color will represent "non defaults". Comment on whether you consider the first two Principal Components to be predictive. If you believe the graph is too cluttered, you are free to do a random sample of the data to make it more readable. That is up to you.

#  continuous variables
continuous_variables <- c(
    "LOAN", "IMP_MORTDUE", "IMP_VALUE", "IMP_YOJ", "IMP_DEROG",
    "IMP_DELINQ", "IMP_CLAGE", "IMP_NINQ", "IMP_CLNO", "IMP_DEBTINC"
)

# Target variable
target_variables <- "TARGET_BAD_FLAG"

# data for PCA
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

ggplot(pca_scores, aes(x = Dim.1, y = Dim.2, color = Default)) +
    geom_point(alpha = 0.5) +
    scale_color_manual(
        values = c("Non Defaults" = "green", "Defaults" = "red")
    ) +
    labs(
        title = "PCA - First Two Principal Components - by Shaoyun Gao",
        x = "PC1",
        y = "PC2"
    ) +
    theme_minimal()


# ! Step 3: tSNE Analysis
# Use only the input variables. Do not use either of the target variables.
# Use only the continuous variables. Do not use any of the flag variables.
# Do a tSNE analysis on the data. Set the dimensions to 2.
# Run two tSNE analysis for Perplexity=30. Color the scatter plot dots using the Target Flag. One color will represent "defaults" and the other color will represent "non defaults". Comment on whether you consider the tSNE values to be predictive.
# Repeat the previous step with a Perplexity greater than 30 (try to get a value much higher than 30).
# Repeat the previous step with a Perplexity less than 30 (try to get a value much lower than 30).
# Decide on which value of Perplexity best predicts the Target Flag.
# Train two Random Forest Models to predict each of the tSNE values.

set.seed(123)

# tSNE, perplexity=30
tsne_30 <- Rtsne(pca_data, dims = 2, perplexity = 30, verbose = TRUE)
tsne_df_30 <- as.data.frame(tsne_30$Y)
tsne_df_30$Default <- factor(hmeq[[target_variables]], levels = c(0, 1), labels = c("Non Defaults", "Defaults"))

ggplot(tsne_df_30, aes(x = V1, y = V2, color = Default)) +
    geom_point(alpha = 0.5) +
    scale_color_manual(
        values = c("Non Defaults" = "green", "Defaults" = "red")
    ) +
    labs(
        title = "tSNE with Perplexity=30",
        x = "tSNE Dimension 1",
        y = "tSNE Dimension 2"
    ) +
    theme_minimal()

# tSNE, greater than 30, 100
tsne_100 <- Rtsne(pca_data, dims = 2, perplexity = 100, verbose = TRUE)
tsne_df_100 <- as.data.frame(tsne_100$Y)
tsne_df_100$Default <- factor(hmeq[[target_variables]], levels = c(0, 1), labels = c("Non Defaults", "Defaults"))

ggplot(tsne_df_100, aes(x = V1, y = V2, color = Default)) +
    geom_point(alpha = 0.5) +
    scale_color_manual(
        values = c("Non Defaults" = "green", "Defaults" = "red")
    ) +
    labs(
        title = "tSNE with Perplexity=100",
        x = "tSNE Dimension 1",
        y = "tSNE Dimension 2"
    ) +
    theme_minimal()

# tSNE, lower than 30, 10
tsne_10 <- Rtsne(pca_data, dims = 2, perplexity = 10, verbose = TRUE)
tsne_df_10 <- as.data.frame(tsne_10$Y)
tsne_df_10$Default <- factor(hmeq[[target_variables]], levels = c(0, 1), labels = c("Non Defaults", "Defaults"))

ggplot(tsne_df_10, aes(x = V1, y = V2, color = Default)) +
    geom_point(alpha = 0.5) +
    scale_color_manual(
        values = c("Non Defaults" = "green", "Defaults" = "red")
    ) +
    labs(
        title = "tSNE with Perplexity=10",
        x = "tSNE Dimension 1",
        y = "tSNE Dimension 2"
    ) +
    theme_minimal()



set.seed(123)
randomForest_tsne1 <- randomForest(x = pca_data, y = tsne_df_30$V1, ntree = 100)
randomForest_tsne2 <- randomForest(x = pca_data, y = tsne_df_30$V2, ntree = 100)

# ! Step 4: Tree and Regression Analysis on Original Data
# Create a Decision Tree to predict Loan Default (Target Flag=1). Comment on the variables that were included in the model.
# Create a Logistic Regression model to predict Loan Default (Target Flag=1). Use either Forward, Backward, or Stepwise variable selection. Comment on the variables that were included in the model.
# Create a ROC curve showing the accuracy of the model.
# Calculate and display the Area Under the ROC Curve (AUC).

# Decision Tree
decisionTree_model <- rpart(as.formula(paste(target_variables, "~ .")),
    data = hmeq %>% select(-TARGET_LOSS_AMT),
    method = "class"
)

rpart.plot(decisionTree_model, extra = 104)
print(decisionTree_model)


full_model <- glm(as.formula(paste(target_variables, "~ .")),
    data = hmeq %>% select(-TARGET_LOSS_AMT),
    family = binomial
)

null_model <- glm(as.formula(paste(target_variables, "~ 1")),
    data = hmeq %>% select(-TARGET_LOSS_AMT),
    family = binomial
)

step_model <- step(null_model,
    scope = list(lower = null_model, upper = full_model),
    direction = "both"
)

summary(step_model)

# ROC curves and AUC
probabilities <- predict(step_model, type = "response")
roc_curve <- roc(hmeq[[target_variables]], probabilities)
plot(roc_curve, main = "ROC Curve for Logistic Regression")
auc(roc_curve)

# ! Step 5: Tree and Regression Analysis on PCA/tSNE Data
# Append the Principal Component values from Step 2 to your data set.
# Using the Random Forest models from Step 3, append the two tSNE values to the data set.
# Remove all of the continuous variables from the data set (set them to NULL). Keep the flag variables in the data set.
# Create a Decision Tree to predict Loan Default (Target Flag=1). Comment on the variables that were included in the model. Did any of the Principal Components or tSNE values make it into the model? Discuss why or why not.
# Create a Logistic Regression model to predict Loan Default (Target Flag=1). Use either Forward, Backward, or Stepwise variable selection. Comment on the variables that were included in the model. Did any of the Principal Components or tSNE values make it into the model? Discuss why or why not.
# Create a ROC curve showing the accuracy of the model.
# Calculate and display the Area Under the ROC Curve (AUC).

data_pca_TSNE <- hmeq
data_pca_TSNE$PC1 <- pca_result$ind$coord[, 1]
data_pca_TSNE$PC2 <- pca_result$ind$coord[, 2]
data_pca_TSNE$PC3 <- pca_result$ind$coord[, 3]
data_pca_TSNE$tSNE1 <- predict(randomForest_tsne1, pca_data)
data_pca_TSNE$tSNE2 <- predict(randomForest_tsne2, pca_data)
data_pca_TSNE <- data_pca_TSNE %>%
    select(-all_of(continuous_variables))



pca2 <- rpart(as.formula(paste(target_variables, "~ .")),
    data = data_pca_TSNE %>% select(-TARGET_LOSS_AMT),
    method = "class"
)

rpart.plot(pca2, extra = 104)
print(pca2)



full_pca <- glm(as.formula(paste(target_variables, "~ .")),
    data = data_pca_TSNE %>% select(-TARGET_LOSS_AMT),
    family = binomial
)

null_pca <- glm(as.formula(paste(target_variables, "~ 1")),
    data = data_pca_TSNE %>% select(-TARGET_LOSS_AMT),
    family = binomial
)

step_pca <- step(null_pca,
    scope = list(lower = null_pca, upper = full_pca),
    direction = "both"
)

summary(step_pca)
p_pca <- predict(step_pca, type = "response")
roc_curve_pca <- roc(data_pca_TSNE[[target_variables]], p_pca)
plot(roc_curve_pca, main = "ROC Curve for PCA/tSNE Logistic Regression - by Shaoyun Gao")
auc(roc_curve_pca)
