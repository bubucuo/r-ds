# ! Load necessary libraries
library(rpart)
library(rpart.plot)
library(ROCR)
library(caret)

# ! Step 1: Read in the Data
# Read the data into R
# List the structure of the data (str)
# Execute a summary of the data
# Print the first six records

PATH <- "/Users/gaoshaoyun/Documents/study/big data/r-ds/HMEQ_Scrubbed"
FILE_NAME <- "HMEQ_Scrubbed.csv"

INFILE <- paste(PATH, FILE_NAME, sep = "/")

setwd(PATH)
hmeq <- read.csv(FILE_NAME)
str(hmeq)
summary(hmeq)
head(hmeq)


# ! Step 2: Classification Decision Tree
# Use the rpart library to predict the variable TARGET_BAD_FLAG
# Develop two decision trees, one using Gini and the other using Entropy
# All other parameters such as tree depth are up to you.
# Do not use TARGET_LOSS_AMT to predict TARGET_BAD_FLAG.
# sy- Write a brief summary of the decision trees discussing whether or not they make sense. Which tree would you recommend using? What type of person will default on a loan?

# * install.packages("caret", dependencies = TRUE)
# library(caret)
# library(rpart)
# library(rpart.plot)
# remove the TARGET_LOSS_AMT variable from the dataset, to avoid predicting TARGET_BAD_FLAG using it
new_hmeq <- hmeq[, !names(hmeq) %in% "TARGET_LOSS_AMT"]
# ConvertTARGET_BAD_FLAG to factors for classification
new_hmeq$TARGET_BAD_FLAG <- as.factor(new_hmeq$TARGET_BAD_FLAG)

set.seed(1)
index <- createDataPartition(new_hmeq$TARGET_BAD_FLAG, p = 0.7, list = FALSE)
train_data <- new_hmeq[index, ]
test_data <- new_hmeq[-index, ]

# sy- Plot both decision trees
# * 1. Decision Tree with Gini index
gini_tree <- rpart(TARGET_BAD_FLAG ~ .,
  data = train_data,
  method = "class",
  parms = list(split = "gini"),
  control = rpart.control(maxdepth = 6, cp = 0.001)
)

# * 2. Decision Tree with Entropy
entropy_tree <- rpart(TARGET_BAD_FLAG ~ .,
  data = train_data,
  method = "class",
  parms = list(split = "information"),
  control = rpart.control(maxdepth = 6, cp = 0.001)
)

# * Plot both decision trees
par(mfrow = c(1, 2))
rpart.plot(gini_tree, main = "Gini Decision Tree - By ShaoYun Gao")
rpart.plot(entropy_tree, main = "Entropy Decision Tree - By ShaoYun Gao")


# sy- List the important variables for both trees
cat("\nImportant variables for Gini trees:\n")
print(gini_tree$variable.importance)

cat("\nImportant variables for Entropy trees:\n")
print(entropy_tree$variable.importance)

# sy- Create a ROC curve for both trees
library(pROC)
gini_probs <- predict(gini_tree, test_data, type = "prob")[, 2]
entropy_probs <- predict(entropy_tree, test_data, type = "prob")[, 2]

# ROC curves
gini_roc_curve <- roc(test_data$TARGET_BAD_FLAG, gini_probs)
entropy_roc_curve_curve <- roc(test_data$TARGET_BAD_FLAG, entropy_probs)

# Plot ROC curves
plot(gini_roc_curve, col = "green", main = "ROC Curves for Classification Trees - by ShaoYun Gao")
lines(entropy_roc_curve_curve, col = "red")
legend("bottomright", legend = c("Gini", "Entropy"), col = c("green", "red"), lwd = 2)



# ! Step 3: Regression Decision Tree
# Use the rpart library to predict the variable TARGET_LOSS_AMT
# Develop two decision trees, one using anova and the other using poisson
# All other parameters such as tree depth are up to you.
# Write a brief summary of the decision trees discussing whether or not they make sense. Which tree would you recommend using? What factors dictate a large loss of money?

#  Do not use TARGET_BAD_FLAG to predict TARGET_LOSS_AMT.
new_data <- hmeq[, !(names(hmeq) %in% c("TARGET_BAD_FLAG"))]

# set seed
set.seed(1)
index2 <- createDataPartition(new_data$TARGET_LOSS_AMT, p = 0.7, list = FALSE)
train_data2 <- new_data[index2, ]
test_data2 <- new_data[-index2, ]

# anova Decision Tree
anova_tree <- rpart(TARGET_LOSS_AMT ~ .,
  data = train_data2,
  method = "anova",
  control = rpart.control(maxdepth = 6, cp = 0.001)
)

# poisson Decision Tree
poisson_tree <- rpart(TARGET_LOSS_AMT ~ .,
  data = train_data2,
  method = "poisson",
  control = rpart.control(maxdepth = 6, cp = 0.001)
)

# sy- Plot both decision trees
par(mfrow = c(1, 2))
rpart.plot(anova_tree, main = "ANOVA Decision Tree by ShaoYun Gao")
rpart.plot(poisson_tree, main = "Poisson Decision Tree - by ShaoYun Gao")

# List important variables for both trees
cat("\nANOVA Important variables:\n")
print(anova_tree$variable.importance)

cat("\nPoisson Important variables:\n")
print(poisson_tree$variable.importance)

# Calculate RMSE for both trees
anova_predict <- predict(anova_tree, test_data2)
poisson_predict <- predict(poisson_tree, test_data2)

rmse_anova <- sqrt(mean((test_data2$TARGET_LOSS_AMT - anova_predict)^2))
rmse_poisson <- sqrt(mean((test_data2$TARGET_LOSS_AMT - poisson_predict)^2))

cat("\n RMSE for ANOVA tree:", rmse_anova, "\n")
cat("RMSE for Poisson tree:", rmse_poisson, "\n")

# ! Step 4: Probability / Severity Model Decision Tree (Push Yourself!)
# Use the rpart library to predict the variable TARGET_BAD_FLAG
# Use the rpart library to predict the variable TARGET_LOSS_AMT using only records where TARGET_BAD_FLAG is 1.
# Plot both decision trees
# List the important variables for both trees
# Using your models, predict the probability of default and the loss given default.
# Multiply the two values together for each record.
# Calculate the RMSE value for the Probability / Severity model.
# Comment on how this model compares to using the model from Step 3. Which one would your recommend using?

# Use the rpart library to predict the variable TARGET_LOSS_AMT using only records where TARGET_BAD_FLAG is 1.
severity_data <- hmeq[hmeq$TARGET_BAD_FLAG == 1, !(names(hmeq) %in% c("TARGET_BAD_FLAG"))]


# Split severity data
set.seed(1)
train_index_sev <- createDataPartition(severity_data$TARGET_LOSS_AMT, p = 0.7, list = FALSE)
train_sev <- severity_data[train_index_sev, ]
test_sev <- severity_data[-train_index_sev, ]

# Severity model based anova
severity_model <- rpart(TARGET_LOSS_AMT ~ .,
  data = train_sev,
  method = "anova",
  control = rpart.control(maxdepth = 6, cp = 0.001)
)

# Plot both decision trees
par(mfrow = c(1, 2))
rpart.plot(gini_tree, main = "Probability Model - by ShaoYun Gao")
rpart.plot(severity_model, main = "Severity Model - by ShaoYun Gao")

# List the important variables for both trees
cat("\n Important variables - Probability Model:\n")
print(gini_tree$variable.importance)

cat("\n Important variables - Severity Model:\n")
print(severity_model$variable.importance)

# sy- Predict probability and severity
data_full <- hmeq[-index, ]
probability_predictions <- predict(gini_tree, data_full, type = "prob")[, 2]
severity_predictions <- predict(severity_model, data_full)

# sy- Multiply probability and severity
combined_predictions <- probability_predictions * severity_predictions

# sy- Calculate RMSE
values <- data_full$TARGET_LOSS_AMT
rmse_combined <- sqrt(mean((values - combined_predictions)^2))

cat("\n RMSE for Probability/Severity model:", rmse_combined, "\n")
