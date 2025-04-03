# ! Load necessary libraries
library(rpart)
library(rpart.plot)
library(ROCR)
library(pROC)
library(caret)
library(dplyr) # For data manipulation


PATH <- "/Users/gaoshaoyun/Documents/study/big data/r-ds/HMEQ_Scrubbed"
FILE_NAME <- "HMEQ_Scrubbed.csv"
INFILE <- paste(PATH, FILE_NAME, sep = "/")
setwd(PATH)

# ! Step 1: Read in the Data
# Read the data into R
# List the structure of the data (str)
# Execute a summary of the data
# Print the first six records
hmeq <- read.csv(FILE_NAME)
str(hmeq)
summary(hmeq)
head(hmeq)

# ! Step 2: Classification Decision Tree
# Using the code discussed in the lecture, split the data into training and testing data sets.
# Use the rpart library to predict the variable TARGET_BAD_FLAG

# Develop two decision trees, one using Gini and the other using Entropy using the training and testing data
# All other parameters such as tree depth are up to you.
# Do not use TARGET_LOSS_AMT to predict TARGET_BAD_FLAG.
# Plot both decision trees
# List the important variables for both trees
# Using the training data set, create a ROC curve for both trees
# Using the testing data set, create a ROC curve for both trees
# Write a brief summary of the decision trees discussing whether or not the trees are are optimal, overfit, or underfit.
# Determine which of the two models performed better and why you believe this

# convert TARGET_BAD_FLAG to factor for classification
hmeq$TARGET_BAD_FLAG <- as.factor(hmeq$TARGET_BAD_FLAG)

# remove TARGET_LOSS_AMT
hmeq_res <- hmeq %>% select(-TARGET_LOSS_AMT)

# write a function to run classification models
classification_model_function <- function(seed_value) {
    set.seed(seed_value)

    # Split data into training (70%) and testing (30%)
    train_index <- createDataPartition(hmeq_res$TARGET_BAD_FLAG, p = 0.7, list = FALSE)
    hmeq_train <- hmeq_res[train_index, ]
    hmeq_test <- hmeq_res[-train_index, ]

    # Gini Model
    gini_model <- rpart(TARGET_BAD_FLAG ~ .,
        data = hmeq_train,
        method = "class",
        parms = list(split = "gini"),
        control = rpart.control(maxdepth = 3, cp = 0.01)
    )

    # Entropy Model
    entropy_model <- rpart(TARGET_BAD_FLAG ~ .,
        data = hmeq_train,
        method = "class",
        parms = list(split = "information"),
        control = rpart.control(maxdepth = 3, cp = 0.01)
    )

    # Plot decision trees
    par(mfrow = c(1, 2))
    rpart.plot(gini_model, main = paste("Gini Model - Seed:", seed_value))
    rpart.plot(entropy_model, main = paste("Entropy Model - Seed:", seed_value))

    # List important variables
    cat("\nImportant variables for Gini model - Seed:", seed_value, "\n")
    print(gini_model$variable.importance)

    cat("\nImportant variables for Entropy model - Seed:", seed_value, "\n")
    print(entropy_model$variable.importance)

    # ROC curves for training data
    gini_train_predict <- predict(gini_model, hmeq_train, type = "prob")[, 2]
    entropy_train_predict <- predict(entropy_model, hmeq_train, type = "prob")[, 2]

    gini_train_roc <- roc(hmeq_train$TARGET_BAD_FLAG, gini_train_predict)
    entropy_train_roc <- roc(hmeq_train$TARGET_BAD_FLAG, entropy_train_predict)

    plot(gini_train_roc, col = "green", main = paste("training ROC - Seed:", seed_value, ""))
    plot(entropy_train_roc, col = "red", add = TRUE)
    legend("bottomright", legend = c("Gini", "Entropy"), col = c("green", "red"), lwd = 2)

    # ROC curves for testing data
    gini_test_predict <- predict(gini_model, hmeq_test, type = "prob")[, 2]
    entropy_test_predict <- predict(entropy_model, hmeq_test, type = "prob")[, 2]

    gini_test_roc <- roc(hmeq_test$TARGET_BAD_FLAG, gini_test_predict)
    entropy_test_roc <- roc(hmeq_test$TARGET_BAD_FLAG, entropy_test_predict)

    plot(gini_test_roc, col = "green", main = paste("testing ROC - Seed:", seed_value, ""))
    plot(entropy_test_roc, col = "red", add = TRUE)
    legend("bottomright", legend = c("Gini", "Entropy"), col = c("green", "red"), lwd = 2)

    return(list(
        gini_train_auc = gini_train_roc$auc,
        entropy_train_auc = entropy_train_roc$auc,
        gini_test_auc = gini_test_roc$auc,
        entropy_test_auc = entropy_test_roc$auc
    ))
}

# * Rerun with different training and testing data at least three times.
res1 <- classification_model_function(123)
res2 <- classification_model_function(456)
res3 <- classification_model_function(789)

# compare performance
classification_function <- rbind(
    c(res1$gini_train_auc, res1$gini_test_auc, res1$entropy_train_auc, res1$entropy_test_auc),
    c(res2$gini_train_auc, res2$gini_test_auc, res2$entropy_train_auc, res2$entropy_test_auc),
    c(res3$gini_train_auc, res3$gini_test_auc, res3$entropy_train_auc, res3$entropy_test_auc)
)

colnames(classification_function) <- c("Gini Train AUC", "Gini Test AUC", "Entropy Train AUC", "Entropy Test AUC")
rownames(classification_function) <- c("Run 1", "Run 2", "Run 3")
cat("\n Compare the Performance: \n")
print(classification_function)


# ! Step 3: Regression Decision Tree
# Using the code discussed in the lecture, split the data into training and testing data sets.
# Use the rpart library to predict the variable TARGET_LOSS_AMT
# Do not use TARGET_BAD_FLAG to predict TARGET_LOSS_AMT.
# Develop two decision trees, one using anova and the other using poisson
# All other parameters such as tree depth are up to you.
# Plot both decision trees
# List the important variables for both trees
# Using the training data set, calculate the Root Mean Square Error (RMSE) for both trees
# Using the testing data set, calculate the Root Mean Square Error (RMSE) for both trees
# Write a brief summary of the decision trees discussing whether or not the trees are are optimal, overfit, or underfit.
# Rerun with different training and testing data at least three times.
# Determine which of the two models performed better and why you believe this

# Remove TARGET_BAD_FLAG
hmeq_reg <- hmeq %>% select(-TARGET_BAD_FLAG)

regression_function <- function(seed_value) {
    set.seed(seed_value)

    train_index <- createDataPartition(hmeq_reg$TARGET_LOSS_AMT, p = 0.7, list = FALSE)
    hmeq_train <- hmeq_reg[train_index, ]
    hmeq_test <- hmeq_reg[-train_index, ]

    # ANOVA Model
    anova <- rpart(TARGET_LOSS_AMT ~ .,
        data = hmeq_train,
        method = "anova",
        control = rpart.control(maxdepth = 3, cp = 0.01)
    )

    # Poisson Model
    poisson <- rpart(TARGET_LOSS_AMT ~ .,
        data = hmeq_train,
        method = "poisson",
        control = rpart.control(maxdepth = 3, cp = 0.01)
    )

    # Plot decision trees
    par(mfrow = c(1, 2))
    rpart.plot(anova, main = paste("ANOVA Model - Seed:", seed_value, ""))
    rpart.plot(poisson, main = paste("Poisson Model - Seed:", seed_value, ""))

    # List important variables
    cat("\nImportant variables for ANOVA model - Seed:", seed_value, "\n")
    print(anova$variable.importance)

    cat("\nImportant variables for Poisson model - Seed:", seed_value, "\n")
    print(poisson$variable.importance)

    # RMSE of training data
    anova_train_predict <- predict(anova, hmeq_train)
    poisson_train_predict <- predict(poisson, hmeq_train)

    anova_train_rmse <- sqrt(mean((hmeq_train$TARGET_LOSS_AMT - anova_train_predict)^2))
    poisson_train_rmse <- sqrt(mean((hmeq_train$TARGET_LOSS_AMT - poisson_train_predict)^2))

    # RMSE of testing data
    anova_test_predict <- predict(anova, hmeq_test)
    poisson_test_predict <- predict(poisson, hmeq_test)

    anova_test_rmse <- sqrt(mean((hmeq_test$TARGET_LOSS_AMT - anova_test_predict)^2))
    poisson_test_rmse <- sqrt(mean((hmeq_test$TARGET_LOSS_AMT - poisson_test_predict)^2))

    # Print RMSE values
    cat("\nANOVA Model RMSE - Seed:", seed_value, ")\n")
    cat("training:", anova_train_rmse, "\n")
    cat("testing:", anova_test_rmse, "\n")

    cat("\nPoisson Model RMSE - Seed:", seed_value, ")\n")
    cat("training:", poisson_train_rmse, "\n")
    cat("testing:", poisson_test_rmse, "\n")

    # Return RMSE values
    return(list(
        anova_train_rmse = anova_train_rmse,
        anova_test_rmse = anova_test_rmse,
        poisson_train_rmse = poisson_train_rmse,
        poisson_test_rmse = poisson_test_rmse
    ))
}

# 3 seeds
reg_res1 <- regression_function(123)
reg_res2 <- regression_function(456)
reg_res3 <- regression_function(789)

regression_res <- rbind(
    c(reg_res1$anova_train_rmse, reg_res1$anova_test_rmse, reg_res1$poisson_train_rmse, reg_res1$poisson_test_rmse),
    c(reg_res2$anova_train_rmse, reg_res2$anova_test_rmse, reg_res2$poisson_train_rmse, reg_res2$poisson_test_rmse),
    c(reg_res3$anova_train_rmse, reg_res3$anova_test_rmse, reg_res3$poisson_train_rmse, reg_res3$poisson_test_rmse)
)

colnames(regression_res) <- c("ANOVA Train RMSE", "ANOVA Test RMSE", "Poisson Train RMSE", "Poisson Test RMSE")
rownames(regression_res) <- c("Run 1", "Run 2", "Run 3")

cat("\n Compare the Performance: \n")
print(regression_res)


# ! Step 4: Probability / Severity Model Decision Tree (Push Yourself!)
# Using the code discussed in the lecture, split the data into training and testing data sets.
# Use the rpart library to predict the variable TARGET_BAD_FLAG
# Use the rpart library to predict the variable TARGET_LOSS_AMT using only records where TARGET_BAD_FLAG is 1.
# Plot both decision trees
# List the important variables for both trees
# Using your models, predict the probability of default and the loss given default.
# Multiply the two values together for each record.
# Calculate the RMSE value for the Probability / Severity model.
# Rerun at least three times to be assured that the model is optimal and not over fit or under fit.
# Comment on how this model compares to using the model from Step 3. Which one would your recommend using?

prob_severity_model_function <- function(seed_value) {
    set.seed(seed_value)

    train_index <- createDataPartition(hmeq$TARGET_BAD_FLAG, p = 0.7, list = FALSE)
    hmeq_train <- hmeq[train_index, ]
    hmeq_test <- hmeq[-train_index, ]

    prob <- rpart(TARGET_BAD_FLAG ~ . - TARGET_LOSS_AMT,
        data = hmeq_train,
        method = "class",
        parms = list(split = "gini"),
        control = rpart.control(maxdepth = 3, cp = 0.01)
    )

    severity_data <- hmeq_train %>% filter(TARGET_BAD_FLAG == 1)
    severity <- rpart(TARGET_LOSS_AMT ~ . - TARGET_BAD_FLAG,
        data = severity_data,
        method = "anova",
        control = rpart.control(maxdepth = 3, cp = 0.01)
    )

    # plot decision trees
    par(mfrow = c(1, 2))
    rpart.plot(prob, main = paste("Probability Model - Seed:", seed_value, ""))
    rpart.plot(severity, main = paste("Severity Model - Seed:", seed_value, ""))

    # List important variables
    cat("\nImportant variables for Probability Model - Seed:", seed_value, "\n")
    print(prob$variable.importance)

    cat("\nImportant variables for Severity Model - Seed:", seed_value, "\n")
    print(severity$variable.importance)

    test_prob <- predict(prob, hmeq_test, type = "prob")[, 2]
    test_severity <- predict(severity, hmeq_test)

    test_severity[test_prob < 0.5] <- 0
    expected_loss <- test_prob * test_severity

    # RMSE
    actual_loss <- hmeq_test$TARGET_LOSS_AMT
    rmse <- sqrt(mean((actual_loss - expected_loss)^2))

    cat("\nProbability/Severity Model RMSE - Seed:", seed_value, "):", rmse, "\n")

    #
    return(rmse)
}


# Run models with 3 different seeds
ps_rmse1 <- prob_severity_model_function(123)
ps_rmse2 <- prob_severity_model_function(456)
ps_rmse3 <- prob_severity_model_function(789)


# compare results
ps_res <- c(ps_rmse1, ps_rmse2, ps_rmse3)
names(ps_res) <- c("Run 1", "Run 2", "Run 3")


cat("\nProbability/Severity Model RMSE Comparison:\n")
print(ps_res)
cat("\n Compare the Performance: \n")
cat("Average Probability/Severity RMSE:", mean(ps_res), "\n")
cat("Average ANOVA Test RMSE:", mean(regression_results[, 2]), "\n")
cat("Average Poisson Test RMSE:", mean(regression_results[, 4]), "\n")
