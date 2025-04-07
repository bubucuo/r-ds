# ! Load necessary libraries
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(dplyr) # For data manipulation
library(randomForest)
library(gbm)
library(ggplot2)
library(pROC)


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
# Create a Decision Tree model using the rpart library to predict the variable TARGET_BAD_FLAG
# Create a Random Forest model using the randomForest library to predict the variable TARGET_BAD_FLAG
# Create a Gradient Boosting model using the gbm library to predict the variable TARGET_BAD_FLAG
# All model parameters such as tree depth are up to you.
# Do not use TARGET_LOSS_AMT to predict TARGET_BAD_FLAG.
# Plot the Decision Tree and list the important variables for the tree.
# List the important variables for the Random Forest and include the variable importance plot.
# List the important variables for the Gradient Boosting model and include the variable importance plot.
# Using the testing data set, create a ROC curves for all models. They must all be on the same plot.
# Display the Area Under the ROC curve (AUC) for all models.
# Rerun with different training and testing data at least three times.
# Determine which model performed best and why you believe this.
# Write a brief summary of which model you would recommend using. Note that this is your opinion. There is no right answer. You might, for example, select a less accurate model because it is faster or easier to interpret.
classification_model_function <- function(seed_value) {
    set.seed(seed_value)
    # Split data into training(70%) and testing(30%)
    index <- createDataPartition(hmeq$TARGET_BAD_FLAG, p = 0.7, list = FALSE)
    hmeq_train <- hmeq[index, ]
    hmeq_test <- hmeq[-index, ]

    # Do not use TARGET_LOSS_AMT to predict TARGET_BAD_FLAG.
    # So remove TARGET_LOSS_AMT
    predict <- setdiff(names(hmeq), c("TARGET_BAD_FLAG", "TARGET_LOSS_AMT"))

    # ? Decision Tree Model
    decision_tree_model <- rpart(TARGET_BAD_FLAG ~ .,
        data = hmeq_train[, c(predict, "TARGET_BAD_FLAG")],
        method = "class",
        control = rpart.control(cp = 0.01, maxdepth = 4)
    )

    # * Plot the Decision Tree
    rpart.plot(decision_tree_model, main = paste("Decision Tree for Loan Default - Seed ", seed_value))

    # * and list the important variables for the tree
    decision_tree_importance <- decision_tree_model$variable.importance
    print(paste("The Important Variables for the Decision Tree Model - Seed ", seed_value))
    print(decision_tree_importance)

    # ? Random Forest model
    random_forest_model <- randomForest(as.factor(TARGET_BAD_FLAG) ~ .,
        data = hmeq_train[, c(predict, "TARGET_BAD_FLAG")],
        ntree = 200,
        importance = TRUE
    )

    # * List the important variables for the Random Forest and include the variable importance plot.
    print(paste("The Important Variables for the Random Forest - Seed ", seed_value, ":"))
    print(importance(random_forest_model))
    varImpPlot(random_forest_model, main = paste("Random Forest Variable Importance - Seed ", seed_value))


    # ? Gradient Boosting Model
    gradient_boosting_model <- gbm(TARGET_BAD_FLAG ~ .,
        data = hmeq_train[, c(predict, "TARGET_BAD_FLAG")],
        distribution = "bernoulli",
        n.trees = 200,
        interaction.depth = 3,
        shrinkage = 0.1
    )

    # List the important variables for the Gradient Boosting model and include the variable importance plot.
    gradient_boosting_importance <- summary(gradient_boosting_model, plotit = FALSE)
    print(paste("The Important Variables for the Gradient Boosting Model - Seed ", seed_value, ":"))
    print(gradient_boosting_importance)

    # Make predictions on hmeq_test
    decision_tree_predict <- predict(decision_tree_model, hmeq_test, type = "prob")[, 2]
    random_forest_predict <- predict(random_forest_model, hmeq_test, type = "prob")[, 2]
    gradient_boosting_predict <- predict(gradient_boosting_model, hmeq_test, n.trees = 200, type = "response")

    # Calculate the ROC curves
    decision_tree_roc <- roc(hmeq_test$TARGET_BAD_FLAG, decision_tree_predict)
    random_forest_roc <- roc(hmeq_test$TARGET_BAD_FLAG, random_forest_predict)
    gradient_boosting_roc <- roc(hmeq_test$TARGET_BAD_FLAG, gradient_boosting_predict)

    # Plot the ROC curves
    plot(decision_tree_roc,
        col = "#d900ff", main = paste("ROC Curves Comparison - Seed ", seed_value),
        lwd = 2, print.auc = TRUE, print.auc.x = 0.5, print.auc.y = 0.2
    )
    plot(random_forest_roc, col = "red", add = TRUE)
    plot(gradient_boosting_roc, col = "green", add = TRUE)
    legend("bottomright",
        legend = c(
            paste("Decision Tree (AUC =", round(auc(decision_tree_roc), 3)),
            paste("Random Forest (AUC =", round(auc(random_forest_roc), 3)),
            paste("Gradient Boosting Model (AUC =", round(auc(gradient_boosting_roc), 3))
        ),
        col = c("#d900ff", "red", "green"), lwd = 2
    )

    # AUC values
    return(c(decision_tree_auc = auc(decision_tree_roc), random_forest_auc = auc(random_forest_roc), gradient_boosting_auc = auc(gradient_boosting_roc)))
}

fixed_seeds <- c(123, 456, 789)

# Run classification models 3 times
classification_results <- lapply(fixed_seeds, function(seed) {
    classification_model_function(seed)
})

# Compare Performance
classification_summary <- do.call(rbind, classification_results)
print("Compare Performance:")
print(classification_summary)


# ! Step 3: Regression Decision Tree
# Using the code discussed in the lecture, split the data into training and testing data sets.
# Create a Decision Tree model using the rpart library to predict the variable TARGET_LOSS_AMT
# Create a Random Forest model using the randomForest library to predict the variable TARGET_LOSS_AMT
# Create a Gradient Boosting model using the gbm library to predict the variable TARGET_LOSS_AMT
# All model parameters such as tree depth are up to you.
# Do not use TARGET_BAD_FLAG to predict TARGET_LOSS_AMT.
# Plot the Decision Tree and list the important variables for the tree.
# List the important variables for the Random Forest and include the variable importance plot.
# List the important variables for the Gradient Boosting model and include the variable importance plot.
# Using the testing data set, calculate the Root Mean Square Error (RMSE) for all models.
# Rerun with different training and testing data at least three times.
# Determine which model performed best and why you believe this.
# Write a brief summary of which model you would recommend using. Note that this is your opinion. There is no right answer. You might, for example, select a less accurate model because it is faster or easier to interpret.

regression_function <- function(seed_value) {
    hmeq_reg <- hmeq[hmeq$TARGET_BAD_FLAG == 1, ]

    set.seed(seed_value)
    index <- createDataPartition(hmeq_reg$TARGET_LOSS_AMT, p = 0.7, list = FALSE)
    hmeq_train <- hmeq_reg[index, ]
    hmeq_test <- hmeq_reg[-index, ]

    predict <- setdiff(names(hmeq_reg), c("TARGET_BAD_FLAG", "TARGET_LOSS_AMT"))

    # Decision Tree Model
    decision_tree_model <- rpart(TARGET_LOSS_AMT ~ .,
        data = hmeq_train[, c(predict, "TARGET_LOSS_AMT")],
        method = "anova",
        control = rpart.control(cp = 0.01, maxdepth = 4)
    )

    # Plot Decision Tree
    rpart.plot(decision_tree_model, main = paste("Decision Tree for Loss Amount - Seed ", seed_value))

    # Variable importance for Decision Tree
    decision_tree_importance <- decision_tree_model$variable.importance
    print(paste("The Important Variables for the Decision Tree Model - Seed ", seed_value))
    print(decision_tree_importance)

    # Random Forest Model
    random_forest_model <- randomForest(TARGET_LOSS_AMT ~ .,
        data = hmeq_train[, c(predict, "TARGET_LOSS_AMT")],
        ntree = 200,
        importance = TRUE
    )

    # Variable importance for Random Forest
    print(paste("The Important Variables for the Random Forest - Seed ", seed_value, ":"))
    print(importance(random_forest_model))
    varImpPlot(random_forest_model, main = paste("Random Forest Variable Importance - Seed ", seed_value))

    # Gradient Boosting Model
    gradient_boosting_model <- gbm(TARGET_LOSS_AMT ~ .,
        data = hmeq_train[, c(predict, "TARGET_LOSS_AMT")],
        distribution = "gaussian",
        n.trees = 200,
        interaction.depth = 3,
        shrinkage = 0.1
    )

    # List the important variables for the Gradient Boosting model and include the variable importance plot.
    gradient_boosting_importance <- summary(gradient_boosting_model, plotit = FALSE)
    print(paste("The Important Variables for the Gradient Boosting Model - Seed ", seed_value, ":"))
    print(gradient_boosting_importance)

    # Make predictions on hmeq_test
    decision_tree_predict <- predict(decision_tree_model, hmeq_test)
    random_forest_predict <- predict(random_forest_model, hmeq_test)
    gradient_boosting_predict <- predict(gradient_boosting_model, hmeq_test, n.trees = 200)

    # Calculate the RMSE
    decision_tree_rmse <- sqrt(mean((hmeq_test$TARGET_LOSS_AMT - decision_tree_predict)^2))
    random_forest_rmse <- sqrt(mean((hmeq_test$TARGET_LOSS_AMT - random_forest_predict)^2))
    gradient_boosting_rmse <- sqrt(mean((hmeq_test$TARGET_LOSS_AMT - gradient_boosting_predict)^2))

    # Return RMSE values
    return(c(decision_tree_rmse = decision_tree_rmse, random_forest_rmse = random_forest_rmse, gradient_boosting_rmse = gradient_boosting_rmse))
}

# Run regression models 3 times
regression_results <- lapply(fixed_seeds, function(seed) {
    regression_function(seed)
})

# Compare Performance
regression_summary <- do.call(rbind, regression_results)
print("Compare Performance:")
print(regression_summary)

# Step 4: Probability / Severity Model
# Using the code discussed in the lecture, split the data into training and testing data sets.
# Use any model from Step 2 in order to predict the variable TARGET_BAD_FLAG
# Develop three models: Decision Tree, Random Forest, and Gradient Boosting to predict the variable TARGET_LOSS_AMT using only records where TARGET_BAD_FLAG is 1.
# Select one of the models to predict damage.
# List the important variables for both models.
# Using your models, predict the probability of default and the loss given default.
# Multiply the two values together for each record.
# Calculate the RMSE value for the Probability / Severity model.
# Rerun at least three times to be assured that the model is optimal and not over fit or under fit.
# Comment on how this model compares to using the model from Step 3. Which one would your recommend using?

prob_severity_model_function <- function(seed_value) {
    set.seed(seed_value)

    cat("\n Seed Value: ", seed_value, "\n")

    index <- createDataPartition(hmeq$TARGET_BAD_FLAG, p = 0.7, list = FALSE)
    hmeq_train <- hmeq[index, ]
    hmeq_test <- hmeq[-index, ]

    # Random Forest
    prob_model <- randomForest(
        as.factor(TARGET_BAD_FLAG) ~ . - TARGET_LOSS_AMT,
        data = hmeq_train,
        ntree = 200,
        importance = TRUE
    )

    # Gradient Boosting Model
    defaulted_train <- hmeq_train[hmeq_train$TARGET_BAD_FLAG == 1, ]
    severity_model <- gbm(
        TARGET_LOSS_AMT ~ . - TARGET_BAD_FLAG,
        data = defaulted_train,
        distribution = "gaussian",
        n.trees = 200
    )


    cat("\nProbability Model Important Variables:\n")
    prob_imp <- importance(prob_model)
    print(prob_imp[order(-prob_imp[, "MeanDecreaseGini"]), ])

    cat("\n Severity Model Important Variables:\n")
    severity_imp <- summary(severity_model, plotit = FALSE)
    print(severity_imp[order(-severity_imp$rel.inf), ])

    hmeq_test$prob_default <- predict(prob_model, hmeq_test, type = "prob")[, 2]
    defaulted_test <- hmeq_test[hmeq_test$TARGET_BAD_FLAG == 1, ]
    if (nrow(defaulted_test) > 0) {
        defaulted_test$pred_loss <- predict(severity_model, defaulted_test, n.trees = 200)
    }

    # results
    hmeq_test$pred_loss <- 0
    hmeq_test$pred_loss[hmeq_test$TARGET_BAD_FLAG == 1] <- defaulted_test$pred_loss
    hmeq_test$expected_loss <- hmeq_test$prob_default * hmeq_test$pred_loss

    # RMSE
    actual_loss <- ifelse(is.na(hmeq_test$TARGET_LOSS_AMT), 0, hmeq_test$TARGET_LOSS_AMT)
    rmse <- sqrt(mean((actual_loss - hmeq_test$expected_loss)^2))
    cat("\nRMSE:", rmse, "\n")

    return(list(
        variable_importance = list(
            probability = prob_imp,
            severity = severity_imp
        ),
        predictions = hmeq_test,
        rmse = rmse,
        seed = seed_value
    ))
}

ps_res <- lapply(fixed_seeds, prob_severity_model_function)

head(ps_res[[3]]$variable_importance$probability)
