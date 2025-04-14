# ! Load necessary libraries
library(tidyverse)
library(caret)
library(rpart)
library(randomForest)
library(pROC)
library(gbm)
library(MASS) # stepAIC

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
# Do not use TARGET_LOSS_AMT to predict TARGET_BAD_FLAG .
# Create a LOGISTIC REGRESSION model using ALL the variables to predict the variable TARGET_BAD_FLAG
# Create a LOGISTIC REGRESSION model and using BACKWARD VARIABLE SELECTION.
# Create a LOGISTIC REGRESSION model and using a DECISION TREE and FORWARD STEPWISE SELECTION.
# List the important variables from the Logistic Regression Variable Selections.
# Compare the variables from the logistic Regression with those of the Random Forest and the Gradient Boosting.
# Using the testing data set, create a ROC curves for all models. They must all be on the same plot.
# Display the Area Under the ROC curve (AUC) for all models.
# Determine which model performed best and why you believe this.
# Write a brief summary of which model you would recommend using. Note that this is your opinion. There is no right answer. You might, for example, select a less accurate model because it is faster or easier to interpret.

# prepare data
features <- setdiff(names(hmeq), c("TARGET_LOSS_AMT", "TARGET_BAD_FLAG"))
set.seed(123)
index <- createDataPartition(hmeq$TARGET_BAD_FLAG, p = 0.7, list = FALSE)
hmeq_train <- hmeq[index, ]
hmeq_test <- hmeq[-index, ]

# ? let's model ~

# * full model
full_logit <- glm(
    TARGET_BAD_FLAG ~ .,
    data = hmeq_train[, c(features, "TARGET_BAD_FLAG")],
    family = binomial
)

# * backward selection
backward_logit <- step(
    full_logit,
    direction = "backward",
    trace = 0
)

# * forward selection
tree_model <- rpart(
    TARGET_BAD_FLAG ~ .,
    data = hmeq_train[, c(features, "TARGET_BAD_FLAG")],
    method = "class"
)
tree_imp <- tree_model$variable.importance
selected_vars <- names(sort(tree_imp, decreasing = TRUE))[1:5]
forward_formula <- as.formula(paste("TARGET_BAD_FLAG ~", paste(selected_vars, collapse = "+")))
forward_logit <- glm(
    forward_formula,
    data = hmeq_train,
    family = binomial
)

# plot
pred_full <- predict(full_logit, hmeq_test, type = "response")
pred_back <- predict(backward_logit, hmeq_test, type = "response")
pred_forward <- predict(forward_logit, hmeq_test, type = "response")

colors <- c("red", "green", "yellow") # 蓝、橙、绿
model_names <- c("Full Model", "Backward Selection", "Forward Selection")

# 创建空白绘图
plot(1,
    type = "n",
    xlim = c(1, 0), ylim = c(0, 1),
    xlab = "False Positive Rate",
    ylab = "True Positive Rate",
    main = "ROC Curves Comparison"
)

roc_full <- roc(hmeq_test$TARGET_BAD_FLAG, pred_full)
roc_back <- roc(hmeq_test$TARGET_BAD_FLAG, pred_back)
roc_forward <- roc(hmeq_test$TARGET_BAD_FLAG, pred_forward)

plot(roc_full, col = colors[1], add = TRUE)
plot(roc_back, col = colors[2], add = TRUE)
plot(roc_forward, col = colors[3], add = TRUE)

# add legend to the plot
legend("bottomright",
    legend = c(
        paste(model_names[1], "(AUC =", round(auc(roc_full), 3), ")"),
        paste(model_names[2], "(AUC =", round(auc(roc_back), 3), ")"),
        paste(model_names[3], "(AUC =", round(auc(roc_forward), 3), ")")
    ),
    col = colors,
    lwd = 2
)

# compare AUC
cat("\n=== full model AUC ===\n")
print(summary(full_logit)$coefficients %>%
    as.data.frame() %>%
    arrange(-abs(Estimate)) %>%
    head(5))

cat("\n=== backward model AUC ===\n")
print(summary(backward_logit)$coefficients %>%
    as.data.frame() %>%
    arrange(-abs(Estimate)))

cat("\n=== forward model AUC ===\n")
print(selected_vars)


# ! Step 3: Linear Regression
# Using the code discussed in the lecture, split the data into training and testing data sets.
# Do not use TARGET_BAD_FLAG to predict TARGET_LOSS_AMT.
# Create a LINEAR REGRESSION model using ALL the variables to predict the variable TARGET_BAD_AMT
# Create a LINEAR REGRESSION model and using BACKWARD VARIABLE SELECTION.
# Create a LINEAR REGRESSION model and using a DECISION TREE and FORWARD STEPWISE SELECTION.
# List the important variables from the Linear Regression Variable Selections.
# Compare the variables from the Linear Regression with those of the Random Forest and the Gradient Boosting.
# Using the testing data set, calculate the Root Mean Square Error (RMSE) for all models.
# Determine which model performed best and why you believe this.
# Write a brief summary of which model you would recommend using. Note that this is your opinion. There is no right answer. You might, for example, select a less accurate model because it is faster or easier to interpret.

# prepare data
loss_data <- hmeq[hmeq$TARGET_BAD_FLAG == 1, ]
features <- setdiff(names(loss_data), c("TARGET_BAD_FLAG", "TARGET_LOSS_AMT"))

# split data
set.seed(123)
index <- createDataPartition(loss_data$TARGET_LOSS_AMT, p = 0.7, list = FALSE)
hmeq_train <- loss_data[index, ]
hmeq_test <- loss_data[-index, ]

# ? let's model ~

# * full model
full_lm <- lm(
    TARGET_LOSS_AMT ~ . - TARGET_BAD_FLAG,
    data = hmeq_train
)

# * backward selection
backward_lm <- stepAIC(
    full_lm,
    direction = "backward",
    trace = 0
)

# * forward selection
tree_model <- rpart(
    TARGET_LOSS_AMT ~ . - TARGET_BAD_FLAG,
    data = hmeq_train,
    method = "anova"
)
tree_imp <- tree_model$variable.importance
selected_vars <- names(sort(tree_imp, decreasing = TRUE))[1:5] # 选择前5重要变量

forward_lm <- lm(
    as.formula(paste("TARGET_LOSS_AMT ~", paste(selected_vars, collapse = "+"))),
    data = hmeq_train
)

# * random forest
rf_model <- randomForest(
    TARGET_LOSS_AMT ~ . - TARGET_BAD_FLAG,
    data = hmeq_train,
    importance = TRUE
)

# * gradient boosting
gbm_model <- gbm(
    TARGET_LOSS_AMT ~ . - TARGET_BAD_FLAG,
    data = hmeq_train,
    distribution = "gaussian",
    n.trees = 200,
    interaction.depth = 3
)

# compare models
cat("\n=== full model important variables ===\n")
print(coef(full_lm) %>% sort(decreasing = TRUE) %>% head(5))

cat("\n=== backward model important variables ===\n")
print(coef(backward_lm) %>% sort(decreasing = TRUE) %>% head(5))

cat("\n=== forward model important variables ===\n")
print(selected_vars)
print(importance(rf_model) %>% as.data.frame() %>% arrange(-`%IncMSE`))

cat("\n=== random forest important variables ===\n")
print(summary(gbm_model, plotit = FALSE))


# estimate model performance

# RMSE function
calc_rmse <- function(model, test_data) {
    pred <- predict(model, newdata = test_data)
    sqrt(mean((test_data$TARGET_LOSS_AMT - pred)^2))
}

# calculate RMSE for all models
rmse_results <- data.frame(
    Model = c("Full Model", "Backward Selection", "Forward Selection", "Random Forest", "Gradient Boosting"),
    RMSE = c(
        calc_rmse(full_lm, hmeq_test),
        calc_rmse(backward_lm, hmeq_test),
        calc_rmse(forward_lm, hmeq_test),
        calc_rmse(rf_model, hmeq_test),
        calc_rmse(gbm_model, hmeq_test)
    )
)

cat("\n=== model performance ===\n")
print(rmse_results %>% arrange(RMSE))


cat("\n=== recommendation ===\n")
# select the best model based on RMSE, the lower the better
best_model <- rmse_results$Model[which.min(rmse_results$RMSE)]
cat("the best model is :", best_model, "\n")


# ! Step 4: Probability / Severity Model
# Using the code discussed in the lecture, split the data into training and testing data sets.
# Use any LOGISTIC model from Step 2 in order to predict the variable TARGET_BAD_FLAG
# Use a LINEAR REGRESSION model to predict the variable TARGET_LOSS_AMT using only records where TARGET_BAD_FLAG is 1.
# List the important variables for both models.
# Using your models, predict the probability of default and the loss given default.
# Multiply the two values together for each record.
# Calculate the RMSE value for the Probability / Severity model.
# Comment on how this model compares to using the model from Step 3. Which one would your recommend using?

# backward_logit is the best model from Step 2
# backward_logit is the best model from Step 2
best_logit_model <- backward_logit
step3_best_rmse <- rmse_results$RMSE[which.min(rmse_results$RMSE)]

# Probability Model, aka Logistic Regression from Step 2
cat("\n=== Probability Model (Logistic) Top Variables ===\n")
logit_vars <- summary(best_logit_model)$coefficients %>%
    as.data.frame() %>%
    arrange(-abs(Estimate)) %>%
    head(5)
print(logit_vars)

# Severity Model
target_var_name <- "TARGET_BAD_FLAG"

severity_data <- hmeq_train[hmeq_train[[target_var_name]] == 1, ]

# Create model formula without using get()
model_terms <- names(severity_data)[!names(severity_data) %in% c(target_var_name, "TARGET_LOSS_AMT")]
severity_formula <- as.formula(paste("TARGET_LOSS_AMT ~", paste(model_terms, collapse = " + ")))

severity_lm <- lm(
    severity_formula,
    data = severity_data
)

# Backward Selection
final_severity_model <- step(
    severity_lm,
    direction = "backward",
    trace = 0
)

cat("\n=== Severity Model (Linear) Top Variables ===\n")
severity_vars <- coef(final_severity_model) %>%
    sort(decreasing = TRUE) %>%
    head(5) %>%
    as.data.frame()
colnames(severity_vars) <- "Coefficient"
print(severity_vars)

# predict probability of default
hmeq_test$prob_default <- predict(best_logit_model, hmeq_test, type = "response")

# predict loss given default
hmeq_test$pred_loss <- 0 # default value

# Use probability threshold since we might not have actual default flag in test data
threshold <- 0.5 # adjust based on your business requirements
predicted_defaults <- which(hmeq_test$prob_default >= threshold)

if (length(predicted_defaults) > 0) {
    hmeq_test$pred_loss[predicted_defaults] <- predict(
        final_severity_model,
        newdata = hmeq_test[predicted_defaults, ]
    )
}

# calculate expected loss
hmeq_test$expected_loss <- hmeq_test$prob_default * hmeq_test$pred_loss

# handle actual loss values
actual_loss <- ifelse(
    is.na(hmeq_test$TARGET_LOSS_AMT),
    0,
    hmeq_test$TARGET_LOSS_AMT
)

# calculate RMSE
rmse_ps <- sqrt(mean((actual_loss - hmeq_test$expected_loss)^2, na.rm = TRUE))
cat("\nProbability/Severity Model RMSE:", round(rmse_ps, 2), "\n")

# model comparison
comparison <- data.frame(
    Model = c("Probability/Severity", "Step3 Best Model"),
    RMSE = c(rmse_ps, step3_best_rmse)
)
cat("\n=== Model Comparison ===\n")
print(comparison)

# visualization
library(ggplot2)
ggplot(hmeq_test, aes(x = expected_loss, y = actual_loss)) +
    geom_point(alpha = 0.6, color = "green") +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(
        title = "Actual vs Predicted Loss (Probability/Severity Model)",
        x = "Predicted Expected Loss",
        y = "Actual Loss Amount"
    ) +
    theme_minimal() +
    coord_equal()
