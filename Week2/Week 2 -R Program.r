#! Step 1: Read in the Data

# List the structure of the data (str)
hmeq <- read.csv("../HMEQ_WK02/HMEQ_Loss.csv")

# Execute a summary of the data
str(hmeq)

# Execute a summary of the data
summary(hmeq)

# Print the first six records
head(hmeq)


# ! Step 2: Box-Whisker Plots
# Plot a box plot of all the numeric variables split by the grouping variable. The plot needs the following:
# The MAIN TITLE of the box plot should be set to your name
# Add color to the boxes
# Comment on whether or not there are any observable differences in the box plots between the two groups.

library(ggplot2)
library(gridExtra)

numerics <- c(
  "LOAN", "MORTDUE", "VALUE", "YOJ", "DEROG", "DELINQ",
  "CLAGE", "NINQ", "CLNO", "DEBTINC"
)

# Plot a box plot(suppress warnings)
plots <- lapply(numerics, function(numerics) {
  ggplot(hmeq, aes(x = factor(TARGET_BAD_FLAG), y = .data[[numerics]], fill = factor(TARGET_BAD_FLAG))) +
    geom_boxplot(na.rm = TRUE) + # Explicitly remove NAs
    labs(title = "Shaoyun Gao", x = "TARGET_BAD_FLAG", y = numerics) +
    scale_fill_brewer(palette = "Set3") +
    theme_minimal()
})

# Arrange plots in grid
suppressWarnings(grid.arrange(grobs = plots, ncol = 3))


# ! Step 3: Histograms
# Plot a histogram of at least one of the numeric variables
# Manually set the number of breaks to a value that makes sense
# Superimpose a density line to the graph
ggplot(hmeq, aes(x = LOAN)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "green", color = "gray") +
  geom_density(color = "red") +
  ggtitle("Histogram of LOAN with Density Line by Shaoyun Gao") +
  xlab("Loan Amount") +
  ylab("Density") +
  theme_minimal()


# ! Step 4: Impute "Fix" all the numeric variables that have missing values
# For the missing Target variables, simply set the missing values to zero
# For the remaining numeric variables with missing values, create two new variables. One variable will have a name beginning with IMP_ and it will contained the imputed value. The second value will have a name beginning with M_ and it will contain a 1 if the record was imputed and a zero if it was not.
# You may impute with any method that makes sense. The median or mean value will be useful in most cases.
# Push yourself! Try one complex imputation like the one described in the lectures.
# Delete the original variable after it has been imputed.

impute_missing <- function(data, var) {
  if (any(is.na(data[[var]]))) {
    if (var == "TARGET_LOSS_AMT") {
      data[[var]][is.na(data[[var]])] <- 0
    } else {
      if (var %in% c("INCOME", "HOME_VAL")) {
        job_groups <- split(data, data$JOB)
        for (group in names(job_groups)) {
          median_value <- median(job_groups[[group]][[var]], na.rm = TRUE)
          data[[paste0("IMP_", var)]][data$JOB == group & is.na(data[[var]])] <- median_value
        }
      } else {
        median_value <- median(data[[var]], na.rm = TRUE)
        data[[paste0("IMP_", var)]][is.na(data[[var]])] <- median_value
      }
      data[[paste0("M_", var)]] <- as.numeric(is.na(data[[var]]))
      data[[var]] <- NULL
    }
  }
  return(data)
}

numerics <- names(hmeq)[sapply(hmeq, is.numeric)]
for (var in numerics) {
  hmeq <- impute_missing(hmeq, var)
}

# Run a summary to prove that all the variables have been imputed
summary(hmeq)

res_vars <- grep("^M_", names(hmeq), value = TRUE)
my_missing_count <- sapply(res_vars, function(var) sum(hmeq[[var]]))
my_missing_count



# ! Step 5: One Hot Encoding
# For the character / category variables, perform one hot encoding. For this create a Flag for each categories.
# Delete the original class variable
# Run a summary to show that the category variables have been replaced by Flag variables.

hot_encode <- function(data, var) {
  if (is.factor(data[[var]]) || is.character(data[[var]])) {
    levels <- levels(as.factor(data[[var]]))
    for (level in levels) {
      data[[paste0("FLAG_", var, "_", level)]] <- as.numeric(data[[var]] == level)
    }
    data[[var]] <- NULL
  }
  return(data)
}

chars <- names(hmeq)[sapply(hmeq, function(x) is.factor(x) || is.character(x))]
for (var in chars) {
  hmeq <- hot_encode(hmeq, var)
}

summary(hmeq)
