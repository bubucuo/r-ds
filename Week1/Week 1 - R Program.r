# Week 1: R Programming Assignment
# Author: Shao Yun Gao

# step 0
# load Data Set: iris
data(iris)

# Step 1: Describe the Data
# List the structure of the data (str)
str(iris)

# Execute a summary of the data
summary(iris)

# Print the first six records
head(iris)

# Step 2: Box-Whisker Plots
# Plot a box plot of the numeric variable split by the grouping variable.
# Sepal.Length | Species
boxplot(Sepal.Length ~ Species,
     data = iris,
     main = "Shao Yun Gao",
     col = c("red", "green", "orange"),
     notch = TRUE,
     xlab = "Species",
     ylab = "Sepal Length"
)

# Step 3: Histograms
# Plot a histogram of the numeric variable
# Sepal.Length
hist(iris$Sepal.Length,
     breaks = 16,
     col = "green", # set background color
     border = "gray", # set line color
     main = "Shao Yun Gao",
     xlab = "Sepal Length",
     probability = TRUE
)

# Superimpose a density line to the graph
lines(density(iris$Sepal.Length), col = "orange", lwd = 2)

# Step 4: Scatter Plots
# Scatter plot of Sepal.Length vs Sepal.Width colored by Species
palette(c("red", "green", "orange")) # set palette for scatter
plot(iris$Sepal.Length, iris$Sepal.Width,
     col = iris$Species,
     pch = 16,
     main = "Shao Yun Gao",
     xlab = "Sepal Length",
     ylab = "Sepal Width"
)

legend("topright",
     legend = levels(iris$Species),
     col = 1:3, pch = 16, title = "Species"
)

# Step 5: Simple Math Calculations
# For the numeric variable, compute the following statistics
# Sepal.Length
cat("Mean:", mean(iris$Sepal.Length), "\n")
cat("Median:", median(iris$Sepal.Length), "\n")
cat("Min:", min(iris$Sepal.Length), "\n")
cat("Max:", max(iris$Sepal.Length), "\n")
cat("Standard Deviation:", sd(iris$Sepal.Length), "\n")

# Calculate the Median for the numeric for each group member.
medianSpecies <- aggregate(Sepal.Length ~ Species, data = iris, FUN = median)
# Sort the result in Descending order.
sortedMedian <- medianSpecies[order(-medianSpecies$Sepal.Length), ]
# print the sorted result
print(sortedMedian)
