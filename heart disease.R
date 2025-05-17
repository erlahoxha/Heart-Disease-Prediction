# Load the required libraries
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
library(readr)
library(dplyr)
library(ggplot2)

# Load the dataset
data <- read_excel("C:/Users/User/Downloads/heartdisease/heartdiseaseprediction/file_show.xlsx")
# Display the structure of the dataset
print("Data Structure:")
print(str(data))

# Handle missing values
# Check for any missing values in the dataset
print("\nMissing Values:")
print(colSums(is.na(data)))

# Key statistics
print("\nKey Statistics:")
print(summary(data))

# Separate statistics for patients with and without heart disease
print("\nStatistics for patients without heart disease:")
print(summary(data[data$target == 0, ]))

print("\nStatistics for patients with heart disease:")
print(summary(data[data$target == 1, ]))

# Plot histograms for numerical columns
numerical_cols <- select(data, where(is.numeric))

plot_histograms <- lapply(names(numerical_cols), function(col) {
  ggplot(data, aes(x = !!sym(col), fill = factor(target))) +
    geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
    labs(title = paste("Distribution of", col),
         x = col,
         y = "Density") +
    theme_minimal()
})

# Plot box plots for numerical columns
plot_boxplots <- lapply(names(numerical_cols), function(col) {
  ggplot(data, aes(x = factor(target), y = !!sym(col))) +
    geom_boxplot(fill = "lightblue", alpha = 0.5) +
    labs(title = paste("Box Plot of", col, "by Heart Disease"),
         x = "Heart Disease",
         y = col) +
    theme_minimal()
})

# Display the plotsd
print(plot_histograms)
