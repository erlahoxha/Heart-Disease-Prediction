# Load the necessary library
install.packages("modeest")
library(modeest)
install.packages("readxl")
library(readxl)

data <- read_excel("file_show.xlsx") 

# Function to calculate mode
calculate_mode <- function(x) {
  if(length(unique(x)) == length(x)) {
    return("No mode")
  } else {
    return(mlv(x, method = "mfv"))
  }
}

# Function to calculate descriptive statistics
calculate_stats <- function(x) {
  stats <- c(
    Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Mode = calculate_mode(x),
    Range = max(x, na.rm = TRUE) - min(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    Standard_Deviation = sd(x, na.rm = TRUE),
    Variance = var(x, na.rm = TRUE),
    IQR = IQR(x, na.rm = TRUE)
  )
  return(stats)
}

# Calculate descriptive statistics
summary_stats <- sapply(data, calculate_stats)

# Print the descriptive statistics
print(summary_stats)
