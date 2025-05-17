#  Load the required libraries
install.packages("readxl")  # Run if not installed
install.packages("ggplot2")  # Run if not installed
library(readxl)
library(ggplot2)

# Load the dataset
data <- read_excel("file_show.xlsx")
View(data)  # View the dataset in RStudio

# Convert categorical variables to factors
data$sex <- as.factor(data$sex)
data$ca <- as.factor(data$ca)
data$thal <- as.factor(data$thal)
data$ChestPainType <- as.factor(data$ChestPainType)

# Fit the logistic regression model
reg1 <- glm(target ~ Age + sex + ChestPainType + Chol + MaxHR + oldpeak + ca + thal, 
            data = data, family = binomial)
summary_reg1 <- summary(reg1)

# Print summary of the regression model
print(summary_reg1)

# Predict probabilities
data$predicted_prob <- predict(reg1, type = "response")


# Scatterplot of Age vs Heart Disease
ggplot(data, aes(x = Age, y = target)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(title = "Scatterplot of Age vs Heart Disease",
       x = "Age",
       y = "Heart disease")
# Scatterplot of Sex vs Heart Disease
ggplot(data, aes(x = sex, y = target)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(title = "Scatterplot of Sex vs Heart Disease",
       x = "Sex",
       y = "Heart disease")

# Scatterplot of Chest Pain Type vs Heart Disease
ggplot(data, aes(x = ChestPainType, y = target)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(title = "Scatterplot of Chest Pain Type vs Heart Disease",
       x = "Chest Pain Type",
       y = "Heart disease")

# Scatterplot of Cholesterol vs Heart Disease
ggplot(data, aes(x = Chol, y = target)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(title = "Scatterplot of Cholesterol vs Heart Disease",
       x = "Cholesterol",
       y = "Heart disease")

# Scatterplot of Oldpeak vs Heart Disease
ggplot(data, aes(x = oldpeak, y = target)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(title = "Scatterplot of Oldpeak vs Heart Disease",
       x = "Oldpeak",
       y = "Heart disease")

# Scatterplot of Major Vessels vs Heart Disease
ggplot(data, aes(x = ca, y = target)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(title = "Scatterplot of Major Vessels vs Heart Disease",
       x = "Major Vessels",
       y = "Heart disease")

# Scatterplot of Thalassemia vs Heart Disease
ggplot(data, aes(x = thal, y = target)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(title = "Scatterplot of Thalassemia vs Heart Disease",
       x = "Thalassemia",
       y = "Heart disease")

# Scatterplot of Maximum Heart Rate vs Heart Disease
# Scatterplot of Maximum Heart Rate vs Heart Disease
ggplot(data, aes(x = MaxHR, y = target)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(title = "Scatterplot of Maximum Heart Rate vs Heart Disease",
       x = "Maximum Heart Rate",
       y = "Heart disease")


# Summary of the target variable
summary(data$target)
