# Install and load necessary libraries
if (!require(readxl)) install.packages("readxl")
if (!require(moments)) install.packages("moments")

library(readxl)
library(moments)

# Read the data and clean column names
car_data <- read_excel("/Users/aawaizali/Library/Mobile Documents/com~apple~CloudDocs/Code/pynb/practice/Car-Price-Prediction-Model/final_data.xlsx")
names(car_data) <- make.names(names(car_data), unique = TRUE)

# Detailed descriptive statistics
price_stats <- summary(car_data$Price)
price_stats["variance"] <- var(car_data$Price)
price_stats["range"] <- range(car_data$Price)
price_stats["IQR"] <- IQR(car_data$Price)
price_stats["skewness"] <- skewness(car_data$Price)
price_stats["kurtosis"] <- kurtosis(car_data$Price)
price_stats["cv"] <- sd(car_data$Price) / mean(car_data$Price)

# Print the statistics
print(price_stats)

# Box plot of Body Type against Price
ggplot(car_data, aes(x = `Body.Type`, y = Price)) +
  geom_boxplot() +
  labs(title = "Box Plot of Prices by Body Type", x = "Body Type", y = "Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Histogram of Prices
ggplot(car_data, aes(x = Price)) +
  geom_histogram(bins = 30, color = "black", fill = "blue") +
  labs(title = "Histogram of Car Prices", x = "Price", y = "Frequency") +
  theme_minimal()

