library(car)
library(ggplot2)

car_data <- read_excel("/Users/aawaizali/Library/Mobile Documents/com~apple~CloudDocs/Code/pynb/practice/Car-Price-Prediction-Model/final_data.xlsx")
names(car_data) <- make.names(names(car_data), unique = TRUE)

# Convert Body Type to a factor
car_data$Body.Type <- as.factor(car_data$Body.Type)

# Regression Model 1: Price ~ Body Type
model1 <- lm(Price ~ Body.Type, data = car_data)
# summary(model1)

# Regression Model 2: Price ~ Displacement
model2 <- lm(Price ~ Displacement, data = car_data)
# summary(model2)

# Plotting
# Scatter plot for Price vs Displacement
ggplot(car_data, aes(x = Displacement, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Scatter Plot of Price vs Displacement", x = "Displacement (cc)", y = "Price")

# Residual plot for Price ~ Body Type
plot(model1$residuals ~ fitted(model1),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residual Plot for Price ~ Body Type")
abline(h = 0, col = "red")

# Residual plot for Price ~ Displacement
plot(model2$residuals ~ fitted(model2),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residual Plot for Price ~ Displacement")
abline(h = 0, col = "red")


# Influence plot for Price ~ Body Type
influencePlot(model1, id.method = "identify", main = "Influence Plot for Price ~ Body Type")

# Influence plot for Price ~ Displacement
influencePlot(model2, id.method = "identify", main = "Influence Plot for Price ~ Displacement")


# Scatter plot for Price vs Displacement with regression line
ggplot(car_data, aes(x = Displacement, y = Price)) +
  geom_point(aes(color = Body.Type), alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Price vs Displacement by Body Type", x = "Displacement (cc)", y = "Price") +
  scale_color_discrete(name = "Body Type")


# Density plot for Price
ggplot(car_data, aes(x = Price)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Car Prices", x = "Price", y = "Density")


# Adding predicted prices to the data frame
car_data$Predicted.Price <- predict(model1, newdata = car_data)

# Boxplot of actual vs. predicted prices by body type
ggplot(car_data, aes(x = Body.Type, y = Price)) +
  geom_boxplot(aes(fill = "Actual"), alpha = 0.6) +
  geom_boxplot(aes(y = Predicted.Price, fill = "Predicted"), alpha = 0.6) +
  labs(title = "Actual vs Predicted Prices by Body Type", x = "Body Type", y = "Price") +
  scale_fill_manual(values = c("Actual" = "blue", "Predicted" = "red"))


# Unique body types for predictions
body_types <- unique(car_data$Body.Type)
data_for_prediction1 <- data.frame(Body.Type = body_types)

# Predict prices based on body type
predicted_prices_body_type <- predict(model1, newdata = data_for_prediction1)
data_for_prediction1$Predicted_Price <- predicted_prices_body_type

# Display predictions for different body types
print(data_for_prediction1)

displacement_values <- data.frame(Displacement = c(1000, 1500, 2000, 2500, 3000))
predicted_prices_displacement <- predict(model2, newdata = displacement_values)
displacement_values$Predicted_Price <- predicted_prices_displacement

# Display predictions for different displacements
print(displacement_values)



