# simple example using the Women dataset
str(women)

# Aiming to predict weight from height
# Dependent variable = weight
# Independent variable = height

simple_linear_model <- lm(weight ~ height, data = women)
simple_linear_model

summary(simple_linear_model)

# weight = -87.52 + 3.45 * height

# ploting simple lm model onto data used to create model
plot(women$height, 
     women$weight, 
     main = "Scatter plot showing the regression line for 
     weight predict from height", 
     xlab = "Height", 
     ylab = "Weight")
abline(simple_linear_model, col = "blue")

# Analyse the correlation coefficient
# measures the level of association between 2 variable
# -1 = perfect negative correlation
# +1 = perfect positive correlation

# A value -0.02 < x < 0.02 suggests that much of the
# variation in outcome variable is not explained by the predictor
# Then we should look for better predictor variables
confint(simple_linear_model)

cor(women$height, women$weight)

# Model accuracy - goodness of fit
# 3 quantities
# Residual standard error (RSE)
# R-squared (r2)
# F-statistic

summary(simple_linear_model)

# RSE = 1.525 = prediction error rate
# When comparing 2 models, smallest RSE is best
# In this model, observed weight values deviate from the true
# regression by approx 1.5 units on average

# r2
# Ranges from 0 - 1
# High r2 = good indicator that model variability in the
# outcome can be explained by the model
# A number close to 0 - model does not explain much of the variability

# F-statistic
# Overall significance of the model
# a large F-statistic corresponds to a significant p-value
# (p < 0.05)

test_data <- 61
test_data <- data.frame(test_data)
colnames(test_data) <- "height"
predict(simple_linear_model, test_data, interval = "prediction")

# Build a model to predict distance from speed
# using the cars dataset

# 1st step - check model assumptions
# These are the core assumptions
# Linearity among the variables
# Normality - normal distribution
# No collinearity - vars are not a linear combination of others
# Independence - residuals are independent and not correlated

# check linearity first using scatter plot
# x-axis = independent var
# y-axis = dependent var
# If relationship exists then the linearity assumption is validated

scatter.smooth(cars$speed, 
               cars$dist, 
               main = "Distance - speed", 
               xlab = "Car speed (mph)", 
               ylab = "Stopping distance")

cor(cars$speed, cars$dist)

# Check for outliers
# An outlier = 1.5 * interquartile range
# IQR = distance between 25th and 75th percentile
# need to check speed and distance for outliers

opar <- par(no.readonly = TRUE)

par(mfrow = c(1,2))

attach(cars)
boxplot(speed, 
        main = "Speed", 
        sub = paste("Outlier rows: ", boxplot.stats(speed)$out))

boxplot(dist, 
        main = "Distance", 
        sub = paste("Outlier rows: ", boxplot.stats(dist)$out))

detach(cars)
par(opar)

# Remove line 120 because it is an outlier
cars <- subset(cars, cars$dist != 120)
