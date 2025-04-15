#1.1
# Set seed for reproducibility
set.seed(1)

# Generate time series data
t <- seq(1, 100, by=1)  # time from 1 to 100
X <- 10 + t + rnorm(100)
Y1 <- 20 + 2 * t + rnorm(100)
Y2 <- 20 + 2 * t + 5 * X + rnorm(100)

# Create data frame for convenience
data <- data.frame(X, Y1, Y2)

# Regress Y1 on X and get R-squared
model_Y1_X <- lm(Y1 ~ X, data = data)
r_squared_Y1 <- summary(model_Y1_X)$r.squared * 100  # as percentage

# Regress Y2 on X and get R-squared
model_Y2_X <- lm(Y2 ~ X, data = data)
r_squared_Y2 <- summary(model_Y2_X)$r.squared * 100  # as percentage

# Print results
cat("R-squared for Y1 ~ X:", round(r_squared_Y1, 2), "%\n")
cat("R-squared for Y2 ~ X:", round(r_squared_Y2, 2), "%\n")




#1.2
# Set seed for reproducibility
set.seed(1)

# Generate time series data
t <- seq(1, 100, by=1)  # time from 1 to 100
X <- 10 + t + rnorm(100)
Y1 <- 20 + 2 * t + rnorm(100)
Y2 <- 20 + 2 * t + 5 * X + rnorm(100)

# Differencing the variables
delta_X <- diff(X)
delta_Y1 <- diff(Y1)
delta_Y2 <- diff(Y2)

# Regress delta_Y1 on delta_X and get R-squared
model_delta_Y1_X <- lm(delta_Y1 ~ delta_X)
r_squared_delta_Y1 <- summary(model_delta_Y1_X)$r.squared * 100  # as percentage

# Regress delta_Y2 on delta_X and get R-squared
model_delta_Y2_X <- lm(delta_Y2 ~ delta_X)
r_squared_delta_Y2 <- summary(model_delta_Y2_X)$r.squared * 100  # as percentage

# Print results
cat("R-squared for delta_Y1 ~ delta_X:", round(r_squared_delta_Y1, 2), "%\n")
cat("R-squared for delta_Y2 ~ delta_X:", round(r_squared_delta_Y2, 0), "%\n")




#2.1
# Regression before differencing
library(AER)
data('USMacroG')

model_before <- lm(consumption ~ cpi, data = USMacroG)
r_squared_before <- summary(model_before)$r.squared * 100  # as percentage

# Differencing the variables
USMacroG_diff <- as.data.frame(apply(USMacroG, 2, diff))
colnames(USMacroG_diff) <- colnames(USMacroG)  # retain original column names

# Regression after differencing
model_after <- lm(consumption ~ cpi, data = USMacroG_diff)
r_squared_after <- summary(model_after)$r.squared * 100  # as percentage

# Print results
cat("R-squared before differencing:", round(r_squared_before, 0), "%\n")
cat("R-squared after differencing:", round(r_squared_after, 1), "%\n")




#2.2
USMacroG_diff <- na.omit(as.data.frame(apply(USMacroG, 2, diff)))
colnames(USMacroG_diff) <- colnames(USMacroG)  # retain original column names

# Scatterplot for quarterly change in consumption and gdp
plot(USMacroG_diff$gdp, USMacroG_diff$consumption,
     xlab = "Quarterly Change in GDP",
     ylab = "Quarterly Change in Consumption",
     main = "Scatterplot of Consumption vs GDP (Quarterly Changes)")

# Regression: Consumption (dependent) on GDP (independent)
model <- lm(consumption ~ gdp, data = USMacroG_diff)

# Display regression summary
summary(model)

# Extracting regression equation parameters
intercept <- coef(model)[1]
slope <- coef(model)[2]
sigma_estimate <- summary(model)$sigma
degrees_of_freedom <- model$df.residual

# Adding regression line to the plot
abline(model, col = "blue")

# Displaying the results
cat("Estimated Regression Equation: Consumption = ", round(intercept, 4), " + ", round(slope, 4), " * GDP\n")
cat("Estimate for σ (Standard Error of Residuals): ", round(sigma_estimate, 4), "\n")
cat("Degrees of Freedom: ", degrees_of_freedom, "\n")




#2.3
# Regression: Consumption (dependent) on GDP (independent)
model <- lm(consumption ~ gdp, data = USMacroG_diff)

# Extracting the estimated standard error for β_gdp
se_beta_gdp <- summary(model)$coefficients["gdp", "Std. Error"]

# Calculating the 99% confidence interval
# Critical value for 99% CI with two-tailed test
alpha <- 0.01
t_critical <- qt(1 - alpha/2, df = model$df.residual)
beta_gdp <- coef(model)["gdp"]
lower_bound <- beta_gdp - t_critical * se_beta_gdp
upper_bound <- beta_gdp + t_critical * se_beta_gdp

# Displaying the results
cat("Estimated Standard Error for β_gdp:", round(se_beta_gdp, 4), "\n")
cat("99% Confidence Interval for β_gdp: [", round(lower_bound, 3), ", ", round(upper_bound, 3), "]\n")

# Checking statistical evidence for β_gdp ≠ 0
p_value <- summary(model)$coefficients["gdp", "Pr(>|t|)"]
evidence <- ifelse(p_value < alpha, "Yes", "No")

cat("Is there strong enough statistical evidence that β_gdp ≠ 0? ", evidence, "\n")




#2.4
# Fit the regression model: Consumption (dependent) on GDP (independent)
model <- lm(consumption ~ gdp, data = USMacroG_diff)

# Extracting R-squared
r_squared <- summary(model)$r.squared

# Calculating SST, SSR, and SSE
SST <- sum((USMacroG_diff$consumption - mean(USMacroG_diff$consumption))^2)  # Total sum of squares
SSR <- sum((fitted(model) - mean(USMacroG_diff$consumption))^2)               # Regression sum of squares
SSE <- sum(residuals(model)^2)                                                # Error sum of squares

# Calculating variation explained by GDP
variation_explained <- r_squared * 100  # as percentage

# Displaying results
cat("SSR:", round(SSR, 0), "\n")
cat("SSE:", round(SSE, 0), "\n")
cat("SST:", round(SST, 0), "\n")
cat("R-squared:", round(r_squared * 100, 0), "\n")
cat("Variation explained by GDP (as percentage):", round(variation_explained, 0), "\n")




#2.5
n <- nrow(USMacroG_diff)
model_partial <- lm(consumption ~ gdp, data = USMacroG_diff[1:(n-1), ])

# Predict consumption for the nth observation using gdp[n]
new_data <- data.frame(gdp = USMacroG_diff$gdp[n])
prediction <- predict(model_partial, newdata = new_data, interval = "prediction", level = 0.95)

# Extract prediction and confidence interval bounds
predicted_value <- prediction[1, "fit"]
lower_bound <- prediction[1, "lwr"]
upper_bound <- prediction[1, "upr"]

# Calculate the actual value of consumption[n] and prediction error
actual_value <- USMacroG_diff$consumption[n]
prediction_error <- actual_value - predicted_value

# Displaying results
cat("Prediction (rounded):", round(predicted_value, 0), "\n")
cat("95% Prediction Interval - Lower Bound:", round(lower_bound, 1), "\n")
cat("95% Prediction Interval - Upper Bound:", round(upper_bound, 1), "\n")
cat("Prediction Error (rounded):", round(prediction_error, 0), "\n")



#2.6

# Regress consumption on tbill, inflation, and cpi
model_multiple <- lm(consumption ~ tbill + inflation + cpi, data = USMacroG_diff)

# Summary of the regression model
summary_model <- summary(model_multiple)

# Extract F-statistic and its p-value
f_statistic <- summary_model$fstatistic
f_stat_p_value <- pf(f_statistic[1], f_statistic[2], f_statistic[3], lower.tail = FALSE)

# Check if the null hypothesis is rejected
significance_level <- 0.05
is_null_rejected <- ifelse(f_stat_p_value < significance_level, "Yes", "No")
f_stat_p_value
# Display results
cat("Null Hypothesis of the F-test: β_tbill = β_inflation = β_cpi = 0\n")
cat("Is the null hypothesis rejected? ", is_null_rejected, "\n")

# Interpretation
if (is_null_rejected == "Yes") {
  cat("Interpretation: At least one of tbill, inflation, or cpi is significantly related to consumption, meaning the model is useful in explaining consumption.\n")
} else {
  cat("Interpretation: There is insufficient evidence to conclude that tbill, inflation, or cpi are significantly related to consumption.\n")
}




#2.7
# Step 1: Simple linear regression of consumption on m1
model_linear <- lm(consumption ~ m1, data = USMacroG_diff)
summary_linear <- summary(model_linear)

# Check significance of m1 (p-value for m1 coefficient)
p_value_m1 <- summary_linear$coefficients["m1", "Pr(>|t|)"]
significance_level <- 0.05
is_m1_significant <- ifelse(p_value_m1 < significance_level, "Yes", "No")

# Step 2: Quadratic regression of consumption on m1 and m1^2
USMacroG_diff$m1_squared <- USMacroG_diff$m1^2
model_quadratic <- lm(consumption ~ m1 + m1_squared, data = USMacroG_diff)
summary_quadratic <- summary(model_quadratic)

# Check if adding m1^2 improves the model by comparing adjusted R-squared values
improvement <- ifelse(summary_quadratic$adj.r.squared > summary_linear$adj.r.squared, "Yes", "No")

# Step 3: Plot scatter plot and add regression lines
plot(USMacroG_diff$m1, USMacroG_diff$consumption,
     xlab = "Quarterly Change in M1",
     ylab = "Quarterly Change in Consumption",
     main = "Consumption vs M1 (Quarterly Changes)")

# Add linear regression line
abline(model_linear, col = "blue", lwd = 2)

# Add quadratic regression curve
curve(predict(model_quadratic, newdata = data.frame(m1 = x, m1_squared = x^2)), 
      add = TRUE, col = "red", lwd = 2)

# Display results
cat("Is m1 significant in the simple linear regression? ", is_m1_significant, "\n")
cat("Does adding a second-order term improve the fitting? ", improvement, "\n")


#2.8
# Step 1: Regress consumption on all other 11 variables
all_vars <- names(USMacroG_diff)[!names(USMacroG_diff) %in% "consumption"]
formula <- as.formula(paste("consumption ~", paste(all_vars, collapse = " + ")))
model_all <- lm(formula, data = USMacroG_diff)

# Extract adjusted R^2
adjusted_r_squared <- summary(model_all)$adj.r.squared * 100  # as percentage

# Check significance of m1 in this model
p_value_m1 <- summary(model_all)$coefficients["m1", "Pr(>|t|)"]
significance_level <- 0.05
is_m1_significant <- ifelse(p_value_m1 < significance_level, "Yes", "No")

# Display results
cat("Adjusted R-squared:", round(adjusted_r_squared, 0), "\n")
cat("Is m1 significant in the multiple regression model? ", is_m1_significant, "\n")
