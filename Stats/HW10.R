# Q1.1
# Load necessary library
library(AER)
data("USMacroG", package = "AER")
USMacroG=na.omit(as.data.frame((apply(USMacroG,2,diff))))
# Fit the regression model
model_1_1 <- lm(consumption ~ gdp + invest + government + dpi + cpi + m1 + tbill + unemp + population + inflation + interest, data = USMacroG)
summary(model_1_1)


#Q1.2
# Load required libraries
library(leaps)

# Run best subset selection on the model
best_subset <- regsubsets(consumption ~ gdp + invest + government + dpi + cpi + m1 + tbill + unemp + population + inflation + interest, data = USMacroG, nbest = 1)

# Get summary for Cp criterion
subset_summary <- summary(best_subset)

# Find the model with the minimum Cp
best_model_index <- which.min(subset_summary$cp)
best_model_vars <- names(coef(best_subset, best_model_index))

# Display the selected variables and coefficients of the t_summbest model based on Cp
best_model_coefficients <- coef(best_subset, best_model_index)
best_model_coefficients


#Q1.4
# Using Adjusted R-squared for best subset selection
selected_model_adj_r2 <- which.max(subset_summary$adjr2)
selected_variables_adj_r2 <- names(coef(best_subset, selected_model_adj_r2))
selected_variables_adj_r2

# Using Adjusted R-squared for best subset selection
best_model_adj_r2_index <- which.max(subset_summary$adjr2)
best_model_adj_r2_vars <- names(coef(best_subset, best_model_adj_r2_index))
best_model_adj_r2_coefficients <- coef(best_subset, best_model_adj_r2_index)
best_model_adj_r2_coefficients


#Q1.5
# Plot residuals to check for non-linearity and constant variance
# Fit the model based on the variables selected by Cp criterion in Q1.2
formula_best_model <- as.formula(paste("consumption ~", paste(best_model_vars[-1], collapse = " + ")))
best_model_cp <- lm(formula_best_model, data = USMacroG)

# Generate the residual plot for the best model
plot(best_model_cp$residuals, main = "Residuals of Best Model Based on Cp Criterion", xlab = "Index", ylab = "Residuals")
abline(h = 0, col = "red")

#Q1.6
# Generate ACF plot of residuals
acf(best_model_cp$residuals)


#Q1.7
# Calculate standardized residuals and leverage values
standardized_residuals <- rstandard(best_model_cp)
leverages <- hatvalues(best_model_cp)
avg_leverage <- mean(leverages)

# Identify outliers and high leverage points
outliers <- which(abs(standardized_residuals) > 3)
high_leverage_points <- which(leverages > 2 * avg_leverage)
intersection <- intersect(outliers, high_leverage_points)

avg_leverage
outliers
high_leverage_points
intersection

# Plot standardized residuals
plot(standardized_residuals, main = "Standardized Residuals")
abline(h = c(-3, 3), col = "red")




#Q2.1
# Load ISLR package and data
library(ISLR)
data("Credit")

# Convert Student to binary (0 = Yes, 1 = No)
Credit$Student <- ifelse(Credit$Student == "Yes", 0, 1)

# Fit the model
model_2_1 <- lm(Balance ~ Income + Limit + Rating + Cards + Age + Student, data = Credit)
summary(model_2_1)


#Q2.2
# Add the interaction term and fit the model
model_2_2 <- lm(Balance ~ Income + Limit + Rating + Cards + Age + Student + Limit:Student, data = Credit)
summary(model_2_2)


#Q2.3
# Add second-order terms and fit the model
model_2_3 <- lm(Balance ~ Income + I(Income^2) + Limit + I(Limit^2) + Rating + Cards + Age + Student + Limit:Student, data = Credit)
summary(model_2_3)




# Load required library
library(Ecdat)
library(forecast)
data(Capm)
r = Capm$rf
dr = diff(r)

# Fit ARIMA model to the time series
fit <- auto.arima(dr)

# Forecast the next period (Dec 2002 to Jan 2003) with 90% confidence interval
forecast_result <- forecast(fit, h = 1, level = 90)

# Print the forecasted value
cat("Forecasted Monthly Change (Dec 2002 to Jan 2003):\n")
print(forecast_result$mean)

# Assume the last observed interest rate in Dec 2002 is `last_rate`
# Replace `last_rate` with the actual value
last_rate <- 0.11 
forecasted_interest_rate <- last_rate + forecast_result$mean
cat("Forecasted Interest Rate for Jan 2003:\n")
print(forecasted_interest_rate)

# 90% Prediction Interval
cat("90% Prediction Interval for Monthly Change:\n")
print(forecast_result$lower[1])  # Lower bound
print(forecast_result$upper[1])  # Upper bound

cat("90% Prediction Interval for Interest Rate:\n")
lower_bound_interest <- last_rate + forecast_result$lower[1]
upper_bound_interest <- last_rate + forecast_result$upper[1]
print(c(lower_bound_interest, upper_bound_interest))


# 设置随机种子，便于结果重现
set.seed(123)

# 模拟一个 MA(2) 模型
library(forecast)

# 定义模型参数
mu <- 0.1               # 长期均值
theta1 <- 0.5           # MA(1) 系数
theta2 <- -0.3          # MA(2) 系数
sigma_epsilon <- 1       # 白噪声的标准差

# 生成白噪声
n <- 500                # 模拟样本数
epsilon <- rnorm(n, mean = 0, sd = sigma_epsilon)

# 构造 MA(2) 序列
Y <- numeric(n)
for (t in 3:n) {
  Y[t] <- mu + epsilon[t] + theta1 * epsilon[t - 1] + theta2 * epsilon[t - 2]
}

# 绘制模拟的时间序列
plot(Y, type = "l", main = "Simulated MA(2) Time Series", ylab = "Y", xlab = "Time")

# 使用 auto.arima 拟合模型
fit <- auto.arima(Y)

# 长期预测
h <- 100  # 预测步数
forecast_result <- forecast(fit, h = h)

# 绘制预测结果
plot(forecast_result, main = "Forecast of MA(2) Model")

summary(fit)


