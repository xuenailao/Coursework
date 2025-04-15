# Q2.3 Calculate Total Sum of Squares (SST)
n <- 500  # Half the size of the dataset
s_y2 <- 9.24  # Sample variance of Y2i
SST <- (n - 1) * s_y2
SST  

# Q2.4 Calculate Error Sum of Squares (SSE)
RSE <- 1.008  # Residual standard error
SSE <- (RSE^2) * (n - 2)
SSE 

# Q2.5 Calculate Regression Sum of Squares (SSR)
SSR <- SST - SSE
SSR  

# Q2.6 Calculate R^2 and sample correlation coefficient ρ
R2 <- SSR / SST
R2  

rho_hat <- sqrt(R2)
rho_hat 

# Q2.7 Calculate β̂1 and β̂2
mean_y2 <- 15.03  # Sample mean of Y2i
mean_x2 <- 5.04   # Sample mean of X2i
s_x2 <- 8.62      # Sample variance of X2i

cov_xy <- rho_hat * sqrt(s_x2 * s_y2)
beta2_hat <- cov_xy / s_x2
beta2_hat  

beta1_hat <- mean_y2 - beta2_hat * mean_x2
beta1_hat  

# Q2.8 Calculate mean of the fitted values Ŷ2i
mean_fitted_y2 <- mean_y2  # Mean of fitted values equals mean of Y2i
mean_fitted_y2  

# Q2.9 Calculate mean of the residuals
mean_residuals <- 0  
mean_residuals  

