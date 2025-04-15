#data <- read.csv("ETFreturns.csv", header=TRUE)
simpleRet <- ETFreturns$SPY[4789:5788]
ret <- na.omit(log(1 + simpleRet))

# 8(a)
library(tseries)
garch_fit_tseries <- garch(ret, order = c(1,1))
summary(garch_fit_tseries)
coef(garch_fit_tseries)

#8(b)
library(fGarch)

garch_fit_fgarch <- garchFit(
  formula = ~ garch(1,1),  
  data = ret,
  trace = FALSE            
)


summary(garch_fit_fgarch)

coef(garch_fit_fgarch)


#9(a)
# Load necessary packages
library(stats)

# Define the negative log-likelihood function for NGARCH(1,1)
negLogLik_ngarch <- function(params, ret) {
  alpha  <- params[1]  # ARCH coefficient
  beta   <- params[2]  # GARCH coefficient
  theta  <- params[3]  # Asymmetry parameter
  sigma  <- params[4]  # Constant term
  sigma1 <- params[5]  # Initial variance
  
  n <- length(ret)
  h <- numeric(n)
  h[1] <- sigma1  # Set initial variance
  
  # Ensure parameters are within valid bounds
  if (alpha <= 0 || beta < 0 || sigma <= 0 || sigma1 <= 0) {
    return(1e10)  # Large penalty if constraints are violated
  }
  
  loglik <- 0
  for (t in 2:n) {
    h[t] <- sigma + alpha * (ret[t-1] - theta * sqrt(h[t-1]))^2 + beta * h[t-1]
    if (h[t] <= 0) return(1e10)  # Avoid negative variances
    
    # Log-likelihood function
    loglik <- loglik + 0.5 * (log(2 * pi) + log(h[t]) + (ret[t]^2 / h[t]))
  }
  
  return(loglik)  # Negative log-likelihood
}

# Load SPY return data (assumed to be loaded in variable 'ret')
# ret <- log(1 + data$SPY_returns[4789:5788])  # Example conversion if using simple returns

# Initial parameter guesses
init_params <- c(alpha = 0.1, beta = 0.85, theta = 0.05, sigma = 0.00001, sigma1 = var(ret))

# Optimize using BFGS method
fit_ngarch <- optim(
  par       = init_params,
  fn        = negLogLik_ngarch,
  ret       = ret,
  method    = "BFGS",
  hessian   = TRUE
)

# Extract results
ngarch_params <- fit_ngarch$par
neg_loglik_value <- fit_ngarch$value

# Display results
cat("Estimated Parameters:\n")
cat("Alpha  =", ngarch_params[1], "\n")
cat("Beta   =", ngarch_params[2], "\n")
cat("Theta  =", ngarch_params[3], "\n")
cat("Sigma  =", ngarch_params[4], "\n")
cat("Sigma1 =", ngarch_params[5], "\n")
cat("Negative Log-Likelihood =", neg_loglik_value, "\n")


#9(b)
# Define a restricted model with theta = 0
negLogLik_ngarch_null <- function(params, ret) {
  alpha  <- params[1]
  beta   <- params[2]
  sigma  <- params[3]
  sigma1 <- params[4]
  
  n <- length(ret)
  h <- numeric(n)
  h[1] <- sigma1
  
  if (alpha <= 0 || beta < 0 || sigma <= 0 || sigma1 <= 0) {
    return(1e10)
  }
  
  loglik <- 0
  for (t in 2:n) {
    h[t] <- sigma + alpha * ret[t-1]^2 + beta * h[t-1]
    if (h[t] <= 0) return(1e10)
    loglik <- loglik + 0.5 * (log(2 * pi) + log(h[t]) + (ret[t]^2 / h[t]))
  }
  
  return(loglik)
}

# Initial guesses for restricted model (theta = 0)
init_params_null <- c(alpha = 0.1, beta = 0.85, sigma = 0.00001, sigma1 = var(ret))

# Optimize the restricted model
fit_ngarch_null <- optim(
  par       = init_params_null,
  fn        = negLogLik_ngarch_null,
  ret       = ret,
  method    = "BFGS",
  hessian   = TRUE
)

# Compute likelihood ratio test statistic
neg_loglik_null <- fit_ngarch_null$value
LR_stat <- 2 * (neg_loglik_null - neg_loglik_value)
p_value <- 1 - pchisq(LR_stat, df = 1)

# Display results
cat("Likelihood Ratio Test for θ = 0:\n")
cat("Log-Likelihood (Restricted Model) =", -neg_loglik_null, "\n")
cat("Log-Likelihood (Full Model) =", -neg_loglik_value, "\n")
cat("LR Statistic =", LR_stat, "\n")
cat("p-value =", p_value, "\n")

# Interpretation
if (p_value < 0.05) {
  cat("Reject the null hypothesis: θ is significantly different from 0.\n")
} else {
  cat("Fail to reject the null hypothesis: No strong evidence that θ is different from 0.\n")
}

#10(a)
# Define the negative log-likelihood function for ARCH(1)
negLogLik_arch <- function(params, ret) {
  alpha  <- params[1]  # ARCH coefficient
  sigma  <- params[2]  # Constant term
  sigma1 <- params[3]  # Initial variance
  
  n <- length(ret)
  h <- numeric(n)
  h[1] <- sigma1  # Initial variance
  
  if (alpha <= 0 || sigma <= 0 || sigma1 <= 0) {
    return(1e10)  # Penalty for invalid parameters
  }
  
  loglik <- 0
  for (t in 2:n) {
    h[t] <- sigma + alpha * ret[t-1]^2  # ARCH(1) variance equation
    if (h[t] <= 0) return(1e10)
    loglik <- loglik + 0.5 * (log(2 * pi) + log(h[t]) + (ret[t]^2 / h[t]))
  }
  
  return(loglik)  # Negative log-likelihood
}

# Initial parameter guesses
init_params_arch <- c(alpha = 0.1, sigma = 1e-5, sigma1 = var(ret))

# Optimize using BFGS method
fit_arch <- optim(
  par       = init_params_arch,
  fn        = negLogLik_arch,
  ret       = ret,
  method    = "BFGS",
  hessian   = TRUE
)

# Extract results
arch_params <- fit_arch$par
neg_loglik_arch <- fit_arch$value

# Display results
cat("Estimated ARCH(1) Parameters:\n")
cat("Alpha  =", arch_params[1], "\n")
cat("Sigma  =", arch_params[2], "\n")
cat("Sigma1 =", arch_params[3], "\n")
cat("Negative Log-Likelihood =", neg_loglik_arch, "\n")


#10(b)
# Assume neg_loglik_garch is from GARCH(1,1) estimation in Question 6(a)
neg_loglik_garch <- -4025.208  # Replace with actual value

# Compute likelihood ratio test statistic
LR_stat <- 2 * (neg_loglik_arch - neg_loglik_garch)
p_value <- 1 - pchisq(LR_stat, df = 1)

# Display results
cat("Likelihood Ratio Test for β = 0:\n")
cat("Log-Likelihood (ARCH(1)) =", -neg_loglik_arch, "\n")
cat("Log-Likelihood (GARCH(1,1)) =", -neg_loglik_garch, "\n")
cat("LR Statistic =", LR_stat, "\n")
cat("p-value =", p_value, "\n")

# Interpretation
if (p_value < 0.05) {
  cat("Reject the null hypothesis: β is significantly different from 0, favoring GARCH(1,1).\n")
} else {
  cat("Fail to reject the null hypothesis: No strong evidence that β is different from 0.\n")
}

