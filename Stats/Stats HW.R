# 安装并加载必要的库
library(ISLR)

# 加载 Smarket 数据
data(Smarket)

# 导入 ISLRSmarketDates.csv 并替换 Smarket 第一列为对应的日期
dates <- read.csv("C:/Users/12194/Desktop/ISLRSmarketDates.csv", header = TRUE)
Smarket[, 1] <- dates$Date

# 绘制 Today 列的直方图，垂直轴为密度，并设置直方图条数为50
par(mfrow=c(1,2))
hist(Smarket$Today, breaks=50, freq=FALSE, main="Histogram of Today with Normal Fit", 
     xlab="Percentage Return", ylim=c(0, 0.35))
curve(dnorm(x, mean=mean(Smarket$Today), sd=sd(Smarket$Today)), 
      col="red", lwd=2, add=TRUE)

# 缩小右尾区域的直方图
hist(Smarket$Today, breaks=50, freq=FALSE, main="Zoomed Right Tail", 
     xlab="Percentage Return", xlim=c(2,6), ylim=c(0,0.1))
curve(dnorm(x, mean=mean(Smarket$Today), sd=sd(Smarket$Today)), 
      col="red", lwd=2, add=TRUE)


# 安装并加载必要的库
library(VGAM)

# 加载 Smarket 数据
data(Smarket)

# 使用 read.csv 正确读取 ISLRSmarketDates.csv 文件
dates <- read.csv("C:/Users/12194/Desktop/ISLRSmarketDates.csv", header = TRUE)
Smarket[, 1] <- dates$Date

# 估计 Laplace 分布的参数
mu <- median(Smarket$Today)
b <- mean(abs(Smarket$Today - mu))

# 绘制 Today 列的直方图，垂直轴为密度，并设置直方图条数为50
par(mfrow=c(1,2))
hist(Smarket$Today, breaks=50, freq=FALSE, main="Histogram of Today with Laplace Fit", 
     xlab="Percentage Return", ylim=c(0, 0.35))
curve(dlaplace(x, location=mu, scale=b), col="blue", lwd=2, add=TRUE)

# 缩小右尾区域的直方图
hist(Smarket$Today, breaks=50, freq=FALSE, main="Zoomed Right Tail with Laplace Fit", 
     xlab="Percentage Return", xlim=c(2,6), ylim=c(0,0.1))
curve(dlaplace(x, location=mu, scale=b), col="blue", lwd=2, add=TRUE)


# 绘制 Today 列的直方图，叠加使用不同带宽的 KDE 曲线
par(mfrow=c(1,1))
hist(Smarket$Today, breaks=50, freq=FALSE, main="Histogram of Today with KDEs", 
     xlab="Percentage Return", ylim=c(0, 0.35))

# 默认带宽的1/4
lines(density(Smarket$Today, bw="nrd0", adjust=0.25), col="blue", lwd=2)

# 默认带宽的4倍
lines(density(Smarket$Today, bw="nrd0", adjust=4), col="red", lwd=2)

# 添加图例
legend("topright", legend=c("1/4 Bandwidth", "4x Bandwidth"), 
       col=c("blue", "red"), lwd=2,cex=0.5)


# 绘制 Today 列的箱线图
boxplot(Smarket$Today, main="Boxplot of Today", ylab="Percentage Return")

# 找出最负的异常值
min_value <- min(Smarket$Today)
outlier_date <- subset(Smarket, Today == min_value)

# 输出最负异常值及其对应日期
min_value
outlier_date

# 计算相关矩阵，排除 Date 和 Direction 列
cor_matrix <- cor(Smarket[, -c(1, 9)])
cor_matrix

# 绘制散点图矩阵，排除 Date 和 Direction 列
pairs(Smarket[, -c(1, 9)], main="Scatterplot Matrix of Smarket Variables")


# 将日期转换为日期格式
Smarket$Date <- as.Date(Smarket$Year, format="%m/%d/%Y")

# 绘制交易量的时间序列图
plot(Smarket$Date, Smarket$Volume, type="l", main="Time Series Plot of Volume", 
     xlab="Date", ylab="Volume (in billions)", col="blue")


# 绘制 Today 列的正态分布和拉普拉斯分布的 QQ 图
par(mfrow=c(1,2))

# 正态分布 QQ 图
qqnorm(Smarket$Today, main="Normal Q-Q Plot")
qqline(Smarket$Today, col="red")

# 拉普拉斯分布 QQ 图
qqplot(qlaplace(ppoints(length(Smarket$Today)), location=median(Smarket$Today), scale=mean(abs(Smarket$Today - median(Smarket$Today)))), 
       Smarket$Today, main="Laplace Q-Q Plot", xlab="Theoretical Quantiles (Laplace)", ylab="Sample Quantiles")
abline(0, 1, col="red")

# 计算偏度和峰度
install.packages("moments")
library(moments)

# 计算 Today 列的样本偏度和峰度
sample_skewness <- skewness(Smarket$Today)
sample_kurtosis <- kurtosis(Smarket$Today)

# 输出偏度和峰度
sample_skewness
sample_kurtosis

# 加载必要的库
install.packages("VGAM")
library(VGAM)

# 绘制 Today 列的正态分布和拉普拉斯分布的概率密度图
par(mfrow=c(1,2))

# 绘制正态分布的概率密度图
hist(Smarket$Today, breaks=50, freq=FALSE, main="Normal Distribution Fit", xlab="Percentage Return")
curve(dnorm(x, mean=mean(Smarket$Today), sd=sd(Smarket$Today)), col="red", lwd=2, add=TRUE)

# 绘制拉普拉斯分布的概率密度图
hist(Smarket$Today, breaks=50, freq=FALSE, main="Laplace Distribution Fit", xlab="Percentage Return")
curve(dlaplace(x, location=median(Smarket$Today), scale=mean(abs(Smarket$Today - median(Smarket$Today)))), col="blue", lwd=2, add=TRUE)


# 1. 读取数据
data <- read.csv("C:/Users/12194/Desktop/TSLA.csv")

# 2. 计算日对数收益率
data$Log.Return <- c(NA, diff(log(data$Adj.Close)))

# 3. 删除第一个 NA 值（因为第一个对数收益率无法计算）
data <- na.omit(data)

# 4. 绘制正态概率图
qqnorm(data$Log.Return, main="Normal Probability Plot of TSLA Log Returns")
qqline(data$Log.Return, col = "red")

# 5. 检查是否符合正态分布
shapiro.test(data$Log.Return)  # Shapiro-Wilk 正态性检验


# Function to generate random values from N(0,1) using Box-Muller method
generate_normal <- function(n) {
  u1 <- runif(n/2)
  u2 <- runif(n/2)
  z1 <- sqrt(-2 * log(u1)) * cos(2 * pi * u2)
  z2 <- sqrt(-2 * log(u1)) * sin(2 * pi * u2)
  return(c(z1, z2))
}

# Function to estimate mu = E[cos(Z)] and calculate standard error and 95% CI
monte_carlo <- function(n) {
  Z <- generate_normal(n)
  cos_Z <- cos(Z)
  estimate <- mean(cos_Z)
  std_error <- sd(cos_Z) / sqrt(n)
  margin_of_error <- 1.96 * std_error
  lower_bound <- estimate - margin_of_error
  upper_bound <- estimate + margin_of_error
  return(list(estimate = estimate, std_error = std_error, lower_bound = lower_bound, upper_bound = upper_bound))
}

# Sample sizes to test
sample_sizes <- c(1000, 4000, 16000, 64000, 256000, 1024000)

# Store results for each sample size
results <- data.frame(Sample_Size = sample_sizes, Estimate = numeric(length(sample_sizes)),
                      Std_Error = numeric(length(sample_sizes)), Lower_CI = numeric(length(sample_sizes)),
                      Upper_CI = numeric(length(sample_sizes)))

# Perform Monte Carlo for each sample size
for (i in 1:length(sample_sizes)) {
  n <- sample_sizes[i]
  mc_result <- monte_carlo(n)
  results$Estimate[i] <- mc_result$estimate
  results$Std_Error[i] <- mc_result$std_error
  results$Lower_CI[i] <- mc_result$lower_bound
  results$Upper_CI[i] <- mc_result$upper_bound
}

# Print the results
print(results)




# Function to generate random values from N(0,1) using Box-Muller method
generate_normal <- function(n) {
  u1 <- runif(n/2)
  u2 <- runif(n/2)
  z1 <- sqrt(-2 * log(u1)) * cos(2 * pi * u2)
  z2 <- sqrt(-2 * log(u1)) * sin(2 * pi * u2)
  return(c(z1, z2))
}

# Function to estimate mu = E[cos(Z)] and calculate standard error
monte_carlo <- function(n) {
  Z <- generate_normal(n)
  cos_Z <- cos(Z)
  estimate <- mean(cos_Z)
  std_error <- sd(cos_Z) / sqrt(n)
  return(list(estimate = estimate, std_error = std_error))
}

# Sample sizes to test
sample_sizes <- c(1000, 4000, 16000, 64000, 256000, 1024000, 4096000, 16384000)

# Store results for each sample size
estimates <- numeric(length(sample_sizes))
standard_errors <- numeric(length(sample_sizes))
margin_of_errors <- numeric(length(sample_sizes))

# Perform Monte Carlo for each sample size
for (i in 1:length(sample_sizes)) {
  n <- sample_sizes[i]
  mc_result <- monte_carlo(n)
  estimates[i] <- mc_result$estimate
  standard_errors[i] <- mc_result$std_error
  margin_of_errors[i] <- 1.96 * mc_result$std_error
}

# Plot 1: Point estimates as n increases
plot(sample_sizes, estimates, type = "o", log = "x", xlab = "Sample Size (log scale)",
     ylab = "Point Estimate of E[cos(Z)]", main = "Convergence of Point Estimates as n Increases")

# Plot 2: Standard errors as n increases (log-log scale)
plot(sample_sizes, standard_errors, type = "o", log = "xy", xlab = "Sample Size (log scale)",
     ylab = "Standard Error (log scale)", main = "Convergence of Standard Errors (log-log scale)")

# Determine the sample size where the margin of error is less than 0.00025
target_margin_of_error <- 0.00025
sample_size_for_target_error <- sample_sizes[which(margin_of_errors < target_margin_of_error)[1]]

# Output the result
cat("Sample size for margin of error < 0.00025:", sample_size_for_target_error, "\n")



# 参数设置
S0 <- 248.035*exp(-0.01)  # 初始资产价格
K <- 250       # 行权价
T <- 1         # 期权到期时间
r <- 0.01      # 无风险利率
sigma <- 0.333 # 波动率

# Black-Scholes 公式中的d1和d2
d1 <- (log(S0 / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
d2 <- d1 - sigma * sqrt(T)

# 计算期权价格
call_price <- S0 * pnorm(d1) - K * exp(-r * T) * pnorm(d2)

# 输出结果
cat("看涨期权价格:", call_price, "\n")




# 设置参数
S1_0 <- 100  # 资产1的初始价格
S2_0 <- 150  # 资产2的初始价格
sigma1 <- 0.50  # 资产1的波动率
sigma2 <- 0.40  # 资产2的波动率
r <- 0.01  # 无风险利率
T <- 1  # 到期期限
K <- 250  # 行权价
rho <- 0.1  # 资产1和资产2的相关系数
q<-0.03
z<-1.96

# Monte Carlo 模拟参数
n <- 10000 # 样本数量

# 生成相关的标准正态变量
Z1 <- rnorm(n)
Z2 <- rho * Z1 + sqrt(1 - rho^2) * rnorm(n)

# 使用几何布朗运动模拟终止时的资产价格
S1_T <- S1_0 * exp((r - 0.5 * sigma1^2) * T + sigma1 * sqrt(T) * Z1)
S2_T <- S2_0 * exp((r - q - 0.5 * sigma2^2) * T + sigma2 * sqrt(T) * Z2)

# 计算折现收益
payoffs <- exp(-r * T) * pmax(S1_T + S2_T - K, 0)

# 期权价格 (折现收益的均值)
option_value <- mean(payoffs)

# 计算样本标准差
std_dev <- sd(payoffs)

# 计算95%置信区间
confidence_interval <- c(
  option_value - z * std_dev / sqrt(n),
  option_value + z * std_dev / sqrt(n)
)

# 输出结果
cat("估算期权价格:", option_value, "\n")
cat("95%置信区间: [", confidence_interval[1], ",", confidence_interval[2], "]\n")

# 设置参数
S1_0 <- 100  # 资产1的初始价格
S2_0 <- 150  # 资产2的初始价格
sigma1 <- 0.50  # 资产1的波动率
sigma2 <- 0.40  # 资产2的波动率
r <- 0.01  # 无风险利率
T <- 1  # 到期期限
K <- 250  # 行权价
rho <- 0.1  # 资产1和资产2的相关系数
z <- 1.96  # 对应95%置信区间的z值

# Monte Carlo 模拟参数
n <- 10240000  # 样本数量

# 生成相关的标准正态随机变量
Z1 <- rnorm(n)
Z2 <- rho * Z1 + sqrt(1 - rho^2) * rnorm(n)

# 使用几何布朗运动模拟终止时的资产价格
S1_T <- S1_0 * exp((r - 0.5 * sigma1^2) * T + sigma1 * sqrt(T) * Z1)
S2_T <- S2_0 * exp((r - 0.5 * sigma2^2) * T + sigma2 * sqrt(T) * Z2)

# 计算篮子看跌期权的折现收益
payoffs_put <- exp(-r * T) * pmax(K - (S1_T + S2_T), 0)

# 估算看跌期权价格 (折现收益的均值)
option_value_put <- mean(payoffs_put)

# 计算样本标准差
std_dev_put <- sd(payoffs_put)

# 计算95%置信区间
confidence_interval_put <- c(
  option_value_put - z * std_dev_put / sqrt(n),
  option_value_put + z * std_dev_put / sqrt(n)
)

# 输出结果
cat("估算篮子看跌期权价格:", option_value_put, "\n")
cat("95%置信区间: [", confidence_interval_put[1], ",", confidence_interval_put[2], "]\n")

0.25*(2896.631+3750.917+2*0.1*sqrt(2897.631*3750.917))




# 读取数据
data <- read.csv("C:/Users/12194/Desktop/exp.csv", header = TRUE)

# 样本数量
n <- length(data['sample'])

# 计算最大似然估计 λ
lambda_hat <- n / sum(data['sample'])

# 计算最大似然估计 θ = 1/λ
theta_hat <- 1 / lambda_hat

# 输出结果
cat("最大似然估计 λ̂:", lambda_hat, "\n")
cat("最大似然估计 θ̂ (均值):", theta_hat, "\n")



# HW6

# 加载必要的库
library(ggplot2)

# 读取数据
tsla_data <- read.csv("C:/Users/12194/Desktop/TSLA.csv")

# 计算每日对数收益率
tsla_data$Log_Returns <- c(NA, diff(log(tsla_data$Adj.Close)))

# 移除NA值
log_returns <- na.omit(tsla_data$Log_Returns)

# 生成经验累积分布函数 (ECDF)
ecdf_data <- ecdf(log_returns)

# 绘制ECDF
ggplot(data.frame(Log_Returns = log_returns), aes(x = Log_Returns)) +
  stat_ecdf(geom = "step") +
  ggtitle("Empirical CDF of TSLA Daily Log Returns") +
  xlab("Log Returns") +
  ylab("ECDF") +
  theme_minimal()
# 绘制TSLA对数收益率的经验累积分布
plot(ecdf_data, verticals = TRUE, do.points = FALSE, main = "Empirical CDF of TSLA Log Returns",
     xlab = "Log Returns", ylab = "ECDF")

# 添加标准正态分布的理论CDF曲线
curve(pnorm(x, mean = mean(log_returns), sd = sd(log_returns)), add = TRUE, col = "red")



# 6.1.2
# 加载必要的库
library(e1071)

# 计算样本峰度
sample_kurtosis <- kurtosis(log_returns)
sample_kurtosis_rounded <- round(sample_kurtosis, 1)
sample_kurtosis_rounded


# 设置种子
set.seed(100)

# 进行5000次重抽样，计算每次的样本峰度
B <- 5000
kurtosis_samples <- replicate(B, {
  resample <- sample(log_returns, length(log_returns), replace = TRUE)
  kurtosis(resample)
})+3

# 绘制样本峰度的直方图
hist(kurtosis_samples, breaks = 30, main = "Histogram of Sample Kurtoses (B=5000)",
     xlab = "Kurtosis", col = "lightblue", border = "black")


# 计算样本峰度的标准误差
standard_error <- sd(kurtosis_samples)/sqrt(n)
standard_error_rounded <- round(standard_error, 2)
standard_error_rounded


# 估计样本峰度大于或等于6的概率
probability_kurtosis_ge_6 <- mean(kurtosis_samples >= 6)
probability_rounded <- round(probability_kurtosis_ge_6, 4)
probability_rounded

# 计算0.025和0.975分位数
quantile_025 <- quantile(kurtosis_samples, 0.025)
quantile_975 <- quantile(kurtosis_samples, 0.975)

# 四舍五入到小数点后七位
quantile_025_rounded <- round(quantile_025, 7)
quantile_975_rounded <- round(quantile_975, 7)

quantile_025_rounded
quantile_975_rounded


# 构建95%置信区间
lower_bound <- round(quantile_025, 2)
upper_bound <- round(quantile_975, 2)

lower_bound
upper_bound


# 加载必要的库
library(readr)
library(tseries)    # 用于Jarque-Bera检验
library(ggplot2)
library(nortest)    # 用于Shapiro-Wilk检验
library(dplyr)
# 读取数据
data <- read_csv("C:/Users/12194/Desktop/TSLANVDA.csv")

# 计算TSLA和NVDA的对数收益
data$TSLA_Log_Return <- log(data$TSLAAdjClose / lag(data$TSLAAdjClose))
data$NVDA_Log_Return <- log(data$NVDAAdjClose / lag(data$NVDAAdjClose))

# 移除第一个缺失值行
data <- na.omit(data)

# 计算差异 D_i = TSLA_Log_Return - NVDA_Log_Return
data$D_i <- data$TSLA_Log_Return - data$NVDA_Log_Return

# 确保 D_i > median(D_i) 转换为逻辑值（TRUE/FALSE）
binary_D_i <- as.factor(data$D_i > median(data$D_i))

# 进行游程检验
runs_test_result <- runs.test(binary_D_i, alternative = "two.sided")

# 输出检验结果
print(runs_test_result)



# Shapiro-Wilk 检验
shapiro_test <- shapiro.test(data$TSLA_Log_Return)
print(shapiro_test)

# Jarque-Bera 检验
jarque_test <- jarque.bera.test(data$TSLA_Log_Return)
print(jarque_test)

# 绘制正态 Q-Q 图
qqnorm(data$TSLA_Log_Return)
qqline(data$TSLA_Log_Return, col = "red")


# 计算检验统计量
mean_diff <- mean(data$D_i)
std_error <- sd(data$D_i) / sqrt(length(data$D_i))

# 计算 Z 统计量
z_stat <- mean_diff / std_error
print(z_stat)

# 设置显著性水平 α = 0.05
alpha <- 0.05
z_critical <- qnorm(alpha)

# 输出拒绝原假设的条件
print(paste("拒绝 H0 的条件为：|Z| >", z_critical))

# 设置样本均值、标准差和样本大小
mean_D <- -0.0008377695845049584  # 样本均值
std_D <- 0.034402152435920146  # 样本标准差
n <- 1256  # 样本大小

# 计算不同显著性水平下的z值
z_alpha_01 <- -2.33  # 对应 alpha = 0.01 (99%置信度)
z_alpha_05 <- -1.645 # 对应 alpha = 0.05 (95%置信度)
z_alpha_10 <- -1.28  # 对应 alpha = 0.10 (90%置信度)

# 计算置信区间的下限
ci_01_lower <- mean_D - z_alpha_01 * (std_error)
ci_05_lower <- mean_D - z_alpha_05 * (std_error)
ci_10_lower <- mean_D - z_alpha_10 * (std_error)

# 打印结果
cat("99% 置信区间: (", ci_01_lower, ", ∞ )\n")
cat("95% 置信区间: (", ci_05_lower, ", ∞ )\n")
cat("90% 置信区间: (", ci_10_lower, ", ∞ )\n")


################################################################
# HW7

library(MASS)     # For fitting distributions
library(fBasics)  # For skewness and kurtosis
library(Bessel) 

zm_data <- read.csv("C:/Users/12194/Desktop/ZM.csv")
zm_data$Date <- as.Date(zm_data$Date, format="%m/%d/%Y")
log_returns <- diff(log(zm_data$ZM))

# Q1.1: BSM
t <- 1 / 252
initial_value_BSM <- c(1, 1)  

BSM <- function(x, theta) {
  exp(-(x - theta[1])^2 / (2 * theta[2]^2 * t)) / sqrt(2 * pi * theta[2]^2 * t)
}

result_BSM <- optim(
  initial_value_BSM,
  fn = function(theta) {
    -sum(log(BSM(log_returns , theta)))  
  },
  method = "BFGS"  
)

print(result_BSM)


#Q2
t <- 1/252

NIG_density <- function(x, theta) {
  alpha <- theta[1]
  beta <- theta[2]
  delta <- theta[3]
  mu <- theta[4]
  
  part1 <- alpha * delta * t / pi
  part2 <- besselK(alpha * sqrt(delta^2 * t^2 + (x - mu * t)^2), 1)
  part3 <- sqrt(delta^2 * t^2 + (x - mu * t)^2)
  part4 <- exp(delta * sqrt(alpha^2 - beta^2) * t + beta * (x - mu * t))
  
  return(part1 * part2 / part3 * part4)
}

rDJ <- log_returns[!is.na(log_returns)] 

initial_value_NIG <- c(10, 0, 2, 0)

result_NIG <- optim(
  initial_value_NIG,
  fn = function(theta) {
    -sum(log(NIG_density(rDJ, theta)))
  },
  method = "L-BFGS-B",  
  lower = c(0.001, -Inf, 0.001, -Inf),
  upper = c(Inf, Inf, Inf, Inf)
)

log_likelihood_NIG <- -result_NIG$value
alpha_hat <- result_NIG$par[1]
beta_hat <- result_NIG$par[2]
delta_hat <- result_NIG$par[3]
mu_hat <- result_NIG$par[4]

cat("NIG Model MLEs:\n")
cat("alpha:", round(alpha_hat, 4), "\n")
cat("beta:", round(beta_hat, 4), "\n")
cat("delta:", round(delta_hat, 4), "\n")
cat("mu:", round(mu_hat, 4), "\n")
cat("Log likelihood (NIG):", round(log_likelihood_NIG, 2), "\n")

#Q3
mean_log_returns <- mean(log_returns, na.rm=TRUE)
var_log_returns <- var(log_returns, na.rm=TRUE)
skewness_log_returns <- skewness(log_returns, na.rm=TRUE)
kurtosis_log_returns <- kurtosis(log_returns, na.rm=TRUE)

cat("Q1.3 Results:\n")
cat("Sample mean:", round(mean_log_returns, 4), "\n")
cat("Sample variance:", round(var_log_returns, 4), "\n")
cat("Sample skewness:", round(skewness_log_returns, 3), "\n")
cat("Sample kurtosis:", round(kurtosis_log_returns, 3), "\n")


#Q4
# 计算样本矩：均值、方差、偏度、峰度
m <- mean(rDJ)
v <- var(rDJ)
s <- skewness(rDJ)
k <- kurtosis(rDJ)

# 基于公式计算 NIG 的参数
alpha <- 3 * sqrt(3 * k - 9 - 4 * s^2) / (sqrt(v) * (3 * k - 9 - 5 * s^2))
beta <- 3 * s / (sqrt(v) * (3 * k - 9 - 5 * s^2))
gamma <- sqrt(alpha^2 - beta^2)
delta <- 9 / ((3 * k - 9 - 4 * s^2) * gamma * t)
mu <- (m * gamma - beta * delta * t) / (gamma * t)

# 将结果存入一个向量
MMestimates <- c(alpha, beta, delta, mu)

# 输出矩匹配估计的参数
cat("Moment Matching Estimates:\n")
cat("alpha:", round(MMestimates[1], 4), "\n")
cat("beta:", round(MMestimates[2], 4), "\n")
cat("delta:", round(MMestimates[3], 4), "\n")
cat("mu:", round(MMestimates[4], 4), "\n")


#Q7
# 定义 Laplace 分布的负对数似然函数
Laplace_density <- function(x, params) {
  mu <- params[1]
  b <- params[2]
  if (b <= 0) return(Inf)
  return(-sum(log((1 / (2 * b)) * exp(-abs(x - mu) / b))))
}

# 使用矩匹配法找到初始值
mu_initial <- mean(rDJ)
b_initial <- sd(rDJ) / sqrt(2)

# 使用 optim 进行最大似然估计
result_Laplace <- optim(
  par = c(mu_initial, b_initial),
  fn = Laplace_density,
  x = rDJ,
  method = "L-BFGS-B",
  lower = c(-Inf, 0.001),
  upper = c(Inf, Inf)
)

# 输出 Laplace 模型的结果
mu_hat <- result_Laplace$par[1]
b_hat <- result_Laplace$par[2]
log_likelihood_Laplace <- -result_Laplace$value

cat("MLE for mu (Laplace):", round(mu_hat, 5), "\n")
cat("MLE for b (Laplace):", round(b_hat, 4), "\n")
cat("Log likelihood (Laplace):", round(log_likelihood_Laplace, 2), "\n")

#Q9
# 计算 AIC
AIC_BSM <- 2 * 2 - 2 * log_likelihood_bsm
cat("AIC for BSM:", round(AIC_BSM, 2), "\n")

AIC_NIG <- 2 * 4 - 2 * log_likelihood_NIG
cat("AIC for NIG:", round(AIC_NIG, 2), "\n")

AIC_Laplace <- 2 * 2 - 2 * log_likelihood_Laplace
cat("AIC for Laplace:", round(AIC_Laplace, 2), "\n")


#Q10
# 绘制直方图
hist(rDJ, breaks=30, probability=TRUE, main="Histogram of ZM Daily Log Returns with Fitted Models", xlab="Log Returns")

# 生成 BSM、NIG 和 Laplace 模型拟合的密度曲线
x_vals <- seq(min(rDJ), max(rDJ), length.out=1000)

# BSM 模型的拟合曲线
bsm_density <- dnorm(x_vals, mean=mu_bsm, sd=sigma_bsm)
lines(x_vals, bsm_density, col="red", lwd=2, lty=1)

# NIG 模型的拟合曲线
nig_density <- sapply(x_vals, function(x) NIG_density(x, c(alpha_hat, beta_hat, delta_hat, mu_hat)))
lines(x_vals, nig_density, col="blue", lwd=2, lty=2)
      
# Laplace 模型的拟合曲线
laplace_density <- (1 / (2 * b_hat)) * exp(-abs(x_vals - mu_hat) / b_hat)
lines(x_vals, laplace_density, col="green", lwd=2, lty=3)

# 添加图例
legend("topright", legend=c("BSM", "NIG", "Laplace"), col=c("red", "blue", "green"), lty=1:3, lwd=2)
