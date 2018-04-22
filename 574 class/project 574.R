library(tidyverse)
library(statmod)
library(LaplacesDemon)




diabetes <- read.csv("/Users/acc/Documents/IIT/Spring 2018/Bayesian Comp Stats/diabetes_data")



#=========================
# Tests

p <- 3
N <- 100
X <- matrix(rnorm(N*p),ncol=p)
y <- rnorm(N)
yX = cbind(y,X)
yX <- data.frame(yX)


lm <- lm(y ~ ., yX)

res_sigma <- mean((lm$residuals^2))
beta_ls <- lm$coefficients[-1]
lambda_0 <- p*sqrt(res_sigma)/sum(abs(beta_ls))

nmax <- 100

p <- ncol(X)
n <- nrow(X)

beta_sample <- matrix(rep(0, p*(1 + nmax)), ncol = p)
tau_sample <- matrix(rep(0, p*nmax), ncol = p)
sigma2_sample <- c()
lambda_sample <- c()

lambda_sample <- c(lambda_sample, lambda_0)
sigma2_sample <- c(sigma2_sample, res_sigma)
beta_sample[1,] <- beta_ls

for(k in 1:nmax) {
  lambda <- lambda_sample[k]
  sigma2 <- sigma2_sample[k]
  beta <- beta_sample[k,]
  
  for(j in 1:p) {
    invgauss_size <- sqrt(lambda^2*sigma2/beta[j]^2)
    invgauss_mu <- lambda^2
    inv_tau2 <- rinvgauss(1, mean = invgauss_size, invgauss_mu)
    tau_sample[k,j] <- (1/(1/inv_tau2))
  }
  
  invgam_n <- (n-1)/2 + p/2
  invgam_shape <- 
    t(res_sigma - X %*% beta_ls) %*% (res_sigma - X %*% beta_ls) / 2 + 
    t(beta_ls) %*% solve(diag(tau_sample[k,])) %*% beta_ls/2
  sigma2 <- rinvgamma(1, scale = invgam_n, shape = invgam_shape)
  sigma2_sample <- c(sigma2_sample, sigma2)
  
  
  A <- t(X)%*%X + diag(tau_sample[k,])
  
  int_val <- t(X) * res_sigma
  norm_mean <- solve(A) %*% int_val
  
  norm_var <- sigma2 * solve(A)
  
  beta <- rnorm(1, mean = norm_mean, sd = norm_var)
  beta_sample[k+1,] <- beta
  
  tau_mean <- colMeans(tau_sample)
  lambda <- sqrt(2*p/sum(tau_mean))
  lambda_sample <- c(lambda_sample, lambda)
}


