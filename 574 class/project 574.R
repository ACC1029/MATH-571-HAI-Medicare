library(tidyverse)
library(statmod)
library(LaplacesDemon)

#=========================
# Tests

p <- 3
N <- 100
X <- matrix(rnorm(N*p),ncol=p)
y <- rnorm(N)
yX = cbind(y,X)
yX <- data.frame(yX)

lm <- lm(y ~ ., yX)

# Initial Values
beta_ls <- lm$coefficients[-1]
res_sigma <- (lm$residuals^2)
sigma2 <- drop((t(res_sigma) %*% res_sigma) / n)
lambda_0 <- p*sqrt(sigma2)/sum(abs(beta_ls))

nmax <- 100

p <- ncol(X)
n <- nrow(X)

## Create empty samples
beta_sample <- matrix(rep(0, p*(1 + nmax)), ncol = p)
tau_sample <- matrix(rep(0, p*nmax), ncol = p)
sigma2_sample <- c()
lambda_sample <- c()

## Add initial values to sample matricies
lambda_sample <- c(lambda_sample, lambda_0)
sigma2_sample <- c(sigma2_sample, sigma2)
beta_sample[1,] <- beta_ls

## Calculate Initial Tau2
for(j in 1:p) {
  invgauss_size <- sqrt(lambda_0^2*sigma2/beta_ls[j]^2)
  invgauss_mu <- lambda_0^2
  inv_tau2 <- rinvgauss(1, mean = invgauss_size, invgauss_mu)
  tau_sample[1,j] <- (1/inv_tau2)
}

## Apply Gibbs Sampling
for(k in 2:nmax) {
  # Get values at k
  old_sigma2 <- sigma2_sample[k - 1]
  
  # Calculate Lambda for K
  # tau_sample colMeans potential issue
  tau_mean <- colMeans(tau_sample)
  lambda <- sqrt(2*p/sum(tau_mean))
  lambda_sample[k] = lambda
  
  
  # Calculate Beta for K
  diag_tau <- diag(tau_sample[k-1,])
  invDiag <- solve(diag_tau)
  A <- t(X)%*%X + invDiag
  invA <- solve(A)
  ## Potential issue
  int_val = t(X) * y
  norm_mean <- invA %*% int_val
  norm_var <- old_sigma2 * solve(A)
  beta <- rmnorm(1, norm_mean[,1:k], norm_var)
  beta_sample[k,] <- beta
  
  
  # Calculate sigma2
  invgam_n <- (n-1)/2 + p/2
  invgam_shape <- 
    t(res_sigma - X %*% beta) %*% (res_sigma - X %*% beta) / 2 + 
    t(beta) %*% solve(diag(tau_sample[k,])) %*% beta/2
  sigma2 <- rinvgamma(1, scale = invgam_n, shape = invgam_shape)
  sigma2_sample <- c(sigma2_sample, sigma2)
  
  # Calculate Tau's for K
  for(j in 1:p) {
    invgauss_size <- sqrt(lambda^2*sigma2/beta[j]^2)
    invgauss_mu <- lambda^2
    inv_tau2 <- rinvgauss(1, mean = invgauss_size, invgauss_mu)
    tau_sample[k,j] <- (1/inv_tau2)
  }
}


#=========================
# Diabetes

diabetes <- read.csv("diabetes_data")
diabetes_params <- diabetes[1:10]

p <- 10
N <- 442
y <- diabetes$Y
X <- data.matrix(diabetes_params)
yX = cbind(y,X)
yX <- data.frame(yX)


lm <- lm(y ~ ., yX)

res_sigma <- mean((lm$residuals^2))
beta_ls <- lm$coefficients[-1]
lambda_0 <- p*sqrt(res_sigma)/sum(abs(beta_ls))

nmax <- 10000

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
    tau_sample[k,j] <- (1/inv_tau2)
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





