library(tidyverse)
library(statmod)
library(LaplacesDemon)

#=========================
# Tests

r <- 1
delta <- 1.78
p <- 3
N <- 100
X <- matrix(rnorm(N*p),ncol=p)
y <- rnorm(N)
yX = cbind(y,X)
yX <- data.frame(yX)


lm <- lm(y ~ ., yX)

XtX <- t(X) %*% X
xy <- t(X) %*% y

# Initial Values
beta_ls <- lm$coefficients[-1]
res_sigma <- (lm$residuals^2)
sigma2 <- drop((t(res_sigma) %*% res_sigma) / n)
gamma_shape <- r + p
gamma_rate <- delta
lambda_0 <- rgamma(1, shape = gamma_shape, rate = gamma_rate)

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

for(k in 2:nmax) {
  # Get values at k
  old_sigma2 <- sigma2_sample[k - 1]
  
  # Calculate Lambda for K
  # tau_sample colMeans potential issue
  tau_mean <- colMeans(tau_sample)
  gamma_shape <- r + p
  gamma_rate <- (sum(tau_mean) / 2) + delta
  lambda <- rgamma(1, shape = gamma_shape, rate = gamma_rate)
  lambda_sample <- c(lambda_sample, lambda)
  
  
  # Calculate Beta for K
  diag_tau <- diag(tau_sample[k-1,])
  invDiag <- solve(diag_tau)
  A <- XtX + invDiag
  invA <- solve(A)
  ## Potential issue
  norm_mean <- invA %*% xy
  norm_var <- old_sigma2 * solve(A)
  beta <- drop(rmnorm(1, norm_mean, norm_var))
  beta_sample[k,] <- beta
  
  
  # Calculate sigma2
  invgam_n <- (n-1)/2 + p/2
  invgam_shape <- 
    t(res_sigma - X %*% beta) %*% (res_sigma - X %*% beta) / 2 + 
    t(beta) %*% solve(diag(tau_sample[k-1,])) %*% beta/2
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


r <- 1
delta <- 1.78
p <- 10
N <- 442
y <- scale(diabetes$Y)
# X <- data.matrix(diabetes_params)
X <- scale(diabetes_params)
yX = cbind(y,X)
yX <- data.frame(yX)


lm <- lm(V1 ~ ., yX)

XtX <- t(X) %*% X
xy <- t(X) %*% y

# Initial Values
beta_ls <- lm$coefficients[-1]
# res_sigma <- X %*% beta_ls
# sigma2 <- drop((t(res_sigma) %*% res_sigma) / n)
sigma2 <- (lm$residuals^2)
gamma_shape <- r + p
gamma_rate <- delta
lambda_0 <- rgamma(1, shape = gamma_shape, rate = gamma_rate)

nmax <- 10000

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
  old_beta <- beta_sample[k-1,]
  
  # Calculate Lambda for K
  # tau_sample colMeans potential issue
  # tau_mean <- colMeans(tau_sample)
  gamma_shape <- r + p
  gamma_rate <- (sum(tau_sample[k - 1,]) / 2) + delta
  lambda <- sqrt(rgamma(1, shape = gamma_shape, scale = 1/gamma_rate))
  lambda_sample <- c(lambda_sample, lambda)
  
  
  
  # Calculate Tau's for K
  for(j in 1:p) {
    invgauss_size <- sqrt(lambda^2*old_sigma2/old_beta[j]^2)
    invgauss_mu <- lambda^2
    inv_tau2 <- rinvgauss(1, mean = invgauss_size, invgauss_mu)
    tau_sample[k,j] <- (1/inv_tau2)
  }
  
  # Calculate sigma2
  invgam_shape <- (n-1)/2 + p/2
  residue <- drop(y - X %*% old_beta)
  invgam_scale <- 
    (t(residue) %*% residue + 
       t(old_beta) %*% solve(diag(tau_sample[k,])) %*% old_beta) / 2
  sigma2 <- 1/rgamma(1, scale = invgam_scale, shape = invgam_shape)
  sigma2_sample <- c(sigma2_sample, sigma2)
  
  # Calculate Beta for K
  diag_tau <- diag(tau_sample[k,])
  invDiag <- solve(diag_tau)
  A <- XtX + invDiag
  invA <- solve(A)
  ## Potential issue
  norm_mean <- invA %*% xy
  norm_var <- sigma2 * solve(A)
  beta <- drop(rmnorm(1, norm_mean, norm_var))
  beta_sample[k,] <- beta
}

