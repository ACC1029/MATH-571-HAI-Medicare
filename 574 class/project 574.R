#diabetes <- read.csv("/Users/acc/Documents/IIT/Spring 2018/Bayesian Comp Stats/diabetes_data")

sigma2


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
  
  invg_size <- sqrt(lambda^2*sigma2/beta[j]^2)
  mu <- lambda^2
  inv_tau2 <- rinvgauss(invg_size, mu)
}

lambda <- lambda_sample[1]
sigma2 <- sigma2_sample[1]
beta <- beta_sample[1,]

invg_size <- sqrt(lambda^2*sigma2/beta[1]^2)
mu <- lambda^2

inv_tau2 <- rinvgauss(1, invg_size, mu)


Dtau <- 

betas <- dnorm((1/A)(t(X)))

A <- t(X)%*%X + 1/Dtau