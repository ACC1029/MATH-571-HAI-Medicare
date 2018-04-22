# library(pscl) # rigamma
library(mnormt) # rmnorm
library(VGAM) # rinv.gaussian
library(miscTools) # colMeans
library(monomvn)

gibbsBLasso = function(x, y, max.steps = 100000) {
  n <- nrow(x)
  m <- ncol(x)
  
  XtX <- t(x) %*% x	#Time saving
  xy <- t(x) %*% y
  
  r <- 1
  delta <- 0.1 # 1.78
  
  betaSamples <- matrix(0, max.steps, m)
  sigma2Samples <- rep(0, max.steps)
  invTau2Samples <- matrix(0, max.steps, m)
  lambdaSamples <- rep(0, max.steps)
  
  beta <- drop(backsolve(XtX + diag(nrow=m), xy))
  residue <- drop(y - x %*% beta)
  sigma2 <- drop((t(residue) %*% residue) / n)
  invTau2 <- 1 / (beta * beta)
  lambda <- m * sqrt(sigma2) / sum(abs(beta))
  
  k <- 0
  while (k < max.steps) {
    k <- k + 1
    
    if (k %% 1000 == 0) {
      cat('Iteration:', k, "\r")
    }
    
    # sample beta
    invD <- diag(invTau2)
    invA <- solve(XtX + invD)
    mean <- invA %*% xy
    varcov <- sigma2 * invA
    beta <- drop(rmnorm(1, mean, varcov))
    betaSamples[k,] <- beta
    
    # sample sigma2
    shape <- (n+m-1)/2
    residue <- drop(y - x %*% beta)
    scale <- (t(residue) %*% residue + t(beta) %*% invD %*% beta)/2
    sigma2 <- 1/rgamma(1, shape, 1/scale)
    sigma2Samples[k] <- sigma2
    
    # sample tau2
    muPrime <- sqrt(lambda^2 * sigma2 / beta^2)
    lambdaPrime <- lambda^2
    invTau2 <- rep(0, m)
    for (i in seq(m)) {
      invTau2[i] <- rinv.gaussian(1, muPrime[i], lambdaPrime)
    }
    invTau2Samples[k, ] <- invTau2
    
    # update lambda
    shape = r + m/2
    scale = delta + sum(1/invTau2)/2
    lambda <- rgamma(1, shape, 1/scale)
    # if (k %% 10 == 0) {
    # low <- k - 9
    # high <- k
    # lambda <- sqrt( 2*m / sum(colMeans(invTau2Samples[low:high, ])) )
    # }
    lambdaSamples[k] <- lambda
  }
  
#  colMedians(betaSamples[seq(max.steps/2, max.steps, 5), ])
  lambdaSamples
}


diabetes <- read.csv("diabetes_data")
diabetes_params <- diabetes[1:10]

p <- 10
N <- 442
y <- diabetes$Y
X <- data.matrix(diabetes_params)
yX = cbind(y,X)
yX <- data.frame(yX)

lambdas <- gibbsBLasso(X, y, 10000)

lasso <- blasso(X, y, T=10000)
