library(matlib)

x = c(0.0, 0.2, 0.4, 0.6, 0.8 , 1.0)
X = matrix(cbind(1, x), ncol = 2)
Y = matrix(c(0, 0, 0, 1, 0, 1), ncol = 1)

beta = matrix(c(0, 0), ncol = 1)

cat("Initial Beta values: ", "beta0 = ", beta[1], ",", 
    " beta1 = ", beta[2], "\n")

maxIter = 10

for(i in 1:maxIter){
  p = exp(X %*% beta) / (1 + exp(X %*% beta))
  w = as.vector(p * (1 - p))
  W = diag(w)
  z = X %*% beta + inv(W) %*% (Y - p)
  beta = inv(t(X) %*% W %*% X) %*% t(X) %*% W %*% z
  cat("Beta values at iter #: ", i, "beta0 = ", beta[1], ",", 
      " beta1 = ", beta[2], "\n")
}

