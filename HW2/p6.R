set.seed(1)
n = 1000
p = 20

xmat = matrix(rnorm(n*p), n, p)
eps = rnorm(n)

beta = rnorm(p)
beta[3] = 0
beta[6] = 0
beta[9] = 0
beta[12] = 0
beta[15] = 0
beta[18] = 0

y = xmat %*% beta + eps

train = sample(1: n, 100)
test = -train
xmat.train = xmat[train, ]
xmat.test = xmat[test, ]
y.train = y[train, ]
y.test = y[test, ]

ans.df = data.frame(x = xmat.train, y = y.train)
ans.model = regsubsets(y ~ ., data = ans.df, nvmax = p)
ans.summary = summary(ans.model)
ans.summary

####### Training MSE #######
ans.train_mse = rep(NA, p)
ans.xcols = colnames(xmat, do.NULL = F, prefix = "x.")
for(i in 1:p){
  c_i = coef(ans.model, id = i)
  if(i > 1){
    y.train.pred = as.matrix(xmat.train[, ans.xcols %in% names(c_i)] %*%
                               c_i[names(c_i) %in% ans.xcols])
  }
  else
  {
    y.train.pred = as.matrix(xmat.train[, ans.xcols %in% names(c_i)] *
                               c_i[names(c_i) %in% ans.xcols])
  }
  ans.train_mse[i] = mean((y.train-y.train.pred)^2)
}
plot(ans.train_mse, ylab = "Training MSE",xlab = "Subset Size", 
     pch = 20, type = "o", col = "blue")
title("Training MSE-Subset size Curve")

####### Testing MSE #######
ans.test_mse = rep(NA, p)
ans.xcols = colnames(xmat, do.NULL = F, prefix = "x.")
for(i in 1:p){
  c_i = coef(ans.model, id = i)
  if(i > 1){
    y.test.pred = as.matrix(xmat.test[, ans.xcols %in% names(c_i)] %*%
                               c_i[names(c_i) %in% ans.xcols])
  }
  else
  {
    y.test.pred = as.matrix(xmat.test[, ans.xcols %in% names(c_i)] *
                               c_i[names(c_i) %in% ans.xcols])
  }
  ans.test_mse[i] = mean((y.test-y.test.pred)^2)
}
plot(ans.test_mse, ylab = "Testing MSE",xlab = "Subset Size", 
     pch = 20, type = "o", col = "blue")
title("Testing MSE-Subset size Curve")

which.min(ans.test_mse)

coef(ans.model, id = which.min(ans.test_mse))
beta

ans.beta_error = rep(NA, p)
ans.xcols = colnames(xmat, do.NULL = F, prefix = "x.")
for(i in 1:p){
  c_i = coef(ans.model, id = i)
  ans.beta_error[i] = sqrt(sum((beta[ans.xcols %in% names(c_i)] - 
                                  c_i[names(c_i) %in% ans.xcols])^2) + 
                             sum(beta[!(ans.xcols %in% names(c_i))]^2))
  }
plot(x = 1:p, ans.beta_error, xlab = "Coefficient #", 
     ylab = "Beta error")
title("Beta error on each dimesion")
which.min(ans.beta_error)
