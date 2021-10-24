set.seed(1)
x = rnorm(100)
eps = rnorm(100)

beta = c(1, -1, 2, 0.5)
y = beta[1] + beta[2]*x + beta[3]*x^2 + beta[4]*x^3 + eps

####### Best Subset #######
library(leaps)
ans.df = data.frame(y = y, x = x)
ans.model = regsubsets(y ~ poly(x, 10, raw = T), 
                       data = ans.df, nvmax = 10, method = "backward")
ans.summary = summary(ans.model)

min_p = which.min(ans.summary$cp)
plot(ans.summary$cp, xlab = "Subset size", ylab = "Cp", 
     col = "blue", pch = 20, type = "o")
points(min_p, ans.summary$cp[min_p], col = "red", lwd = 3)
title("Best Subset with Cp using 'backward'")
coefficients(ans.model, id = which.min(ans.summary$cp))

min_p = which.min(ans.summary$bic)
plot(ans.summary$bic, xlab = "Subset size", ylab = "BIC", 
     col = "blue", pch = 20, type = "o")
points(min_p, ans.summary$bic[min_p], col = "red", lwd = 3)
title("Best Subset with BIC using 'backward'")
coefficients(ans.model, id = which.min(ans.summary$bic))

max_p = which.max(ans.summary$adjr2)
plot(ans.summary$adjr2, xlab = "Subset size", ylab = "AdjR2", 
     col = "blue", pch = 20, type = "o")
points(max_p, ans.summary$adjr2[max_p], col = "red", lwd = 3)
title("Best Subset with AdjR2 using 'backward'")
coefficients(ans.model, id = which.max(ans.summary$adjr2))

####### LASSO #######
library(glmnet)
xmat = model.matrix(y ~ poly(x, 10, raw = T), data = ans.df)[, -1]
ans.lasso = cv.glmnet(xmat, y, alpha = 1)
ans.lambda = ans.lasso$lambda.min
ans.lambda
plot(ans.lasso)

ans.lasso = glmnet(xmat, y, alpha = 1)
predict(ans.lasso, s = ans.lambda, type = "coefficients")

####### Comparison #######
beta_7 = 3
yy = beta[1] * beta_7*x^7 + eps
ans.df = data.frame(y = yy, x = x)
ans.model = regsubsets(y ~ poly(x, 10, raw = T), 
                       data = ans.df, nvmax = 10)
ans.summary = summary(ans.model)

min_p = which.min(ans.summary$cp)
plot(ans.summary$cp, xlab = "Subset size", ylab = "Cp", 
     col = "blue", pch = 20, type = "o")
points(min_p, ans.summary$cp[min_p], col = "red", lwd = 3)
title("Best Subset with Cp")
coefficients(ans.model, id = which.min(ans.summary$cp))

min_p = which.min(ans.summary$bic)
plot(ans.summary$bic, xlab = "Subset size", ylab = "BIC", 
     col = "blue", pch = 20, type = "o")
points(min_p, ans.summary$bic[min_p], col = "red", lwd = 3)
title("Best Subset with BIC")
coefficients(ans.model, id = which.min(ans.summary$bic))

max_p = which.max(ans.summary$adjr2)
plot(ans.summary$adjr2, xlab = "Subset size", ylab = "AdjR2", 
     col = "blue", pch = 20, type = "o")
points(max_p, ans.summary$adjr2[max_p], col = "red", lwd = 3)
title("Best Subset with AdjR2")
coefficients(ans.model, id = which.max(ans.summary$adjr2))

xmat = model.matrix(y ~ poly(x, 10, raw = T), data = ans.df)[, -1]
ans.lasso = cv.glmnet(xmat, y, alpha = 1)
ans.lambda = ans.lasso$lambda.min
ans.lambda
plot(ans.lasso)

ans.lasso = glmnet(xmat, y, alpha = 1)
predict(ans.lasso, s = ans.lambda, type = "coefficients")
