set.seed(1)
x = rnorm(100, mean = 0, sd = 1)

eps = rnorm(100, mean = 0, sd = 1)

y = -1 + 0.5*x + eps
length(y)

plot(x, y, xlab = "x", ylab = "y", main = "Y-X Relationship")

lm.fit = lm(y ~ x)
summary(lm.fit)
  
plot(x, y, xlab = "x", ylab = "y", main = "Y-X Relationship")
abline(lm.fit)
par(col='black')
plot(x, y, xlab = "x", ylab = "y", main = "Y-X Relationship")
abline(lm.fit, col='red', lty=3)
abline(a=-1, b=0.5, col='green', lty=1)
legend('topleft', inset=0.05, c('least-squares','population-regression'), lty = c(3, 1), col = c('red', 'green'), bty="o")

confint(lm.fit)

# poly_fit = lm(y ~ poly(x, 2))
# summary(poly_fit)
  
# xlims = range(x)
# x.grid = seq(from=xlims[1], to=xlims[2])
# predictions = predict(poly_fit, newdata = list(x = x.grid), se=TRUE)
# se.bands = cbind(predictions$fit+2*predictions$se.fit, predictions$fit-2*predictions$se.fit)
# plot(x, y, xlab = "X", ylab = "Y", main = "Polynormial Relationship", col = "black")
# lines(x.grid, predictions$fit, lwd = 2, col = "green", lty = 1)
# matlines(x.grid, se.bands, lwd = 1, col="red", lty = 3)
# legend('topleft', inset = 0.05, c("polynomial-regression", "SE-wrapper"), lwd = c(2, 1), lty = c(1, 3), col = c("green", "red"), bty = "o")
  