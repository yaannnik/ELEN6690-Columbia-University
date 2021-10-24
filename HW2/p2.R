####### Ridge #######
y = 2
lambda = 1
beta = seq(-5,5,0.01)

plot(beta,(y-beta)^2 + lambda*beta^2,type="l",xlab = "beta",ylab = "Optimization function",xlim=c(-5,5))

min_x = y/(1+lambda)
min_y = (y-min_x)^2 + lambda*min_x^2
points(min_x,min_y,col="red")

title("Optimization Result for Ridge Regression")

####### LASSO #######
y = 1
lambda = 3
beta = seq(-5,5,0.01)

plot(beta,(y-beta)^2 + lambda*abs(beta),type="l",xlab = "beta",ylab = "Optimization function",xlim=c(-5,5))

if(y > lambda/2) {
  min_x1 = y - lambda/2
} else {
  if(y < -lambda/2){
    min_x1 = y + lambda/2
  } else {
    min_x1 = 0
  }
}

min_y1 = (y-min_x1)^2 + lambda*abs(min_x1)
points(min_x1,min_y1,col="red")

title("Optimization Result for LASSO Regression")

