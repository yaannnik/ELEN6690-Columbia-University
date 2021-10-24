library(ISLR)
set.seed(1)
train.size = nrow(College) / 2
train = sample(1: nrow(College), train.size)
test = -train
College.train = College[train, ]
College.test = College[test, ]

College.lm = lm(Apps~., data = College.train)
summary(College.lm)

College.pred = predict(College.lm, College.test)
College.error = mean((College.test$Apps - College.pred)^2)

train_mat = model.matrix(Apps ~ ., data = College.train)
test_mat = model.matrix(Apps ~ ., data = College.test)
College.ridge = cv.glmnet(train_mat, College.train$Apps, alpha = 0)
plot(College.ridge)
College.lambda = College.ridge$lambda.min
College.pred = predict(College.ridge, newx=test_mat, s=College.lambda)
College.error = mean((College.test$Apps - College.pred)^2)

College.lasso = cv.glmnet(train_mat, College.train$Apps, alpha = 1)
plot(College.lasso)
College.lambda = College.lasso$lambda.min
College.pred = predict(College.lasso, newx=test_mat, s=College.lambda)
College.error = mean((College.test$Apps - College.pred)^2)
