library(ISLR)
library(tree)
attach(OJ)
set.seed(1000)

train = sample(dim(OJ)[1], 800)
OJ.train = OJ[train, ]
OJ.test = OJ[-train, ]

OJ.tree = tree(Purchase ~ ., data = OJ.train)
summary(OJ.tree)

OJ.tree

plot(OJ.tree)
text(OJ.tree)
title("Tree")

OJ.pred = predict(OJ.tree, OJ.test, type = "class")
table(OJ.test$Purchase, OJ.pred)
OJ.error = sum(OJ.test$Purchase != OJ.pred)/dim(OJ.test)[1]
OJ.error

OJ.cvtree = cv.tree(OJ.tree, FUN = prune.tree)
OJ.cvtree

plot(OJ.cvtree$size, OJ.cvtree$dev, pch = 20, type = "o", col = "blue", 
     xlab = "Tree Size",ylab = "Dev")
title("Dev-Size Curve")

OJ.cvtree$size[which.min(OJ.cvtree$dev)]

OJ.pruned = prune.tree(OJ.tree, best = OJ.cvtree$size[which.min(OJ.cvtree$dev)])
OJ.pruned


summary(OJ.pruned)

OJ.prunedpred = predict(OJ.pruned, OJ.test, type = "class")
table(OJ.test$Purchase, OJ.prunedpred)
OJ.prunederror = sum(OJ.test$Purchase != OJ.prunedpred)/dim(OJ.test)[1]
OJ.prunederror
