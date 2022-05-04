library(tree)
library(ISLR2)

set.seed(1)

# training data
train = sample(1:nrow(Boston), nrow(Boston)/2)

# fit tree
tree.boston = tree(medv~., Boston, subset=train)
summary(tree.boston)

# plot tree
plot(tree.boston)
text(tree.boston, pretty=0)

# could fit larger tree via
# control = tree.control(nobs=length(train), mindev=0)

# try CV to improve perf
cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type="b")

# can prune back simplify
prune.boston = prune.tree(tree.boston, best=5)
plot(prune.boston)
text(prune.boston, pretty=0)

# cv preferred fullest model, so use that to predict
yhat = predict(tree.boston, newdata = Boston[-train,])
boston.test = Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0,1)
mean((yhat-boston.test)^2)
