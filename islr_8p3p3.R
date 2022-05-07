# islr_8p3p3: bagging and random forests

library(randomForest)
library(ISLR2)

set.seed(1)

train = sample(1:nrow(Boston), nrow(Boston)/2)
boston.test = Boston[-train, "medv"]

# bagging
bag.boston = randomForest(medv ~ ., data = Boston,
                          subset = train, mtry = 12,
                          importance = TRUE)
bag.boston

yhat.bag = predict(bag.boston, newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)

# MSE
mean((yhat.bag - boston.test)^2)

# predictions, on average, w/in 4.84k
sqrt(mean((yhat.bag - boston.test)^2)) 


# random forests
set.seed(1)
rf.boston = randomForest(medv ~., data = Boston,
                         subset = train, mtry=6,
                         importance=TRUE)
yhat.rf = predict(rf.boston, newdata = Boston[-train,])
mean((yhat.rf - boston.test)^2)

sqrt(mean((yhat.rf - boston.test)^2))

# importance of variables
# 1. mean decrease of accuracy in predictions on OOB
#    when given variable permuted
# 2. total decrease in node impurity resulting from splits
#    over variable, averaged over all trees
importance(rf.boston)

varImpPlot(rf.boston)
