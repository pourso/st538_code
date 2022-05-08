# islr_8p3p4: boosting

library(randomForest)
library(ISLR2)
library(gbm)

set.seed(1)

train = sample(1:nrow(Boston), nrow(Boston)/2)

boston.test = Boston[-train, "medv"]

boost.boston = gbm(medv ~ ., data = Boston[train, ],
                   distribution = "gaussian", n.trees = 5000,
                   interaction.depth = 4
                   )

summary(boost.boston)

# partial dependence plots
# marginal effect of selected var's on response after integrating out others
plot(boost.boston, i = "rm")
plot(boost.boston, i = "lstat")

# prediction
yhat.boost = predict(boost.boston,
                     newdata = Boston[-train, ], n.trees = 5000)
mean((yhat.boost - boston.test)^2)

# change shrinkage param
boost.boston = gbm(medv ~ ., data = Boston[train, ],
                   distribution = "gaussian", n.trees = 5000,
                   interaction.depth = 4, shrinkage = 0.2, verbose=F
)
yhat.boost = predict(boost.boston,
                     newdata = Boston[-train, ], n.trees = 5000)
mean((yhat.boost - boston.test)^2)

