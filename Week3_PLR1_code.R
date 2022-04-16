library(ggplot2)
library(glmnet)
library(faraway)

###############################################################

?fat
X <- as.matrix(fat[, 9:18])
y <- as.numeric(fat[, 1])

##### Lasso Estimates
lasso <- glmnet(X, y)
plot(lasso)

lasso.cv <- cv.glmnet(X, y)
lasso1 <- glmnet(X, y, lambda = lasso.cv$lambda.min)
lasso1$beta
lasso2 <- glmnet(X, y, lambda = lasso.cv$lambda.1se)
lasso2$beta

coef(lasso.cv)

##### Ridge Estimates
ridge <- glmnet(X, y, alpha = 0)
ridge.cv <- cv.glmnet(X, y, alpha = 0)
ridge1 <- glmnet(X, y, lambda = ridge.cv$lambda.min, alpha = 0)
ridge1$beta

coef(ridge.cv)

