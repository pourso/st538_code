library(ggplot2)
library(faraway)

###############################################################
# Dataset
?fat
X <- as.matrix(fat[, 9:18])
y <- as.numeric(fat[, 1])

##### Elastic Net Estimates
library(glmnet)

elnet <- glmnet(X, y, alpha = 0.5)
elnet.cv <- cv.glmnet(X, y, alpha = 0.5)
# Finding the Elastic net estimates for lambda with minimum cross-validation error
elnet1 <- glmnet(X, y, lambda = elnet.cv$lambda.min, alpha = 0.5)
# Finding the Elastic net estimates for largest lambda within 
# 1 std. dev. of minimum cross-validation error
elnet2 <- glmnet(X, y, lambda = elnet.cv$lambda.1se, alpha = 0.5)
# Extracting Elastic net estimates of coefficients from glmnet object
elnet1$beta
# Extracting Elastic net estimates of coefficients from glmnet object
elnet2$beta
# Extracting Elastic net estimates of coefficients from a cv.glmnet object
coef(elnet.cv)

##### Adaptive Lasso Estimates
# library(parcor)
# 
# adlasso <- adalasso(X, y)
# adlasso$coefficients.adalasso

##### BoLasso Estimates
# Package mht is not available in CRAN anymore
# You have to download the package from this website
# https://cran.r-project.org/src/contrib/Archive/mht/
# then install the package from your computer
library(mht) 

bolas <- bolasso(X, y, mu = seq(5, 0.1, -0.1))
# Extracting Selection Sets for all values of lambda
sel.var <- bolas$ind
sel.var
# Extracting predictor valriables for the selection set
# corresponding to the smallest value of lambda
sel.X <- X[, sel.var[-1, 50]]
dt <- data.frame(cbind(y, sel.X))
head(dt)
# Fitting a linear model on the selected variables
bolas.lm <- lm(y ~ abdom + wrist, data = dt)
bolas.lm$coefficients

##### Non-convex Penalty Estimates
library(ncvreg)

# SCAD Penalty
ncv.reg1 <- ncvreg(X, y, penalty="SCAD")
# Plot the values of coefficients for different values of lambda
plot(ncv.reg1)

ncv.reg.cv <- cv.ncvreg(X, y, penalty="SCAD")
# Finding the coefficient estimates for lambda with minimum cross-validation error
id <- which(ncv.reg1$lambda == ncv.reg.cv$lambda.min)
ncv.reg1$beta[, id]

# MCP Penalty
ncv.reg2 <- ncvreg(X, y, penalty="MCP")
# Plot the values of coefficients for different values of lambda
plot(ncv.reg2)

ncv.reg.cv <- cv.ncvreg(X, y, penalty="MCP")
# Finding the coefficient estimates for lambda with minimum cross-validation error
id <- which(ncv.reg2$lambda == ncv.reg.cv$lambda.min)
ncv.reg2$beta[, id]

##### Group Lasso Estimates
library(grpreg)

# Grouping the 10 predictors into 8 groups
group <- c(1,2,3,4,5,5,6,7,8,8)

grplasso <- grpreg(X, y, group, penalty="grLasso")
# Plot the values of coefficients for different values of lambda
plot(grplasso)

grplasso.cv <- cv.grpreg(X, y, penalty="grLasso")
# Extracting coefficient estimates from a cv.grpreg object
coef(grplasso.cv)
# Finding the Elastic net estimates for largest lambda within 
# 1 std. dev. of minimum cross-validation error
sel.1se <- which.min(grplasso.cv$cve > 
                       (grplasso.cv$cve[grplasso.cv$min] +
                          grplasso.cv$cvse[grplasso.cv$min]))
sel.lambda <- grplasso.cv$lambda[sel.1se]
grplasso1 <- grpreg(X, y, lambda = sel.lambda, penalty="grLasso")
grplasso1$beta

##### Group MCP Estimates
library(grpreg)

# Grouping the 10 predictors into 8 groups
group <- c(1,2,3,4,5,5,6,7,8,8)

grpncv <- grpreg(X, y, group, penalty="grMCP")
# Plot the values of coefficients for different values of lambda
plot(grpncv)

grpncv.cv <- cv.grpreg(X, y, penalty="grMCP")
# Extracting coefficient estimates from a cv.grpreg object
coef(grpncv.cv)
# Finding the Elastic net estimates for largest lambda within 
# 1 std. dev. of minimum cross-validation error
sel.1se <- which.min(grpncv.cv$cve > 
                       (grpncv.cv$cve[grpncv.cv$min] +
                          grpncv.cv$cvse[grpncv.cv$min]))
sel.lambda <- grpncv.cv$lambda[sel.1se]
grpncv1 <- grpreg(X, y, lambda = sel.lambda, penalty="grMCP")
grpncv1$beta

