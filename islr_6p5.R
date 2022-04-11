library(ISLR2)
View(Hitters)
names(Hitters)

dim(Hitters)

# check for missing values, remove observations w/ missing values
sum(is.na(Hitters$Salary))
Hitters = na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))


# best subset selection based on RSS
library(leaps)
regfit.full = regsubsets(Salary ~ ., Hitters)
summary(regfit.full)

# default: 8 var max, use nvmax to increase
regfit.full = regsubsets(Salary ~ ., Hitters, nvmax=19)
reg.summary = summary(regfit.full)
names(reg.summary)

# as expected R^2 increases w/ p
reg.summary$rsq

tb = tibble(
  p = 1:length(reg.summary$rsq),
  rsq = reg.summary$rsq,
  rss = reg.summary$rss,
  adjr2 = reg.summary$adjr2,
  cp = reg.summary$cp,
  bic = reg.summary$bic,
)
ggplot(tb, aes(x = p,y = rsq)) +
  geom_line(color = 'tomato') +
  geom_line(aes(y = adjr2), color = 'cornflowerblue') +
#  geom_line(aes(y = cp), color = 'chartreuse') +
#  geom_line(aes(y = bic), color = 'magenta') +
  theme_minimal() +
  labs(
    title = "R^2 and adjusted R^2"
  )

# plot adjr2 and place dot at max
par ( mfrow = c(2, 2))
plot ( reg.summary $rss , xlab = " Number of Variables ",
         ylab = " RSS ", type = "l")
plot ( reg.summary $adjr2 , xlab = " Number of Variables ",
         ylab = " Adjusted RSq ", type = "l")
which.max ( reg.summary $ adjr2 )
points (11 , reg.summary $ adjr2 [11] , col = " red ", cex = 2,
          pch = 20)

# plot Cp and BIC and place dot at min
plot ( reg.summary $cp , xlab = " Number of Variables ",
       ylab = "Cp", type = "l")
which.min ( reg.summary $cp)
points (10 , reg.summary $cp [10] , col = " red ", cex = 2,
          pch = 20)
which.min ( reg.summary $bic)
plot ( reg.summary $bic , xlab = " Number of Variables ",
         ylab = " BIC ", type = "l")
points (6, reg.summary $ bic [6] , col = " red ", cex = 2,
        pch = 20)

# regsubset built-in plot
plot ( regfit.full , scale = "r2")
plot ( regfit.full , scale = "adjr2")
plot ( regfit.full , scale = "Cp")
plot ( regfit.full , scale = "bic")

# coeff command to see estimates w/ p==6 model
coef(regfit.full, 6)


# fwd and bwd step-wise selection
regfit.fwd = regsubsets(Salary ~ ., data=Hitters, 
                        nvmax = 19, method = "forward")
summary(regfit.fwd)
regfit.bwd = regsubsets(Salary ~ ., data=Hitters, 
                        nvmax = 19, method = "backward")
summary(regfit.bwd)


# validation set and cross-val approach


## use train-test split to find # of vars to use, then retrain
##  to get best vars
##
## below seems quite variable: p changes quite a bit w/ diff seeds
# train-test split
set.seed(1)
train = sample(c(T, F), nrow(Hitters), replace=T)
test = !train

# best subset w/ train set
regfit.best = regsubsets(Salary ~., data = Hitters[train,],
                         nvmax = 19)

# used by regression packages to build "X" matrix from data
test.mat = model.matrix(Salary ~ ., data=Hitters[test, ])

val.errors = rep(NA, 19)
for(i in 1:19) {
  coefi = coef(regfit.best, id=i)
  pred = test.mat[,names(coefi)] %*% coefi
  val.errors[i] = mean((Hitters$Salary[test] - pred)^2)
}
val.errors
which.min(val.errors)

coef(regfit.best, 7)

# function to do above
predict.regsubsets <- function (object , newdata , id , ...) {
  form <- as.formula ( object $ call [[2]])
  mat <- model.matrix (form , newdata )
  coefi <- coef ( object , id = id)
  xvars <- names ( coefi )
  mat [, xvars ] %*% coefi
}

# best subset on full data set w/ 7 vars
regfit.best = regsubsets(Salary ~., data=Hitters, 
                         nvmax = 7)
coef(regfit.best, 7)


## cross-validation
# must perform best subset selection w/in each of k folds
k = 10
n = nrow(Hitters)
set.seed(1)
folds = sample(rep(1:k, length=n))
cv.errors = matrix(NA, k, 19,
                   dimnames = list(NULL, paste(1:19)))

# for loop for cross-val
for(j in 1:k) {
  # best subset selection based on training data
  best.fit = regsubsets(Salary~.,
                        data=Hitters[folds !=j,],
                        nvmax = 19)
  # for each p-sized var space, get test error
  for (i in 1:19) {
    pred = predict(best.fit, Hitters[folds==j,], id = i)
    cv.errors[j,i] = mean((Hitters$Salary[folds==j]-pred)^2)
  }
}

# now average cols to get cross-val error for each p-var model
mean.cv.errors = apply(cv.errors, 2, mean)
mean.cv.errors

# 10-var model is best
which.min(mean.cv.errors)

reg.best = regsubsets(Salary ~., data = Hitters, nvmax = 19)
coef(reg.best, 10)

# 6.5.2 Ridge regression and lasso