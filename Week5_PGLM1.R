library(MASS)
library(mvtnorm)
library(class)
library(caret)
library(glmnet)

#######SOUTH AFRICAN HEART DISEASE #########################################

sa.HeartDisease<-read.table(
  "http://web.stanford.edu/~hastie/ElemStatLearn/datasets/SAheart.data",
  sep=",",head=T,row.names=1)	
# A retrospective sample of males in a heart-disease high-risk region
# of the Western Cape, South Africa. There are roughly two controls per
# case of CHD. Many of the CHD positive men have undergone blood
# pressure reduction treatment and other programs to reduce their risk
# factors after their CHD event. In some cases the measurements were
# made after these treatments. These data are taken from a larger
# dataset, described in  Rousseauw et al, 1983, South African Medical
# Journal. 
# 
# sbp		systolic blood pressure
# tobacco		cumulative tobacco (kg)
# ldl		low densiity lipoprotein cholesterol
# adiposity
# famhist		family history of heart disease (Present, Absent)
# typea		type-A behavior
# obesity
# alcohol		current alcohol consumption
# age		age at onset
# chd		response, coronary heart disease
# 
# To read into R:
# read.table(
#   "http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/SAheart.data", 
#   sep=",",head=T,row.names=1)

################## Handwritten Digit Recognition Data ######################

digit = read.table('Digit_recog_train.txt')
# Response
digit_no = digit[,1]
# Predictors
digit_pix = digit[,-1]
# Response for only digit 3
digit_no3 = digit_no[digit_no == 3]
# Predictors for Response having value only 3
digit_pix3 = digit_pix[digit_no == 3,]
# Response for only digits 1 and 3
digit_no13 = digit_no[union(which(digit_no == 1), which(digit_no == 3))]
# Predictors for Response having value only 1 and 3
digit_pix13 = digit_pix[union(which(digit_no == 1), which(digit_no == 3)),]

# Dividing the dataset into training and test sets in a 3:1 ratio
sel = sample(c(0,1), length(digit_no), replace = T, prob = c(1/4,3/4))
# Predictors for the training data
digit_pix.tr = digit_pix[sel == 1,]
# Response for the training data
digit_no.tr = digit_no[sel==1]
# Whole training data
digit_tr = digit[sel==1,]
# Predictors for the test data
digit_pix.test = digit_pix[sel!=1,]
# Response for the test data
digit_no.test = digit_no[sel!=1]

# Dividing the dataset with only digits 1 and 3
# into training and test sets in a 3:1 ratio
sel = sample(c(0,1), length(digit_no13), replace = T, prob = c(1/4,3/4))
# Predictors for the training data
digit_pix.tr13 = digit_pix13[sel == 1,]
# Response for the training data
digit_no.tr13 = digit_no13[sel==1]
# Response for the training data made into factor variable
digit_no.tr13 = factor(digit_no.tr13)
# Predictors for the test data
digit_pix.test13 = digit_pix13[sel != 1,]
# Response for the test data
digit_no.test13 = digit_no13[sel!=1]

################### Simulated Data ############################################

# Data with non-overlapping labels of response
n=200
# Response
pin1 = sample(1:2, n, replace = T, prob = rep(1/2, 2))
# Generation of predictors
mu = matrix(c(0,0, 3,3), nrow = 2, ncol=2, byrow = T)
sd = c(0.5,1)
xsim1 = matrix(0, nrow = n, ncol = 2)
for(i in 1:n) {
  xsim1[i,] = rmvnorm(1, mu[pin1[i],], sd[pin1[i]]*diag(2))
}
# Plot of the data colored by response labels
plot(xsim1[,1], xsim1[,2], col = factor(pin1))
sim.data1 = data.frame(y=pin1, x=xsim1)

# Data with overlapping labels of response
n=200
# Response
pin2 = sample(1:2, n, replace = T, prob = rep(1/2, 2))
# Generation of predictors
mu = matrix(c(0,0, 2,2), nrow = 2, ncol=2, byrow = T)
sd = c(0.5,1.5)
xsim2 = matrix(0, nrow = n, ncol = 2)
for(i in 1:n) {
  xsim2[i,] = rmvnorm(1, mu[pin2[i],], sd[pin2[i]]*diag(2))
}
# Plot of the data colored by response labels
plot(xsim2[,1], xsim2[,2], col = factor(pin2))
sim.data2 = data.frame(y=pin2, x=xsim2)

################# Penalized Logistic Regression ###############################

# SA Heart Disease Data
X <- sa.HeartDisease[, 1:9]
X$famhist <- ifelse(X$famhist == "Present", 1, 0)
# Predictor variables
X <- as.matrix(X)
# Response variable
y <- as.factor(sa.HeartDisease[, 10])
# Penalized logistic regression
glm.HD <- glmnet(X, y, family = "binomial")
summary(glm.HD)
plot(glm.HD)

glm.HD.cv <- cv.glmnet(X, y, family = "binomial")
# Finding the coefficient estimates for lambda with 
# minimum cross-validation error
glm.HD1 <- glmnet(X, y, family = "binomial", lambda = glm.HD.cv$lambda.min)
# Extracting coefficient estimates from a glmnet object
glm.HD1$beta
# Finding the for largest lambda within 1 std. dev. of 
# minimum cross-validation error
glm.HD2 <- glmnet(X, y, family = "binomial", lambda = glm.HD.cv$lambda.1se)
# Extracting coefficient estimates from a glmnet object
glm.HD2$beta
# Fitted values
res = predict(glm.HD1, X, type = 'response')
res[res > 0.5] = 1
res[res <= 0.5] = 0
table(y, res)
res = predict(glm.HD2, X, type = 'response')
res[res > 0.5] = 1
res[res <= 0.5] = 0
# Misclassification errors in the training data
table(y, res)

# Handwritten Digit Recognition Data
# Dataset with only digits 1 and 3
# Training data
X <- as.matrix(digit_pix.tr13)
y <- as.factor(digit_no.tr13)
glm_digit = glmnet(X, y, family = "binomial")
plot(glm_digit)
glm_digit.cv <- cv.glmnet(X, y, family = "binomial")
# Finding the coefficient estimates for lambda with 
# minimum cross-validation error
glm_digit1 <- glmnet(X, y, family = "binomial", lambda = glm_digit.cv$lambda.min)
glm_digit1$beta
# Finding the for largest lambda within 1 std. dev. of 
# minimum cross-validation error
glm_digit2 <- glmnet(X, y, family = "binomial", lambda = glm_digit.cv$lambda.1se)
glm_digit2$beta
# Fitted values
res = predict(glm_digit1, as.matrix(digit_pix.test13), type = 'response')
res[res > 0.5] = 1
res[res <= 0.5] = 0
# Misclassification errors in the training data
table(digit_no.test13, res)
# Predicted values
res = predict(glm_digit2, as.matrix(digit_pix.test13), type = 'response')
res[res > 0.5] = 1
res[res <= 0.5] = 0
# Misclassification errors in the test data
table(digit_no.test13, res)

# Simulated Data 1
X <- as.matrix(sim.data1[,2:3])
y <- as.factor(sim.data1[,1])
glm.sim1 = glmnet(X, y, family = "binomial")
plot(glm.sim1)

glm.sim1.cv <- cv.glmnet(X, y, family = "binomial")
# Finding the coefficient estimates for lambda with 
# minimum cross-validation error
glm.sim11 <- glmnet(X, y, family = "binomial", lambda = glm.sim1.cv$lambda.min)
glm.sim11$beta
# Finding the for largest lambda within 1 std. dev. of 
# minimum cross-validation error
glm.sim12 <- glmnet(X, y, family = "binomial", lambda = glm.sim1.cv$lambda.1se)
glm.sim12$beta
# Fitted values
res = predict(glm.sim11, X, type = 'response')
res[res > 0.5] = 1
res[res <= 0.5] = 0
# Plot of the data colored by response labels
plot(sim.data1$x.1, sim.data1$x.2, pch = as.numeric(sim.data1$y),
     col = factor(res))
legend("topleft", c("True 0, Estimated 0", "True 0, Estimated 1",
                    "True 1, Estimated 0", "True 1, Estimated 0"),
       pch = c(0,0,1,1), col = c(1,2,1,2), cex = 0.5)
# Misclassification errors in the training data
table(sim.data1$y, res)

# Simulated Data 2
X <- as.matrix(sim.data2[,2:3])
y <- as.factor(sim.data2[,1])
glm.sim2 = glmnet(X, y, family = "binomial")
plot(glm.sim2)

glm.sim2.cv <- cv.glmnet(X, y, family = "binomial")
# Finding the coefficient estimates for lambda with 
# minimum cross-validation error
glm.sim21 <- glmnet(X, y, family = "binomial", lambda = glm.sim2.cv$lambda.min)
glm.sim21$beta
# Finding the for largest lambda within 1 std. dev. of 
# minimum cross-validation error
glm.sim22 <- glmnet(X, y, family = "binomial", lambda = glm.sim2.cv$lambda.1se)
glm.sim22$beta
# Fitted values
res = predict(glm.sim22, X, type = 'response')
res[res > 0.5] = 1
res[res <= 0.5] = 0
# Plot of the data colored by response labels
plot(sim.data2$x.1, sim.data2$x.2, pch = as.numeric(sim.data2$y),
     col = factor(res))
legend("topleft", c("True 0, Estimated 0", "True 0, Estimated 1",
                    "True 1, Estimated 0", "True 1, Estimated 0"),
       pch = c(0,0,1,1), col = c(1,2,1,2), cex = 0.5)
# Misclassification errors in the training data
table(sim.data2$y, res)
