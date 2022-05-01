library(MASS)
library(mvtnorm)
library(class)
library(caret)
library(e1071)

################## Handwritten Digit Recognition Data ######################

digit = read.table('Week4_Digit_recog_train.txt')
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
sim.data1 = data.frame(y=factor(pin1), x=xsim1)

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
sim.data2 = data.frame(y=factor(pin2), x=xsim2)

################# SVM ######################################
library(e1071)

# Simulated Data 1
svm.sim1 = svm(y~x.1+x.2, data = sim.data1, kernel = 'radial')
# svm.sim1 = svm(y~x.1+x.2, data = sim.data1, kernel = 'polynomial')
# Fitted Values in class labels
res = predict(svm.sim1, sim.data1[2:3])
# Plotting the data with fitted labels based on the model
plot(sim.data1$x.1, sim.data1$x.2, pch = as.numeric(sim.data1$y),
     col = factor(res))
legend("topleft", c("True 0, Estimated 0", "True 0, Estimated 1",
                    "True 1, Estimated 0", "True 1, Estimated 0"),
       pch = c(0,0,1,1), col = c(1,2,1,2), cex = 0.5)
# Misclassification errors in the training data
table(sim.data1$y, res)

# Simulated Data 2
svm.sim2 = svm(y~x.1+x.2, data = sim.data2, kernel = 'radial')
# svm.sim2 = svm(y~x.1+x.2, data = sim.data2, kernel = 'polynomial')
# Fitted Values in class labels
res = predict(svm.sim2, sim.data2[2:3])
plot(sim.data2$x.1, sim.data2$x.2, pch = as.numeric(sim.data2$y),
     col = factor(res))
legend("topleft", c("True 0, Estimated 0", "True 0, Estimated 1",
                    "True 1, Estimated 0", "True 1, Estimated 0"),
       pch = c(0,0,1,1), col = c(1,2,1,2), cex = 0.5)
# Misclassification errors in the training data
table(sim.data2$y, res)

# Handwritten Digit Recognition Data
svm.digit = svm(x = digit_pix.tr, y = digit_no.tr, kernel = 'radial')
# svm.digit = svm(x = digit_pix.tr, y = digit_no.tr, kernel = 'polynomial')
# Fitted Values in class labels
res = predict(svm.digit, digit_pix.test, decision.values = T)
res = abs(round(res))
# Misclassification errors in the test data
table(digit_no.test, factor(res))

