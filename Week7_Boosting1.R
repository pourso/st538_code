
library(mvtnorm)
library(caret)
library(gbm)
library(adabag)

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
# Response labels
pin1 = sample(1:2, n, replace = T, prob = rep(1/2, 2)) 
# Parameters for simulating predictors
mu = matrix(c(0,0, 3,3), nrow = 2, ncol=2, byrow = T) 
sd = c(0.5,1) 
# Simulated Predictors
xsim1 = matrix(0, nrow = n, ncol = 2)  
for(i in 1:n) {
  xsim1[i,] = rmvnorm(1, mu[(pin1[i]),], sd[pin1[i]]*diag(2))  
}
# Plot of the data colored by response labels
plot(xsim1[,1], xsim1[,2], col = factor(pin1))  
# Simulated Data
sim.data1 = data.frame(y=factor(pin1), x=xsim1)  

# Data with overlapping labels of response
n=200
# Response labels
pin2 = sample(1:2, n, replace = T, prob = rep(1/2, 2))  
# Parameters for simulating predictors
mu = matrix(c(0,0, 2,2), nrow = 2, ncol=2, byrow = T)  
sd = c(0.5,1.5) 
# Simulated Predictors
xsim2 = matrix(0, nrow = n, ncol = 2)  
for(i in 1:n) {
  xsim2[i,] = rmvnorm(1, mu[(pin2[i]),], sd[pin2[i]]*diag(2))  
}
# Plot of the data colored by response labels
plot(xsim2[,1], xsim2[,2], col = factor(pin2))
# Simulated Data
sim.data2 = data.frame(y=factor(pin2), x=xsim2) 

################### Boosting ############################################

# Random Forest Classifier for Simulated Data 1
n = dim(sim.data1)[1]
gbm.sim1 = gbm.fit(x=sim.data1[,2:3], y=(as.numeric(sim.data1$y)-1),
                  distribution = 'adaboost', n.trees = 2500, nTrain = n*0.7,
                  verbose = F)

# Fitted response labels
pred.gbm.sim1 = ifelse(gbm.sim1$fit > 0, 2, 1)

# Plotting the data with fitted labels based on the model
plot(sim.data1$x.1, sim.data1$x.2, pch = as.numeric(sim.data1$y),
     col = factor(pred.gbm.sim1))

# Misclassification errors in the training data
table(sim.data1$y, pred.gbm.sim1)

# Error curves for training and validation vs. iterations
gbm.perf(gbm.sim1)


# Random Forest Classifier for Simulated Data 2
n = dim(sim.data2)[1]
gbm.sim = gbm.fit(x=sim.data2[,2:3], y=(as.numeric(sim.data2$y)-1),
                  distribution = 'adaboost', n.trees = 2500, nTrain = n*0.7,
                  verbose = F)

# Fitted response labels
pred.gbm.sim = ifelse(gbm.sim$fit > 0, 2, 1)

# Plotting the data with fitted labels based on the model
plot(sim.data2$x.1, sim.data2$x.2, pch = as.numeric(sim.data2$y),
     col = factor(pred.gbm.sim))

# Misclassification errors in the training data
table(sim.data2$y, pred.gbm.sim)

# Error curves for training and validation vs. iterations
gbm.perf(gbm.sim)

# Boosting Classifier for Digit Recognition Data
n = dim(digit_tr)[1]
gbm.digit = gbm.fit(digit_pix.tr, factor(digit_no.tr),
                    distribution = 'multinomial', n.trees = 500,
                    shrinkage = 0.05, nTrain = (0.7*(dim(digit_pix.tr)[1])))
# gbm.digit = gbm(V1 ~ ., data = digit_tr,
#                 n.trees = 500, shrinkage = 0.05, train.fraction = 0.7,
#                 verbose = T)
# Error curves for training and validation vs. iterations
gbm.perf(gbm.digit)

par(mfrow = c(1,2))
plot(gbm.digit$train.error)
plot(gbm.digit$valid.error)

# Fitted response labels
boostPred.digit = predict(gbm.digit, digit_pix.test, n.trees = 500)
res = apply(boostPred.digit, 1, which.max)-1

# Misclassification errors in the test data
cm.gbm = confusionMatrix(factor(res), factor(digit_no.test))
cm.gbm$byClass
cm.gbm$table

# F1 score of classification accuracy for test data
mean(cm.gbm$byClass[,7])

