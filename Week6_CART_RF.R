
library(mvtnorm)
library(class)
library(caret)
library(rpart)
library(randomForest)
library(ModelMetrics)
library(ipred)

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
# 	sep=",",head=T,row.names=1)

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

################### Bagging ############################################

# Bagging for Simulated Data 1
baggedTree.sim1 = bagging(y~x.1+x.2, data = sim.data1, nbagg=100)

# Fitted Values in class probabilities
myBaggingPred.sim1 = predict(baggedTree.sim1, newdata=sim.data1[2:3], 
                             type = 'prob')

# Fitted response labels
res = ifelse(myBaggingPred.sim1[, 1] > 0.5, 1, 2)

# Plotting the data with fitted labels based on the model
plot(sim.data1$x.1, sim.data1$x.2, pch = as.numeric(sim.data1$y),
     col = factor(res))

# Misclassification errors in the training data
table(sim.data1$y, res)

# Bagging for Simulated Data 2
baggedTree.sim2 = bagging(y~x.1+x.2, data = sim.data2, nbagg=100)

# Fitted Values in class probabilities
myBaggingPred.sim2 = predict(baggedTree.sim2, newdata=sim.data2[2:3],
                             type = 'prob')
# Fitted response labels
res = ifelse(myBaggingPred.sim2[, 1] > 0.5, 1, 2)

# Plotting the data with fitted labels based on the model
plot(sim.data2$x.1, sim.data2$x.2, pch = as.numeric(sim.data2$y),
     col = factor(res))
# Misclassification errors in the training data
table(sim.data2$y, res)

# Bagging for Digit Recognition Data
baggedTree.digit = ipredbagg(y=factor(digit_no.tr), X = digit_pix.tr, nbagg=50)
# Predicted response labels
myBaggingPred.digit = predict(baggedTree.digit, newdata=digit_pix.test,
                              type = 'class')
# Misclassification errors in the test data
table(myBaggingPred.digit, digit_no.test)

################### Random Forest ############################################

# Random Forest Classifier for Simulated Data 1
rf.sim1 = randomForest(sim.data1[,2:3], factor(sim.data1$y))
# Fitted response labels
pred.rf.sim1 = rf.sim1$predicted
# Plotting the data with fitted labels based on the model
plot(sim.data1$x.1, sim.data1$x.2, pch = as.numeric(sim.data1$y), 
     col = factor(pred.rf.sim1))
# Misclassification errors in the training data
table(sim.data1$y, pred.rf.sim1)

# Random Forest Classifier for Simulated Data 2
rf.sim2 = randomForest(sim.data2[,2:3], factor(sim.data2$y))
# Fitted response labels
pred.rf.sim2 = rf.sim2$predicted
# Plotting the data with fitted labels based on the model
plot(sim.data2$x.1, sim.data2$x.2, pch = as.numeric(sim.data2$y), 
     col = factor(pred.rf.sim2))
# Misclassification errors in the training data
table(sim.data2$y, pred.rf.sim2)

# Random Forest Classifier for Digit Recognition Data
rf.digit = randomForest(digit_pix.tr, factor(digit_no.tr), 
                        xtest = digit_pix.test, ytest = factor(digit_no.test), 
                        importance = T, proximity = T)
# Predicted response labels
rfPred.digit = (rf.digit$test)$predicted
# Misclassification errors in the test data
table(digit_no.test, rfPred.digit)  
# Variable Importance Plot
varImpPlot(rf.digit)  
# digit.rf.mds = cmdscale(1 - rf.digit$proximity, k = 2, eig=TRUE)
# plot(digit.rf.mds$points[,1], digit.rf.mds$points[,2], col = digit_no.tr)


