library(MASS)
library(mvtnorm)
library(class)
library(caret)

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
# read.table("http://www-stat.stanford.edu/~tibs/
#            ElemStatLearn/datasets/SAheart.data",
#            sep=",",head=T,row.names=1)

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

################### Linear Discriminant Analysis ###############################

# Handwritten Digit Recognition Data
# Fitting the model to training data
lda_digit = lda(x = digit_pix.tr, grouping = digit_no.tr)
# Predicted labels for test data
pr.no = predict(lda_digit, digit_pix.test)$class
# Misclassification errors in the test data
table(factor(pr.no), factor(digit_no.test))

# Simulated Data
# Fitting the model to the data
lda.sim1 = lda(x = sim.data1[,2:3], grouping = factor(sim.data1$y))
# Plotting the data with fitted labels based on the model
plot(sim.data1$x.1, sim.data1$x.2, pch = sim.data1$y, 
     col = predict(lda.sim1)$class)
# Fitting the model to the data
lda.sim2 = lda(x = sim.data2[,2:3], grouping = factor(sim.data2$y))
# Plotting the data with fitted labels based on the model
plot(sim.data2$x.1, sim.data2$x.2, pch = sim.data2$y, 
     col = predict(lda.sim2)$class)

################### k-Nearest Neighbor ########################################

# Simulated Data
# Fitting the model on the data
knn.sim1 = knn.cv(sim.data1[,2:3], sim.data1$y)
# Plotting the data with fitted labels based on the model
plot(sim.data1$x.1, sim.data1$x.2, pch = sim.data1$y, col = knn.sim1)
# Fitting the model on the data
knn.sim2 = knn.cv(sim.data2[,2:3], sim.data2$y)
# Plotting the data with fitted labels based on the model
plot(sim.data2$x.1, sim.data2$x.2, pch = sim.data2$y, col = knn.sim2)

# Handwritten Digit Recognition Data
# Fitting the model to training data and finding labels for test data
knn.digit = knn(digit_pix.tr, digit_pix.test, digit_no.tr, k=1)
# Misclassification errors in the test data
table(digit_no.test, knn.digit)
# Confusion matrix or misclassification error matrix
cm.knn = confusionMatrix(data = factor(knn.digit), 
                         reference = factor(digit_no.test))
# F_1 score of classification accuracy (higher the better, maximum is 1)
mean(cm.knn$byClass[,7])

# Which 'k' for k-NN gives highest F_1 score?
meanAcc = rep(0, 10)
cm = vector('list', 10)
for(k in 1:10) {
  knn.digit = knn(digit_pix.tr, digit_pix.test, digit_no.tr, k=k)
  cm[[k]] = confusionMatrix(data = factor(knn.digit), 
                            reference = factor(digit_no.test))
  meanAcc[k] = mean(cm[[k]]$byClass[,7])
}
# Plot of F_1 accuracies
plot(1:10, meanAcc, xlab = 'k', ylab = 'Mean F_1 Accuracy')









