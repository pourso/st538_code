library(MASS)
library(mvtnorm)
library(class)
library(caret)
library(rpart)
library(randomForest)
library(ModelMetrics)
library(ipred)

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

################### Classification Tree #########################################

# Classification Tree for Simulated Data 1
rpart.sim1 = rpart(y~x.1+x.2, data = sim.data1)
# display the results
printcp(rpart.sim1) 
# visualize cross-validation results
plotcp(rpart.sim1) 
# detailed summary of splits
summary(rpart.sim1, pretty = T) 
# plot tree
plot(rpart.sim1, uniform=TRUE,
     main="Classification Tree")
text(rpart.sim1, use.n=TRUE, all=TRUE, cex=.5)
# prune the tree
pfit<- prune(rpart.sim1, 
             cp = rpart.sim1$cptable[which.min(rpart.sim1$cptable[,"xerror"]),
                                     "CP"])
# plot the pruned tree
plot(pfit, uniform=TRUE,
     main="Pruned Classification Tree")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
# Fitted Values in class probabilities
res1 = predict(rpart.sim1, sim.data1[2:3])
# Fitted response labels
res = ifelse(res1[, 1] > 0.5, 1, 2)
# Plotting the data with fitted labels based on the model
plot(sim.data1$x.1, sim.data1$x.2, pch = as.numeric(sim.data1$y),
     col = factor(res))
# Misclassification errors in the training data
table(sim.data1$y, res)

# Classification Tree for Simulated Data 2
rpart.sim2 = rpart(y~x.1+x.2, data = sim.data2)
# display the results
printcp(rpart.sim2) 
# visualize cross-validation results
plotcp(rpart.sim2) 
# detailed summary of splits
summary(rpart.sim2, pretty = T) 
# plot tree
plot(rpart.sim2, uniform=TRUE,
     main="Classification Tree")
text(rpart.sim2, use.n=TRUE, all=TRUE, cex=.5)
# prune the tree
pfit<- prune(rpart.sim2, 
             cp = rpart.sim2$cptable[which.min(rpart.sim2$cptable[,"xerror"]),
                                     "CP"])
# plot the pruned tree
plot(pfit, uniform=TRUE,
     main="Pruned Classification Tree")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
# Fitted Values in class probabilities
res1 = predict(rpart.sim2, sim.data2[2:3])
# Fitted response labels
res = ifelse(res1[,1] > 0.5, 1, 2)
# Plotting the data with fitted labels based on the model
plot(sim.data2$x.1, sim.data2$x.2, pch = as.numeric(sim.data2$y),
     col = factor(res))
# Misclassification errors in the training data
table(sim.data2$y, res)

# Classification Tree for Digit Recognition Data
# Taining Data
digit.dt <- data.frame(x=digit_pix.tr, y=factor(digit_no.tr))
# Test Data
digit.dt1 <- data.frame(x=digit_pix.test, y=factor(digit_no.test))

rpart.digit = rpart(y~., data = digit.dt, method = 'class')

# Predicted Values in class probabilities
res1 = predict(rpart.digit, newdata = digit.dt1[,1:256], type = 'prob')
# Predicted response labels
res = apply(res1, 1, function(x) {which.max(x)}) - 1
# Misclassification errors in the test data
table(digit_no.test, factor(res))

