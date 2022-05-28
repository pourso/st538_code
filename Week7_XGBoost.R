
library(mvtnorm)
library(caret)
library(xgboost)
library(DiagrammeR)

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

################### XGBoost ############################################

# XGBoost Classifier for Simulated Data 2
n = dim(sim.data2)[1]
train.data = as.matrix(sim.data2[,2:3])
train.labels = as.numeric(sim.data2$y) - 1
xgb.sim1 = xgboost(data = train.data, label = train.labels, 
                  max.depth = 6, eta = 1, nrounds = 100,
                  objective = "binary:logistic", eval_metric = "logloss")
# Early Stopping using cross-validation
xgb.sim.cv = xgb.cv(data = train.data, label = train.labels, 
                    max.depth = 6, eta = 1, nrounds = 100,
                    nfold = 5, early_stopping_rounds = 5,
                    objective = "binary:logistic", eval_metric = "logloss")
sel_rounds = xgb.sim.cv$best_iteration
xgb.sim2 = xgboost(data = train.data, label = train.labels, 
                  max.depth = 6, eta = 1, nrounds = sel_rounds,
                  objective = "binary:logistic", eval_metric = "logloss")
# Fitted response labels
pred1 = predict(xgb.sim1, train.data)
pred.xgb.sim1 = ifelse(pred1 > 0.5, 2, 1)

pred2 = predict(xgb.sim2, train.data)
pred.xgb.sim2 = ifelse(pred2 > 0.5, 2, 1)
# Plotting the data with fitted labels based on the model
plot(sim.data2$x.1, sim.data2$x.2, pch = as.numeric(sim.data2$y),
     col = factor(pred.xgb.sim1))
plot(sim.data2$x.1, sim.data2$x.2, pch = as.numeric(sim.data2$y),
     col = factor(pred.xgb.sim2))
# Plotting the first two trees in the boosted classifier
xgb.plot.tree(model = xgb.sim1, trees = 1:2)
# Misclassification errors in the training data
table(sim.data2$y, pred.xgb.sim1)
table(sim.data2$y, pred.xgb.sim2)

############ XGB Classifier for Digit Recognition Data

train.data = as.matrix(digit_pix.tr)
prt = proc.time()
xgb.digit1 = xgboost(data = train.data, label = digit_no.tr, 
                   max.depth = 6, eta = 1, nrounds = 100,
                   objective = "multi:softmax", num_class = 10,
                   eval_metric = "mlogloss")
time1 = proc.time() - prt

# Predicted response labels
test.data = as.matrix(digit_pix.test)
xgb.pred.digit1 = predict(xgb.digit1, test.data)

# Misclassification errors in the test data
cm.gbm1 = confusionMatrix(factor(xgb.pred.digit1), factor(digit_no.test))
cm.gbm1$byClass
cm.gbm1$table
# F1 score of classification accuracy for test data
mean(cm.gbm1$byClass[,7])

# Feature Importance
importance_matrix = xgb.importance(model = xgb.digit1)
xgb.plot.importance(importance_matrix)

