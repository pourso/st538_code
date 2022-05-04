# library(MASS)

library(rpart)

####### Analysis of Baseball Hitters Data #################################
library(ISLR)

# Taking log-transformation of salary
Hitters$log_salary <- log(Hitters$Salary)  
# Deleting NA observations
Hitters1 <- Hitters[!is.na(Hitters$log_salary), ]  
# Training Data id with 70-30 split of training and test data
tr_sample <- sample(1:length(Hitters1$Salary), 0.7*length(Hitters1$Salary)) 
# Training Data
Hitters1_tr <- Hitters1[tr_sample, ]  
# Test Data
Hitters1_test <- Hitters1[-tr_sample, ]  

# Regression Tree for Baseball Hitters Data
formula <- paste('log_salary ~ ', paste(names(Hitters1)[1:18], collapse = '+'))
rpart1 <- rpart(formula, data = Hitters1_tr)
# display the results
printcp(rpart1) 
# visualize cross-validation results
plotcp(rpart1) 
# detailed summary of splits
summary(rpart1, pretty = T) 
# plot tree
plot(rpart1, uniform=TRUE,
     main="Classification Tree")
text(rpart1, use.n=TRUE, all=TRUE, cex=.5)
# prune the tree
pfit<- prune(rpart1, 
             cp= rpart1$cptable[which.min(rpart1$cptable[,"xerror"]),"CP"])
# plot the pruned tree
plot(pfit, uniform=TRUE,
     main="Pruned Classification Tree")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
# Prediction
Hitters1_test$pred <- predict(rpart1, Hitters1_test)
qplot(log_salary, pred, data = Hitters1_test) + 
  geom_line(aes(log_salary, log_salary))
