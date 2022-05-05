library(tree)
library(ISLR2)

attach(Carseats)

# recode continuous sales as binary
High = factor(ifelse(Sales <= 8, "No", "Yes"))

# attach back to dataframe
Carseats = data.frame(Carseats, High)

set.seed(1)

# model binary var as classification tree
tree.carseats = tree(High ~ . - Sales, Carseats)

summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty=0)

# prints each branch of tree
# - split criterion
# - # of obsv's
# - deviance
# - overall prediction
# - fraction of observations 
# - * denotes terminal node
tree.carseats

set.seed(2)

# train/ test split
train = sample(1:nrow(Carseats), 200)
Carseats.test = Carseats[-train, ]
High.test = High[-train]

# model
tree.carseats = tree(High ~ . - Sales, Carseats, subset = train)

# predictions
tree.pred = predict(tree.carseats, Carseats.test, type="class")

# ~75% right
table(tree.pred, High.test)


## Pruning (cost complexity)
set.seed(2)
# misclassification error for pruning
cv.carseats = cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)

cv.carseats

par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type="b")
plot(cv.carseats$k, cv.carseats$dev, type="b")

# pick # terminal nodes based on cv
prune.carseats = prune.misclass(tree.carseats, best=5)

# full size app to avoid plot errors
plot(prune.carseats)
text(prune.carseats, pretty=0)

# assess on test data
tree.pred1 = predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred1, High.test)



