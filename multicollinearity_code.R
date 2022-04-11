library(faraway)
library(ggplot2)

###############################################################

head(seatpos)

qplot(Ht, hipcenter, data = seatpos)

lmod <- lm(hipcenter ~ ., data = seatpos)
summary(lmod) # Note the large SE of the estimates as well as the high R-squared value

# Variance Inflation Factors
vif(lmod)
