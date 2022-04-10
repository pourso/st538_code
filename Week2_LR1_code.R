library(Sleuth3)
library(ggplot2)
library(GGally)

##########################################################################

## Loading Data
Data = case0902

# Scatterplot Matrix
pairs(Data[, -1])
# Alternate Command for Scatterplot Matrix
ggpairs(Data[, -1])

# Log-Transformed Data
Data.log = Data
Data.log[,-1] = log(Data[,-1])

# Scatterplot Matrix
pairs(Data.log[, -1])

# Linear Model Fit
lin_mod <- lm(Brain ~ Gestation + Body + Litter, data = Data)
summary(lin_mod)

# Linear Model Fit with log-Transformed Data
lin_mod_log <- lm(Brain ~ Gestation + Body + Litter, data = Data.log)
summary(lin_mod_log)

