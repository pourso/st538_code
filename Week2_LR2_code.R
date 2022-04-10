library(Sleuth3)
library(ggplot2)
library(Rmisc)
library(graphics)

##########################################################################

## Code drawing qqplot and qqline together in ggplot2, you do not need to understand this code

if(! (packageVersion("ggplot2") >= "2.0.0")) {
  stop("This version of stat_qqline require ggplot2 version 2.0.0 or greater")
}

StatQQLine <- ggproto("StatQQLine", Stat,
                      compute_group = function(data, scales, distribution = qnorm, dparams = list()) {
                        data <- remove_missing(data, na.rm = TRUE, "sample", name = "stat_qqline")
                        y <- quantile(data$sample, c(0.25, 0.75))
                        x <- do.call(distribution, c(list(p = c(0.25, 0.75)), dparams))
                        slope <- diff(y)/diff(x)
                        int <- y[1L] - slope * x[1L]
                        data.frame(slope = slope, intercept = int)
                      },
                      
                      required_aes = c("sample")
)

stat_qqline <- function(mapping = NULL, data = NULL, geom = "abline",
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        distribution = qnorm, dparams = list(),
                        inherit.aes = TRUE, ...) {
  layer(
    stat = StatQQLine, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, distribution = distribution, dparams = dparams, ...)
  )
}

## Loading Data
Data = case1101

## Plot Sex vs. Metabol and Sex vs. Gastric values
# png('plot41.png')  # Use this function to store the output of plot
p1 <- qplot(Sex, Metabol, data=Data)
p2 <- qplot(Sex, Gastric, data=Data)
multiplot(p1, p2, cols=2)
# dev.off() # Use this function to store the output of plot

## Fitting the model with 3-way interactions
Data$Sex = relevel(factor(Data$Sex),2)
Data$Alcohol = relevel(factor(Data$Alcohol), 2)
lin_mod = lm(Metabol~Gastric+Sex+Alcohol+Gastric*Sex*Alcohol, data = Data)
summary(lin_mod)

## Fitting the model without observations #31 and #32
lin_mod1 = lm(Metabol~Gastric+Sex+Alcohol+Gastric*Sex*Alcohol, data = Data[-c(31,32),])
summary(lin_mod1)



## Case Influence Statistics

## Attaching the Case Influence Statistics with the Data
Data <- fortify(lin_mod, Data)

## Plot the Case Influence Statistics for each observation (subject)
# png('plot42.png')  # Use this function to store the output of plot
p1 <- qplot(Subject,.hat, data = Data)
p2 <- qplot(Subject,.stdresid, data = Data)
p3 <- qplot(Subject,.cooksd, data = Data)
multiplot(p1,p2,p3,cols=1)
# dev.off()  # Use this function to store the output of plot

## Simplified model

## Male and Female observations
new.obs1 <- data.frame(Gastric=Data$Gastric,Sex=rep('Male', length(Data$Sex)))
new.obs2 <- data.frame(Gastric=Data$Gastric,Sex=rep('Female', length(Data$Sex)))

## Model with #31 and #32
# dropping alcoholic indicator
lin_mod2 <- lm(Metabol~Gastric+Sex*Gastric, data=Data)
Data <- fortify(lin_mod2, Data)
# why are genders faked?
Data$fitted1 <- predict(lin_mod2, new.obs1)
Data$fitted2 <- predict(lin_mod2, new.obs2)

## Model Without sample #31 and #32
Data.sel <- Data[-c(31, 32), ]
lin_mod3 <- lm(Metabol~Gastric+Sex*Gastric, data=Data.sel)
Data.sel <- fortify(lin_mod3, Data.sel)
Data.sel$fitted3 <- predict(lin_mod3, new.obs1[-c(31, 32), ])
Data.sel$fitted4 <- predict(lin_mod3, new.obs2[-c(31, 32), ])

## Plot Scatterplot and Fitted lines with and without observations #31 and #32
# png('plot43.png')
qplot(Gastric, Metabol, data=Data.sel, col=Sex) + 
  geom_line(aes(Gastric, fitted1), col=2, linetype=1,show.legend=T) + 
  geom_line(aes(Gastric, fitted2),col=3, linetype=1,show.legend=T) + 
  geom_line(aes(Gastric, fitted3), col=2, linetype=2, data=Data.sel, show.legend=T) + 
  geom_line(aes(Gastric, fitted4), col=2, linetype=2, data=Data.sel, show.legend=T)
# dev.off()

## Male and Female observations
new.obs1 <- data.frame(Gastric=Data$Gastric,Sex=rep('Male', length(Data$Sex)))
new.obs2 <- data.frame(Gastric=Data$Gastric,Sex=rep('Female', length(Data$Sex)))

## Model with #1, #23, #31 and #32
lin_mod2 <- lm(Metabol~Gastric+Sex*Gastric, data=Data)
Data <- fortify(lin_mod2, Data)
Data$fitted1 <- predict(lin_mod2, new.obs1)
Data$fitted2 <- predict(lin_mod2, new.obs2)

## Model Without #1, #23, #31 and #32
Data.sel1 <- Data[-c(1, 23, 31, 32), ]
lin_mod3 <- lm(Metabol~Gastric+Sex*Gastric, data=Data.sel1)
Data.sel1 <- fortify(lin_mod3, Data.sel1)
Data.sel1$fitted3 <- predict(lin_mod3, new.obs1[-c(1, 23, 31, 32), ])
Data.sel1$fitted4 <- predict(lin_mod3, new.obs2[-c(1, 23, 31, 32), ])

## Plot Scatterplot and Fitted lines with and without observations #31 and #32
# png('plot44.png')
qplot(Gastric, Metabol, data=Data.sel1, col=Sex) + geom_line(aes(Gastric, fitted1), col=2, linetype=1,show.legend=T) + geom_line(aes(Gastric, fitted2),col=3, linetype=1,show.legend=T) + geom_line(aes(Gastric, fitted3), col=2, linetype=2, show.legend=T) + geom_line(aes(Gastric, fitted4), col=3, linetype=2, show.legend=T)
# dev.off()

#### Comparing Models

## Full Model
lin_mod1 = lm(Metabol~Gastric+Sex+Alcohol+Gastric*Sex*Alcohol, data = Data.sel)
summary(lin_mod1)

## Reduced Model 
lin_mod_red = lm(Metabol~Gastric+Sex+Gastric*Sex, data = Data.sel)
summary(lin_mod_red)

## More Reduced Model
lin_mod_red1 = lm(Metabol~Gastric+Gastric:Sex, data = Data.sel)
summary(lin_mod_red1)

anova(lin_mod_red, lin_mod1)
anova(lin_mod_red1, lin_mod_red)

#### Residual Plots

## Residual vs Fitted
lin_mod = lm(Metabol~Gastric+Sex+Alcohol+Gastric*Sex*Alcohol, data = Data)
Data = fortify(lin_mod, Data)
#png('plot45.png')
qplot(.fitted, .resid, data=Data) # constant variance violated? variance appears to increase w/ fitted value...
#dev.off()

## Residual VS Explanatory
#png('plot46.png')
qplot(Gastric, .resid, data = Data) # constant variance violated? variance appears to increase w/ predictor...
#dev.off()
#png('plot47.png')
p1 <- qplot(Sex, .resid, data=Data)
p2 <- qplot(Alcohol, .resid, data=Data)
multiplot(p1, p2, cols=2)
#dev.off()

## QQ-Plot
#png('plot48.png')
qplot(sample = Data$.resid) + stat_qqline() # slight deviation from normality at ends
#dev.off()

## Residual vs Fitted
# removing observations improves constant variance
lin_mod2 = lm(Metabol~Gastric+Sex+Alcohol+Gastric*Sex*Alcohol, data = Data.sel1)
Data2 = fortify(lin_mod2, Data.sel1)
#png('plot49.png')
qplot(.fitted, .resid, data=Data.sel1)
#dev.off()

## Residual VS Explanatory
# png('plot410.png')
qplot(Gastric, .resid, data = Data.sel1)
# dev.off()
# png('plot411.png')
p1 <- qplot(Sex, .resid, data=Data.sel1)
p2 <- qplot(Alcohol, .resid, data=Data.sel1)
multiplot(p1, p2, cols=2)
# dev.off()

## QQ-Plot
png('plot413.png')
qplot(sample = Data.sel1$.resid) + stat_qqline() 
dev.off()

