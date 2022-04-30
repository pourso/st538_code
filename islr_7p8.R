# 7.8 non-linear modeling

library(ISLR2)
attach(Wage)

# 7.8.1 polynomial regression and step functions

# use orthogonal polynomials
fit = lm(wage ~ poly(age, 4), data = Wage)
coef(summary(fit))

# use raw polys
fit2 = lm(wage ~ poly(age, 4, raw = T), data = Wage)
coef(summary(fit2))

# alternative syntax
fit2a = lm(wage ~ age + I(age ^2) + I( age ^3) + I( age ^4), data = Wage )
coef( fit2a )

# alternative syntax
fit2b = lm(wage ~ cbind(age, age^2, age^3, age^4), data = Wage)

# get predictions
agelims = range(age)
age.grid = seq(from = agelims[1], to = agelims[2])
preds = predict(fit, newdata = list(age = age.grid), se=T)
se.bands = cbind(preds$fit + 2 * preds$se.fit, 
                 preds$fit - 2 * preds$se.fit)

# plot data and add fit
par ( mfrow = c(1, 2) , mar = c(4.5 , 4.5 , 1, 1) ,
      oma = c(0, 0, 4, 0))
plot (age , wage , xlim = agelims , cex = .5, col = " darkgrey ")
title (" Degree -4 Polynomial ", outer = T)
lines ( age.grid , preds$fit , lwd = 2, col = " blue ")
matlines ( age.grid , se.bands , lwd = 1, col = " blue ", lty = 3)

# how to determine degree of polynomial? hypothesis tests/ ANOVA

# fit 5 models and compare simpler model to more complex
fit.1 = lm(wage ~ age, data = Wage)
fit.2 = lm(wage ~ poly(age,2), data = Wage)
fit.3 = lm(wage ~ poly(age,3), data = Wage)
fit.4 = lm(wage ~ poly(age,4), data = Wage)
fit.5 = lm(wage ~ poly(age,5), data = Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

# instead of ANOVA, consider coeff of orthogonal polys
coef(summary(fit.5))

# ANOVA works regardless of orthogonal polys though
fit.1 - lm(wage ~ education + age, date = Wage)
fit.2 - lm(wage ~ education + poly(age, 2), date = Wage)
fit.3 - lm(wage ~ education + poly(age, 3), date = Wage)
anova(fit.1, fit.2, fit.3)


# logistic regression for wage > 250k
fit <- glm (I( wage > 250) ~ poly (age , 4) , data = Wage ,
            family = binomial )
preds = predict(fit, newdata = list(age = age.grid), se=T)

pfit = exp(preds$fit)/ (1 + exp(preds$fit))
se.bands.logit = cbind(preds$fit + 2 * preds$se.fit,
                       preds$fit - 2 * preds$se.fit)
se.bands = exp(se.bands.logit)/ (1 + exp(se.bands.logit))

plot (age , I( wage > 250) , xlim = agelims , type = "n",
      ylim = c(0, .2) )
points ( jitter ( age ), I(( wage > 250) / 5) , cex = .5, pch = "|",
           col = " darkgrey ")
lines ( age.grid , pfit , lwd = 2, col = " blue ")
matlines ( age.grid , se.bands , lwd = 1, col = " blue ", lty = 3)

# step function requires cut()
table(cut(age, 4))

fit = lm(wage ~ cut(age, 4), data=Wage)
coef(summary(fit))

# 7.8.2 Splines
library(splines)

# bs() generates matrix of basis functions w/ specified knots
# cubic splines by default
fit = lm(wage ~ bs(age, knots = c(25, 40, 60)), data=Wage)

pred = predict(fit, newdata = list(age=age.grid), se=T)

plot(age, wage, col='gray')
lines(age.grid, pred$fit, lwd=2)
lines(age.grid, pred$fit+ 2*pred$se, lty='dashed')
lines(age.grid, pred$fit- 2*pred$se, lty='dashed')

dim(bs(age,knots=c(25, 40, 60)))
dim(bs(age, df=6))
attr(bs(age, df=6), "knots")

# natural spline
fit2 = lm(wage ~ ns(age, df=4), data=Wage)
pred2 = predict(fit2, newdata = list(age=age.grid), se=T)
lines(age.grid, pred2$fit, col = "red", lwd=2)


# smoothing spline
# first fit specifies df
# second fit finds lamda by cross-val
plot(age, wage, xlim = agelims, cex=.5, col='darkgrey')
title("Smoothing spline")
fit = smooth.spline(age, wage, df=16)
fit2 = smooth.spline(age, wage, cv=T)
fit2$df

lines(fit, col = "red", lwd=2)
lines(fit2, col = "blue", lwd=2)
legend("topright", legend=c("16 DF", "6.8 DF"),
       col = c("red", "blue"), lty=1, lwd=2, cex=.8)


# GAMs

# predict wage using natural spline functions 
gam1 = lm(wage ~ ns(year, 4)+ ns(age, 5) + education,
          data=Wage)

# to use smoothing splines or other functions that
# can't be expressed in terms of basis functions
# and fit using least squares, need gam library

# s() function -> smoothing spline
library(gam)
gam.m3 = gam(wage ~ s(year, 4) + s(age, 5) +education,
             data=Wage)
par(mfrow=c(1,3))
plot(gam.m3, se=T, col="blue")
plot.Gam(gam1, se=T, col='red')

# ANOVA to tell if simpler model will work
gam.m1 = gam(wage~s(age,5)+education, data=Wage)
gam.m2 = gam(wage~year+s(age,5)+education, data=Wage)
anova(gam.m1, gam.m2, gam.m3, test="F")

summary(gam.m3)

# predictions on training set
preds = predict(gam.m2, newdata=Wage)

