library(faraway)
library(ggplot2)

###############################################################

#### Transformation of Predictors

#### Broken Stick

head(savings)

# Fitting two different regression lines
# One for values of pop15 below cut
# Another for values of pop15 above cut
cut <- 35
X <- with(savings, cbind(as.numeric(pop15 < cut), as.numeric(pop15 >= cut), 
                         pop15*(pop15 < cut), pop15*(pop15 >= cut)))

# when fitting a linear model y ~ x - 1 specifies a line through the origin
# no intercept?
lmod <- lm(sr ~ X - 1, data = savings)
#lmod_b <- lm(sr ~ X + 0, data = savings)

# X1 and X3 correspond to first line
# X2 and X4 correspond to the second line
summary(lmod)

# Plotting the lines
plot(savings$pop15, savings$sr, xlab = "Popln under 15", ylab = "Savings Rate")
abline(v = 35, lty = 5)
segments(20, lmod$coef[1] + lmod$coef[3]*20, 35, lmod$coef[1] + lmod$coef[3]*35)
segments(48, lmod$coef[2] + lmod$coef[4]*48, 35, lmod$coef[2] + lmod$coef[4]*35)

# Fitting two different regression lines in another way
# One for values of pop15 below cut
# Another for values of pop15 above cut
lmod1 <- lm(sr ~ pop15, data = savings, subset = (pop15 < cut))
summary(lmod1)
lmod2 <- lm(sr ~ pop15, data = savings, subset = (pop15 > cut))
summary(lmod2)

# Plotting the lines
plot(savings$pop15, savings$sr, xlab = "Popln under 15", ylab = "Savings Rate")
abline(v = 35, lty = 5)
segments(20, lmod1$coef[1] + lmod1$coef[2]*20, 35, 
         lmod1$coef[1] + lmod1$coef[2]*35)
segments(48, lmod2$coef[1] + lmod2$coef[2]*48, 35, 
         lmod2$coef[1] + lmod2$coef[2]*35)

# Fitting two different regression lines 
# One for values of pop15 below cut
# Another for values of pop15 above cut
# The two lines are connected at the cut point giving a "broken stick"
lhs <- function(x) ifelse(x < cut, cut - x, 0)
rhs <- function(x) ifelse(x < cut, 0, x - cut)
lmod <- lm(sr ~ lhs(pop15) + rhs(pop15), savings)
summary(lmod)

# Plotting the lines
x <- seq(20, 48, by = 1)
py <- lmod$coef[1] + lmod$coef[2]*lhs(x) + lmod$coef[3]*rhs(x)
# png('broken_stick.png')
plot(savings$pop15, savings$sr, xlab = "Popln under 15", ylab = "Savings Rate")
abline(v = 35, lty = 3)
lines(x, py, lty = 2)
# dev.off()

#### Polynomial Regression

# I() -> "as is", suppress special formual syntax
lmod1 <- lm(sr ~ dpi , savings)
lmod2 <- lm(sr ~ dpi + I(dpi^2), savings)
lmod3 <- lm(sr ~ dpi + I(dpi^2) + I(dpi^3), savings)
lmod4 <- lm(sr ~ poly(dpi, 4, raw = T), savings) 

# Plotting the polynomial fits
plot(savings$dpi, savings$sr)
abline(lmod1, col=2)
new.data <- data.frame(dpi= seq(min(savings$dpi), max(savings$dpi), 10))
new.data$sr <- predict(lmod2, new.data)
lines(new.data$dpi, new.data$sr, col=3)
new.data$sr <- predict(lmod3, new.data)
lines(new.data$dpi, new.data$sr, col=4)
new.data$sr <- predict(lmod4, new.data)
lines(new.data$dpi, new.data$sr, col=6)
legend("topright", c("Linear", "Quadratic", "Cubic", "Quartic"), 
       col = c(1, 3, 4, 6), lty = rep(1, 4), cex = 0.5)

anova(lmod1, lmod2, lmod3, lmod4)


#### Splines Regression
library(splines)

# Natural Splines Regression
ns1 <- ns(savings$dpi, df = 5)
spline_mod <- lm(sr ~ ns(dpi, df = 5), data = savings)
summary(spline_mod)

# Position of knots in the spline
attr(terms(spline_mod), "predvars")

# Plotting the spline fit
plot(savings$dpi, savings$sr, xlab = "dpi", ylab = "Savings Rate")
new.data <- data.frame(dpi= seq(min(savings$dpi), max(savings$dpi), 10))
lines(new.data$dpi, predict(spline_mod, data.frame(dpi = new.data)))

# B-spline Regression
bs1 <- bs(savings$dpi, df = 5)
spline_mod_bs <- lm(sr ~ bs(dpi, df = 5, degree = 2), data = savings)
summary(spline_mod_bs)

# Position of knots in the spline
attr(terms(spline_mod_bs), "predvars")

# Plotting the spline fit
plot(savings$dpi, savings$sr, xlab = "dpi", ylab = "Savings Rate")
new.data <- data.frame(dpi= seq(min(savings$dpi), max(savings$dpi), 10))
lines(new.data$dpi, predict(spline_mod_bs, data.frame(dpi = new.data)))

#### GAM

library(mgcv)

gamod <- gam(sr ~ s(pop15) + s(pop75) + s(dpi) + s(ddpi), data=savings)

# Plotting the GAM fit for each of the predictors
plot(gamod, pages = 1)

###############################################################

#### Box-Cox Transformation

library(MASS)
lmod <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = savings)
boxcox(lmod, plotit = TRUE)
boxcox(lmod, plotit = TRUE, lambda = seq(0.5, 1.5, by = 0.1))
# lambda = 1 falls within the 95% confidence interval
# indcating no transformation is necessary for the response

lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + 
             Adjacent, data = gala)
boxcox(lmod, plotit = TRUE, lambda = seq(-0.25, 0.75, by = 0.05))
# lambda = 1/3 falls within the 95% confidence interval
# indcating a cube root transformation (y^(1/3)) for the response
# would make a better response for the linear regression model

