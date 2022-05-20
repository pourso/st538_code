library(mvtnorm)
library(ellipse)
library(psych)
library(glasso)

##################################################################################

# The function ellipsem() adds the ellipse t(x-mu)%*%amat%*%(x-mu) = c2 to the 
# current plot or a new plot; function ellipse() draws an ellipse given the 
# half-lengths of the axes, the angle of orientation, and the centre; angle() 
# computes the angle between the X-axis and a vector drawn from the origin to 
# the point (x, y).

ellipsem <- function (mu, amat, c2, npoints = 100, showcentre = T, ...) { 
  if(all(dim(amat) == c(2, 2))) { 
    eamat <- eigen(amat) 
    hlen <- sqrt(c2/eamat$val) 
    theta <- angle(eamat$vec[1, 1], eamat$vec[2, 1]) 
    ellipse(hlen[1], hlen[2], theta, mu[1], mu[2], npoints = npoints, ...) 
    if (showcentre) points(mu[1], mu[2], pch = 3) 
  } 
  invisible() 
}

ellipse <- function (hlaxa = 1, hlaxb = 1, theta = 0, xc = 0, yc = 0, 
                     newplot = T, npoints = 100, ...) { 
  a <- seq(0, 2 * pi, length = npoints + 1) 
  x <- hlaxa * cos(a) 
  y <- hlaxb * sin(a) 
  alpha <- angle(x, y) 
  rad <- sqrt(x^2 + y^2) 
  xp <- rad * cos(alpha + theta) + xc 
  yp <- rad * sin(alpha + theta) + yc 
  if (newplot) 
    plot(xp, yp, type = "l", ...) 
  else lines(xp, yp, ...) 
  invisible() 
}

angle <- function (x, y) { 
  angle2 <- function(xy) { 
    x <- xy[1] 
    y <- xy[2] 
    if (x > 0) { 
      atan(y/x) 
    } else { 
      if (x < 0 & y != 0) { 
        atan(y/x) + sign(y) * pi 
      } else { 
        if (x < 0 & y == 0) { 
          pi 
        } else { 
          if (y != 0) { 
            (sign(y) * pi)/2 
          } else { 
            NA 
          } 
        } 
      } 
    } 
  } 
  apply(cbind(x, y), 1, angle2) 
}

############ Principal Component Analysis #########################################

### Drawing the distance of points from lines in the scatter plot like 
### in slides 6-9 in lecture slides Week8_PCA1.pdf

x <- read.csv("class_scores.csv")
mean.x <- colMeans(x)
# png("pca_var1.png")
plot(x[,1], x[,2], xlab="X1", ylab="X2", xlim=c(20,100), ylim=c(20,100))
abline(v=mean.x[1])
for(i in 1:dim(x)[1]) {
  segments(x[i,1], x[i,2], mean.x[1], x[i,2], lty=2)
}
# dev.off()

# png("pca_var2.png")
plot(x[,1], x[,2], xlab="X1", ylab="X2", xlim=c(20,100), ylim=c(20,100))
abline(h=mean.x[2])
for(i in 1:dim(x)[1]) {
  segments(x[i,1], x[i,2], x[i,1], mean.x[2], lty=2, xlim=c(20,100), ylim=c(20,100))
}
# dev.off()

cov.x <- cov(x)
pc <- eigen(cov.x)
b <- pc$vectors[2,1]/pc$vectors[1,1]
a <- mean.x[2] - b*mean.x[1]
perp.segment.coord <- function(x0, y0, a=0,b=1){
  #finds endpoint for a perpendicular segment from the point (x0,y0) to the line
  # defined by lm.mod as y=a+b*x
  x1 <- (x0+b*y0-a*b)/(1+b^2)
  y1 <- a + b*x1
  return(cbind(x1, y1))
}
ss <- perp.segment.coord(x[,1], x[,2], a, b)
# png('pca_var3.png')
plot(x[,1], x[,2], xlab="X1", ylab="X2", xlim=c(20,100), ylim=c(20,100))
abline(a, b)
for(i in 1:dim(x)[1]) {
  segments(x[i,1], x[i,2], ss[i,1], ss[i,2], lty=2, xlim=c(0,100), ylim=c(0,100))
}
# dev.off()
b <- pc$vectors[2,2]/pc$vectors[1,2]
a <- mean.x[2] - b*mean.x[1]
ss <- perp.segment.coord(x[,1], x[,2], a, b)
# png('pca_var4.png')
plot(x[,1], x[,2], xlab="X1", ylab="X2", xlim=c(20,100), ylim=c(20,100))
abline(a, b)
for(i in 1:dim(x)[1]) {
  segments(x[i,1], x[i,2], ss[i,1], ss[i,2], lty=2, xlim=c(0,100), ylim=c(0,100))
}
# dev.off()

###############################################################

### Drawing the contour ellipse for multivariate normal density 
### and the population principal components

mu <- c(0, 0)
cov.x <- matrix(c(1, 0.5, 0.5, 1), nrow=2)
qchisq(0.95, 2)
pc <- eigen(cov.x)
b1 <- pc$vectors[2,1]/pc$vectors[1,1]
a1 <- mu[2] - b1*mu[1]
b2 <- pc$vectors[2,2]/pc$vectors[1,2]
a2 <- mu[2] - b2*mu[1]
#png("pca_var5.png")
ellipsem(mu, solve(cov.x), 6, xlim=c(-4,4), ylim=c(-4,4),
         xlab="X1", ylab="X2")
abline(a1, b1, lty=2, col=2)
abline(a2, b2, lty=2, col=2)
#dev.off()

###############################################################

### Principal Components for Banknote data
x <- read.csv("banknote.csv")

# png("banknote1,png")
pairs(x[, 1:6], col=(x$Y+1))  # Scatter plot pairs
# dev.off()

bn.pc <- prcomp(x[, 1:6])  # principal component function
plot(bn.pc)  # plot of eigenvalues
## biplot(bn.pc) 
summary(bn.pc)  # summary of variances of each principal directions
plot(bn.pc$sdev)  # standard deviations of each principal component
plot(cumsum(bn.pc$sdev^2)/sum(bn.pc$sdev^2)) # plot of cumulative variance of PCs  

bn.pc1 <- prcomp(x[, 1:6], scale. = T)  # PCA of Sample Correlation Matrix

bn.pc1$rotation  # Loadings of the PCs
plot(bn.pc1$rotation[,1])  # plot of loadings of First PC
abline(h=0)

plot(cumsum(bn.pc1$sdev^2)/sum(bn.pc1$sdev^2))  # plot of cumulative variance of PCs 

###############################################################

# Class Scores
x <- read.csv("class_scores.csv")

pc <- prcomp(x)  # Eigen decomposition of Sample Covariance
pc1 <- prcomp(x, scale. = T)  # Eigen decomposition of Sample Covariance

plot(pc1)  # plot of eigenvalues
#biplot(ov.pc)
summary(pc1)  # summary of variances of each principal directions
plot(pc1$sdev)  # standard deviations of each principal component
plot(cumsum(pc1$sdev)/sum(pc1$sdev))  # plot of cumulative variance of PCs

# Plot of Principal Component Directions
mean.x <- colMeans(x)
x <- t(t(x)-mean.x)
mean.x <- colMeans(x)
cv.x <- cov(x)
b1 <- pc$rotation[2,1]/pc$rotation[1,1]
a1 <- mean.x[2] - b1*mean.x[1]
b2 <- pc$rotation[2,2]/pc$rotation[1,2]
a2 <- mean.x[2] - b2*mean.x[1]
#png("pca_var6.png")
plot(x[,1], x[,2], xlab="X1", ylab="X2", xlim=c(-40,40), ylim=c(-40,40))
ellipsem(mean.x, solve(cv.x), 6, newplot=F)
abline(a1, b1, lty=2, col=2)
abline(a2, b2, lty=2, col=2)
#dev.off()


