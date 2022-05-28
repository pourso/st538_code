library(mvtnorm)
library(cluster)
library(plotrix)
library(graphics)
library(mclust)
library(MASS)

############## Data ##################################################

# Generating data with four clusters and two variables
x <- matrix(0, nrow=100, ncol=2)
x[1:25,] <- rmvnorm(25, c(2,2))
x[26:50,] <- rmvnorm(25, c(-2,2))
x[51:75,] <- rmvnorm(25, c(-2,-2))
x[76:100,] <- rmvnorm(25, c(2,-2))
# True cluster assignments
cl.true <- c(rep(1, 25), rep(2, 25), rep(3, 25), rep(4, 25))  

# Cancer Data
cancerData <- read.table(file="mod8/14cancer.xtrain",sep="")
# namesCancer<-read.table(file="namesCancer.txt",sep="",header="FALSE")
namesCancer <- read.table(file="mode8/namesCancer.txt")
myLabels <- c('breast','prostate','lung','collorectal','lymphoma',
              'bladder','melanoma','uterus','leukemia','renal','pancreas',
              'ovary','meso','cns')
names(cancerData) <- c(namesCancer)
# dim(cancerData)
myLabels <- c('breast','prostate','lung','collorectal','lymphoma',
              'bladder','melanoma','uterus','leukemia','renal','pancreas',
              'ovary','meso','cns')

# Handwritten Digit Data
digit = read.table('mod8/Digit_recog_train.txt')
digit_no = digit[,1]
digit_pix = digit[,-1]
digit_no3 = digit_no[digit_no == 3]
digit_pix3 = digit_pix[digit_no == 3,]

sel = sample(c(0,1), length(digit_no), replace = T, prob = c(4/5,1/5))
digit_pix = digit_pix[sel == 1,]
digit_no = digit_no[sel==1]

################# Model Based Clustering ####################################

# x <- t(cancerData)
# x <- digit_pix

###### Gaussian mixture model clustering

# GMM clustering with equal covariance structure for different clusters
mod1 <- Mclust(x, 2:5, modelNames="EEE")  # "EEE": ellipsoidal, equal volume, shape, and orientation
mod1$BIC  # Finding Best Model and number of clusters
mod1$z    # Soft clustering
clm <- pam(mod1$z, 4) # Hard clustering with 4 clusters
clm$clustering  # cluster membership of each point
# Misclassification table for true and estimated clusters
table(clm$clustering, cl.true) 
# Estimated Gaussian parameters for each cluster
mod1$parameters$mean
mod1$parameters$variance
mod1$parameters$pro

# GMM clustering with different covariance structure for different clusters
mod2 <- Mclust(x, 2:5, modelNames="VVV") # "VVV": ellipsoidal, varying volume, shape, and orientation
mod2$BIC # Finding Best Model and number of clusters
mod2$z   # Soft clustering
clm <- pam(mod2$z, 2)   # Hard clustering with 2 clusters
clm$clustering  # cluster membership of each point
# Misclassification table for true and estimated clusters
table(clm$clustering, cl.true) 
# Estimated Gaussian parameters for each cluster
mod2$parameters$mean
mod2$parameters$variance
mod2$parameters$pro

# hard clustering w/ 4
clm <- pam(mod2$z, 4)   # Hard clustering with 4 clusters
table(clm$clustering, cl.true) 



##### Gaussian mixture density plot
s1 <- diag(rep(1,2))
s2 <- diag(rep(1,2))
s3 <- diag(rep(1,2))
s4 <- diag(rep(1,2))
dbvnorm <- function(x1, x2, mu1=c(2,2), mu2=c(-2,2), mu3=c(-2,-2), mu4=c(2,-2),
                    s1=diag(rep(1,2)), s2=diag(rep(1,2)), 
                    s3=diag(rep(1,2)), s4=diag(rep(1,2)), pr=rep(0.25, 4)) {
  rslt <- pr[1]*dmvnorm(cbind(x1, x2), mean=mu1, sigma=s1) + 
    pr[2]*dmvnorm(cbind(x1, x2), mean=mu2, sigma=s2) +
    pr[3]*dmvnorm(cbind(x1, x2), mean=mu3, sigma=s3) +
    pr[4]*dmvnorm(cbind(x1, x2), mean=mu4, sigma=s4)
}

x1 <- seq(-5, 5, 0.25) # grid
x2 <- seq(-5, 5, 0.25) # grid

z <- outer(x1, x2, dbvnorm)
persp(x1, x2, z, main = "Mixture Bivariate Normal Density", zlab = "", 
      theta=-10, phi=30, r=100, d=0.1, expand=0.5, 
      ltheta=90, lphi=180, shade=0.75, ticktype="detailed",
      nticks=5)
# mtext(expression(list(mu[1]==0,mu[2]==0,sigma[11]==10,sigma[22]==10,rho==0.5)))


