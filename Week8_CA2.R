library(mvtnorm)
library(cluster)
library(plotrix)
library(graphics)
library(MASS)

############## Data ##################################################

# Generating data with two clusters and two variables
x <- matrix(0, nrow=50, ncol=2)
x[1:25,] <- rmvnorm(25, c(0,0))
x[26:50,] <- rmvnorm(25, c(4,4))

# Cancer Data
cancerData <- read.table(file="mod8/14cancer.xtrain",sep="")
# namesCancer<-read.table(file="namesCancer.txt",sep="",header="FALSE")
namesCancer <- read.table(file="mod8/namesCancer.txt")
myLabels <- c('breast','prostate','lung','collorectal','lymphoma','bladder','melanoma','uterus','leukemia','renal','pancreas','ovary','meso','cns')
names(cancerData) <- c(namesCancer)
# dim(cancerData)
myLabels <- c('breast','prostate','lung','collorectal','lymphoma','bladder','melanoma','uterus','leukemia','renal','pancreas','ovary','meso','cns')

# Handwritten Digit Data
digit = read.table('mod8/Digit_recog_train.txt')
digit_no = digit[,1]
digit_pix = digit[,-1]
digit_no3 = digit_no[digit_no == 3]
digit_pix3 = digit_pix[digit_no == 3,]

sel = sample(c(0,1), length(digit_no), replace = T, prob = c(4/5,1/5))
digit_pix = digit_pix[sel == 1,]
digit_no = digit_no[sel==1]

######################### Hierarchical Clustering #####################################

hc <- hclust(dist(x), method="complete")
# hc <- hclust(dist(x), method="single")
# hc <- hclust(dist(x), method="average")
# hc <- hclust(dist(x), method="centroid")
plot(hc)
cl <- cutree(hc, k=2)
plot(x[,1], x[,2], col=cl)

######

distancesCancerData <- dist(t(cancerData))

hclustCancer <- hclust(distancesCancerData, method = 'single')
# hclustCancer = hclust(distancesCancerData, method = 'complete')
# hclustCancer = hclust(distancesCancerData, method = 'average')
# hclustCancer = hclust(distancesCancerData, method = 'centroid')
plot(hclustCancer)
hclustCancerClusters = cutree(hclustCancer, k = 5)
table(factor(hclustCancerClusters), factor(unlist(namesCancer)))

######

distancesDigit <- dist(digit_pix)

hclustDigit = hclust(distancesDigit, method = 'single')
# hclustDigit = hclust(distancesDigit, method = 'complete')
# hclustDigit = hclust(distancesDigit, method = 'average')
# hclustDigit = hclust(distancesDigit, method = 'centroid')
plot(hclustDigit)
hclustDigitClusters = cutree(hclustDigit, k = 10)
table(factor(hclustDigitClusters), factor(digit_no))

