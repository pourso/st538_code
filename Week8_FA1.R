library(psych)

#################### Data ######################################

# Car Data
#The data are averaged marks for 24 car types from a sample of 40 persons. 
# The marks range from 1 (very good) to 6 (very bad) like German school marks. 
# The variables are:
# Model, Economy, Service, Non-depreciation of value, Price - Mark 1 for very cheap cars,
# Design, Sporty car, Safety, Easy handling
x <- read.table("mod8/carmean2.txt", header =T)

# Education Data
?Harman
data(Harman)
x <- Harman.Holzinger

################################################################

x <- read.table("mod8/carmean2.txt", header =T)
cor.x <- cor(x[,2:9])

# Factor Analysis of the data using Maximum likelihood method
fa1 <- factanal(factors=1, covmat=cor.x, rotation="none")  # No. of factors: 1
fa2 <- factanal(factors=2, covmat=cor.x, rotation="none")  # No. of factors: 2

# Results of Factor Analysis
print(fa2, digits=2)
print(fa2, digits=2, cutoff=.3)  # cutoff: Truncate the loading value below 0.3 to 0

load <- fa2$loadings[,1:2]
plot(load,type="n") # set up plot
text(load,labels=names(x),cex=.7)  # Plot the factor loadings

?principal
?fa

# Factor analysis using principal components method
fa.pc <- principal(cor.x, nfactors=2)

# Factor analysis with Maximum likelihood mmethod; 
# factor loadings calculated using varimax criterion
# factor scores calculated using regression method 
fa.cm <- fa(cor.x, nfactors=2, rotate="varimax", scores="regression", fm="ml")

################################################################

?Harman
data(Harman)
x <- Harman.Holzinger

# Factor analysis with Maximum likelihood mmethod; 
# factor loadings calculated using varimax criterion
# factor scores calculated using regression method 
fa.h <- fa(x, nfactors=2, rotate="varimax", scores="regression", fm="ml")

