library(parallel)            # base package for parallel computing

# Using lapply in place of for loop
lapply(1:3, function(x) c(x, x^2, x^3))

# Additional values can also be fed by adding named parameters
lapply(1:3/3, round, digits=3)

# Calculate the number of cores
no_cores <- detectCores() - 1   

# Initiate cluster
cl <- makeCluster(no_cores)

# parallel version of lapply is parLapply
parLapply(cl, 2:4, function(exponent) {2^exponent})

# You will get an error using following command
base <- 2 
parLapply(cl, 2:4, function(exponent) base^exponent) 

# Use following command
base <- 2 
clusterExport(cl, "base") 
parLapply(cl, 2:4, function(exponent) base^exponent)

# Stopping the parallel environment
stopCluster(cl)

#
# Parallel versions of lapply using Forking
#
# It relies on forking and hence is not available on Windows unless mc.cores = 1.

if (.Platform$OS.type == "windows") {
  cores = 1
} else {
  cores = no_cores
}

# parallel version of lapply using Forking is mclapply
mclapply(2:4, function(exponent) {2^exponent}, mc.cores = cores)

# mclapply with progress tracking bar is pbmclapply
library(pbmcapply)     # Track Progress of mclpply with progress bar
pbmclapply(2:4, function(exponent) {2^exponent}, mc.cores = cores)

# 
# foreach command instead of for loop
#
library(foreach) 
library(doParallel)   # for using registerDoParallel

# making clusters
cl <- makeCluster(no_cores) 
registerDoParallel(cl)   # creating implicit clusters

# different outputs
# vector
foreach(exponent = 2:4, .combine = c) %dopar% base^exponent
# matrix
foreach(exponent = 2:4, .combine = rbind) %dopar% base^exponent
# list
foreach(exponent = 2:4, .combine = list, .multicombine = TRUE) %dopar% base^exponent

base <- 2 
foreach(exponent = 2:4, .combine = c) %dopar% base^exponent 

# In Windows you will get an error
test <- function (exponent) { foreach(exponent = 2:4, .combine = c)
  %dopar% base^exponent } 
test()

# Use the following command instead
base <- 2 
test <- function (exponent) { foreach(exponent = 2:4, .combine = c,
                                      .export = "base") %dopar% base^exponent } 
test() 

# Stopping the parallel environment
stopCluster(cl)