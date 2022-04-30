################### Simulated Data ############################################
# two datasets:
# 1. rand data from normal dist w/ same mean, std
# 2. rand data from normal dist, half w/ mean 0, half w/ mean 0.25


# Data where all null hypotheses are true
# So No null hypotheses should be rejected
p <- 50
n <- 100
x <- matrix(0, nrow = n, ncol = p)
mu0 <- 0
s0 <- 1
for(j in 1:p) {
  x[,j] <- rnorm(n, mu0, s0)
}

# Data where some of null hypotheses are true
# (p/2)^2 = 625 null hypotheses are false for this data among 
# p*(p-1)/2 = 1225 hypotheses
p <- 50
n <- 100
x1 <- matrix(0, nrow = n, ncol = p)
mu0 <- c(rep(0.25, p/2), rep(0, p/2))
s0 <- 1
for(j in 1:p) {
  x1[,j] <- rnorm(n, mu0[j], s0)
}

# False null hypothesis tests or true alternative hypotheses
tr <- rep(0, p*(p-1)/2)
l <- 0
for(j in 1:(p-1)) {
  for(k in (j+1):p) {
    l <- l + 1
    if((j <= p/2) & (k > p/2)) {
      tr[l] <- 1
    } 
  }
}

################### Individual Tests ############################################

al <- 0.05  # Level of test

# p-values of the p(p-1)/2 = 1225 hypothesis tests
tt <- NULL
tt1 <- NULL
l <- 0
for(j in 1:(p-1)) {
  for(k in (j+1):p) {
    l <- l + 1
    tt[l] <- t.test(x[, j], x[, k])$p.value
    tt1[l] <- t.test(x1[, j], x1[, k])$p.value
  }
}

# ~0.067 of t-tests on "like mean" dataset falsely reject H0

# Histogram of the 1225 hypothesis tests for the first dataset
hist(tt, breaks=50)
abline(v=al, col=2, lwd=2)
rej_t <- (tt <= al)   # which tests are being rejected
sum(rej_t)   # Number of rejected tests
# Note that number of rejected tests should be close to or less than al*p*(p-1)/2

# ~0.23 of t-tets on "diff mean" dataset rejected, but half differ in mean?

# Histogram of the 1225 hypothesis tests for the second dataset
hist(tt1, breaks=50)
abline(v=al, col=2, lwd=2)
rej_t <- (tt1 <= al)   # which tests are being rejected
sum(rej_t)   # Number of rejected tests
# Note that number of rejected tests is close to or less than (p/2)^2

# how many times did we reject H0 for "like mean" data?
# ~0.057 FDR

tr_discov <- tr*rej_t
sum(tr_discov) # Number of true rejections (should be diff, found to be diff)
false_discov <- (1 - tr)*rej_t
sum(false_discov) # Number of false rejections 
fdr <- sum(false_discov)/sum(rej_t)  # False discovery rate
fdr
# Note the large number of false discoveries

################### Bonferroni cutoff ############################################

m <- (p*(p-1)/2)   # Number of tests

# Bonferroni uses very small individual alpha

hist(tt, breaks=50)
abline(v=al/m, col=2, lwd=2)
# which tests are being rejected with Bonferroni cut-off
rej_bon <- (tt <= al/m)  
sum(rej_bon)   # Number of rejected tests with Bonferroni cut-off
# Note that number of rejected tests should be quite small

# only one test from "diff mean" data is rejected

hist(tt1, breaks=50)
abline(v=al/m, col=2, lwd=2)
# which tests are being rejected with Bonferroni cut-off
rej_bon <- (tt1 <= al/m)  
sum(rej_bon)   # Number of rejected tests with Bonferroni cut-off
# Note that number of rejected tests is much less than true number (p/2)^2 = 625

# no false discoveries 

tr_discov <- tr*rej_bon
sum(tr_discov) # Number of true rejections 
false_discov <- (1 - tr)*rej_bon
sum(false_discov) # Number of false rejections 
fdr <- sum(false_discov)/sum(rej_bon)  # False discovery rate
fdr

################### Sidak cutoff ############################################

# Sidak and Bonferroni nearly agree

a_sid <- 1 - (1 - al)^(1/m)
# Comparison between Sidak and Bonferroni cut-off
a_sid
al/m

hist(tt, breaks=50)
abline(v=a_sid, col=2, lwd=2)
# which tests are being rejected with Sidak cut-off
rej_sid <- (tt <= a_sid)  
sum(rej_sid)  # Number of rejected tests with Sidak cut-off
# Note that number of rejected tests should be quite small

hist(tt1, breaks=50)
abline(v=a_sid, col=2, lwd=2)
# which tests are being rejected with Sidak cut-off
rej_sid <- (tt1 <= a_sid)  
sum(rej_sid)  # Number of rejected tests with Sidak cut-off
# Note that number of rejected tests is much less than true number (p/2)^2 = 625

################### Tukey's HSD cutoff ############################################

# Tukey's cutoff shows marginal improvement in true discoveries

data1 <- data.frame(res = as.vector(x), 
                    gr = as.factor(kronecker(1:50, rep(1, 100))))
aov1 <- aov(res ~ gr, data = data1)
Tukey1 <- TukeyHSD(aov1)
# which tests are being rejected with Tukey's HSD cut-off
rej_THSD <- (Tukey1$gr[, 4] <= al)  
sum(rej_THSD)   # Number of rejected tests with Tukey's HSD cut-off
# Note that number of rejected tests should be quite small

data2 <- data.frame(res = as.vector(x1), 
                    gr = as.factor(kronecker(1:50, rep(1, 100))))
aov2 <- aov(res ~ gr, data = data2)
Tukey2 <- TukeyHSD(aov2)
# which tests are being rejected with Tukey's HSD cut-off
rej_THSD <- (Tukey2$gr[, 4] <= al)  
sum(rej_THSD)   # Number of rejected tests with Tukey's HSD cut-off
# Note that number of rejected tests is much less than true number (p/2)^2 = 625
# But slightly higher than Bonferroni cut-off

################### Holm's method ############################################

p.adj.h <- p.adjust(tt, "holm")
# which tests are being rejected with Holm's method
rej_h <- (p.adj.h < al)  
sum(rej_h)  # Number of rejected tests with Holm's method
# Note that number of rejected tests should be quite small

p.adj.h1 <- p.adjust(tt1, "holm")
# which tests are being rejected with Holm's method
rej_h <- (p.adj.h1 < al)  
sum(rej_h)  # Number of rejected tests with Holm's method
# Note that number of rejected tests is much less than true number (p/2)^2 = 625
# But slightly higher than Bonferroni cut-off

################### Benjamini-Hochberg Procedure #################################

# more true rejections, but still nowhere near "diff mean" pop
# FDR is ~0.02, much better than w/ no control

q <- 0.05  # False discovery rate control
p.adj.bh <- p.adjust(tt, "BH")
# which tests are being rejected with Benjamini-Hochberg procedure
rej_bh <- (p.adj.bh < q)  
sum(rej_bh)  # Number of rejected tests with Benjamini-Hochberg procedure
# Note that number of rejected tests should be quite small

p.adj.bh1 <- p.adjust(tt1, "BH")
# which tests are being rejected with Benjamini-Hochberg procedure
rej_bh <- (p.adj.bh1 < q)  
sum(rej_bh)   # Number of rejected tests with Benjamini-Hochberg procedure
# Note that number of rejected tests is higher than previous procedures

tr_discov <- tr*rej_bh
sum(tr_discov) # Number of true rejections with Benjamini-Hochberg procedure
false_discov <- (1 - tr)*rej_bh
sum(false_discov) # Number of false rejections with Benjamini-Hochberg procedure
fdr <- sum(false_discov)/sum(rej_bh)  # False discovery rate
fdr
# Also note that the number of false discoveries are smaller
# And the false discovery rate is controlled
