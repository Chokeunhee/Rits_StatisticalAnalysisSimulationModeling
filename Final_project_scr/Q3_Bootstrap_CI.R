#Question3

library(boot)
library(extraDistr)
library(fitdistrplus)
datafile2 <- read.csv("~/Desktop/Stat_final/final/datafile2.csv", header = FALSE)
datafile2 <- data.frame(datafile2)
set.seed(0)
#lognormal
dist11 <- function(data, i){
  d <- data[i,]
  return(fitdist(d, "lnorm", method = "mle")$estimate[1])
}

dist12 <- function(data, i){
  d <- data[i,]
  return(fitdist(d, "lnorm", method = "mle")$estimate[2])
}
#weibull
dist21 <- function(data, i){
  d <- data[i,]
  return(fitdist(d, "weibull", method = "mle")$estimate[1])
}

dist22 <- function(data, i){
  d <- data[i,]
  return(fitdist(d, "weibull", method = "mle")$estimate[2])
}
#wald
dist31 <- function(data, i){
  d <- data[i,]
  return(fitdist(d, "wald", start=list(mu=1,lambda=1))$estimate[1])
}

dist32 <- function(data, i){
  d <- data[i,]
  return(fitdist(d, "wald", start=list(mu=1,lambda=1))$estimate[2])
}

bs11 <- boot(data = datafile2, dist11, R = 3000)
bs12 <- boot(data = datafile2, dist12, R = 3000)
bs21 <- boot(data = datafile2, dist21, R = 3000)
bs22 <- boot(data = datafile2, dist22, R = 3000)
bs31 <- boot(data = datafile2, dist31, R = 3000)
bs32 <- boot(data = datafile2, dist32, R = 3000)
bs11
bs12
bs21
bs22
bs31
bs32

# log mu
hist(bs11$t, main = "CI Bootstrap for mu", xlab="bootstrap sample",breaks = 100)
abline(v=bs11$t0,col="red",lty=1,lwd=2)
abline(v=c(2.36,2.47),col="blue",lty=2,lwd=1)
legend("topright",
       legend = c(expression(paste("confidence interval"))),col="blue",lty =2, lwd=1)
#log sig
hist(bs12$t, main = "CI Bootstrap for sigma", xlab="bootstrap sample",breaks = 100)
abline(v=bs12$t0,col="red",lty=1,lwd=2)
abline(v=c(1.086,1.158),col="blue",lty=2,lwd=1)
legend("topright",
       legend = c(expression(paste("confidence interval"))),col="blue",lty =2, lwd=1)
#wei c
hist(bs21$t, main = "CI Bootstrap for c", xlab="bootstrap sample",breaks = 100)
abline(v=bs21$t0,col="red",lty=1,lwd=2)
abline(v=c(0.8522,0.9307),col="blue",lty=2,lwd=1)
legend("topright",
       legend = c(expression(paste("confidence interval"))),col="blue",lty =2, lwd=1)
#wei b
hist(bs22$t, main = "CI Bootstrap for b", xlab="bootstrap sample",breaks = 100)
abline(v=bs22$t0,col="red",lty=1,lwd=2)
abline(v=c(18.52,20.76),col="blue",lty=2,lwd=1)
legend("topright",
       legend = c(expression(paste("confidence interval"))),col="blue",lty =2, lwd=1)
#wal eta
hist(bs31$t, main = "CI Bootstrap for eta", xlab="bootstrap sample",breaks = 100)
abline(v=bs31$t0,col="red",lty=1,lwd=2)
abline(v=c(19.36,22.39),col="blue",lty=2,lwd=1)
legend("topright",
       legend = c(expression(paste("confidence interval"))),col="blue",lty =2, lwd=1)
#wal rambda
hist(bs32$t, main = "CI Bootstrap for rambda", xlab="bootstrap sample",breaks = 100)
abline(v=bs32$t0,col="red",lty=1,lwd=2)
abline(v=c(7.461,9.087),col="blue",lty=2,lwd=1)
legend("topright",
       legend = c(expression(paste("confidence interval"))),col="blue",lty =2, lwd=1)


#CI-99%
boot.ci(boot.out = bs11, conf = 0.99, type = "basic")
boot.ci(boot.out = bs12, conf = 0.99, type = "basic")
boot.ci(boot.out = bs21, conf = 0.99, type = "basic")
boot.ci(boot.out = bs22, conf = 0.99, type = "basic")
boot.ci(boot.out = bs31, conf = 0.99, type = "basic")
boot.ci(boot.out = bs32, conf = 0.99, type = "basic")
