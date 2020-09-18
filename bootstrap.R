library(boot)
final.mod <- fit4.aic

bootstrap.fit <- function(data, indices) {
  
  d <- data[indices, ]
  fit <- update(final.mod, data=d)
  
  return(coef(fit))
}
set.seed(15)

logit.boot <- boot(data=dat4, statistic=bootstrap.fit, R=250)


par(mfrow=c(1,1))

## per beta
m2 <- mean(logit.boot$t[,2])
sd2 <- sqrt(var(logit.boot$t[,2],na.rm=T))
hist(logit.boot$t[,2],freq=F)
lines(seq(-.8, .8, by=.001), dnorm(seq(-.8, .8, by=.001), m2, sd2), col="blue")

m3 <- mean(logit.boot$t[,3],na.rm=T)
sd3 <- sqrt(var(logit.boot$t[,3],na.rm=T))
hist(logit.boot$t[,3],freq=F)
lines(seq(-.8, .8, by=.001), dnorm(seq(-.8, .8, by=.001), m3, sd3), col="blue")

m4 <- mean(logit.boot$t[,4],na.rm=T)
sd4 <- sqrt(var(logit.boot$t[,4],na.rm=T))
hist(logit.boot$t[,4],freq=F)
lines(seq(-.8, .8, by=.001), dnorm(seq(-.8, .8, by=.001), m4, sd4), col="blue")


m5 <- mean(logit.boot$t[,5],na.rm=T)
sd5 <- sqrt(var(logit.boot$t[,5],na.rm=T))
hist(logit.boot$t[,5],freq=F)
lines(seq(-.8, .8, by=.001), dnorm(seq(-.8, .8, by=.001), m5, sd5), col="blue")


m6 <- mean(logit.boot$t[,6],na.rm=T)
sd6 <- sqrt(var(logit.boot$t[,6],na.rm=T))
hist(logit.boot$t[,6],freq=F)
lines(seq(-.8, .8, by=.001), dnorm(seq(-.8, .8, by=.001), m6, sd6), col="blue")


m7 <- mean(logit.boot$t[,7],na.rm=T)
sd7 <- sqrt(var(logit.boot$t[,7]),na.rm=T)
hist(logit.boot$t[,7],freq=F)
lines(seq(-.8, .8, by=.001), dnorm(seq(-.8, .8, by=.001), m7, sd7), col="blue")


m3 <- mean(logit.boot$t[,3],na.rm=T)
sd3 <- sqrt(var(logit.boot$t[,3]),na.rm=T)
hist(logit.boot$t[,3],freq=F)

lines(seq(-.8, .8, by=.001), dnorm(seq(-.8, .8, by=.001), m3, sd3), col="blue")



m3 <- mean(logit.boot$t[,3],na.rm=T)
sd3 <- sqrt(var(logit.boot$t[,3]),na.rm=T)
hist(logit.boot$t[,3],freq=F)

lines(seq(-.8, .8, by=.001), dnorm(seq(-.8, .8, by=.001), m3, sd3), col="blue")



m3 <- mean(logit.boot$t[,3],na.rm=T)
sd3 <- sqrt(var(logit.boot$t[,3]),na.rm=T)
hist(logit.boot$t[,3],freq=F)

lines(seq(-.8, .8, by=.001), dnorm(seq(-.8, .8, by=.001), m3, sd3), col="blue")



m3 <- mean(logit.boot$t[,3],na.rm=T)
sd3 <- sqrt(var(logit.boot$t[,3]),na.rm=T)
hist(logit.boot$t[,3],freq=F)

lines(seq(-.8, .8, by=.001), dnorm(seq(-.8, .8, by=.001), m3, sd3), col="blue")



m3 <- mean(logit.boot$t[,3],na.rm=T)
sd3 <- sqrt(var(logit.boot$t[,3]),na.rm=T)
hist(logit.boot$t[,3],freq=F)

lines(seq(-.8, .8, by=.001), dnorm(seq(-.8, .8, by=.001), m3, sd3), col="blue")


R <- 500                     # number of bootstrap samples
n <- nrow(dat4)              # sample size
k <- length(coef(final.mod))     # number of coefficients

# set up a empty Rxn matrix B
B <- matrix(nrow = R, ncol = k,
           dimnames = list(paste("Sample",1:R), 
                           names(coef(final.mod))))
# loop R times
for(i in 1:R){
  boot.data <- dat4[sample(x = 1:n, size = n, replace = TRUE), ]
  boot.fit <- glm(final.mod$formula, 
                 data=boot.data, family = binomial, weights = count)
  
  B[i,] <- coef(boot.fit)
}

