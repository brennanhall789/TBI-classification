########
# Sampling from model
########
## Conclusion: all models have the same 95% ci inclusion rate and all p.hat that are
### outside CI are overestimates.

######
# fit4.aic
######
## For Y ~ Bin(m,p)
# get p estimate
p.hat <- predict(fit4.aic, type="response", se.fit = TRUE)
y.hat <- predict(fit4.aic, type="link", se.fit = TRUE)

#sample from Y.hat ~ Bin(m,p.hat)
set.seed(111)
y.hat.sample <- rbinom(n = 30, size = dat4$count, prob = p.hat$fit)
y.hat.sample
p.hat.sample <- y.hat.sample/dat4$count

# confidence interval for y.hat
alpha = 0.5
z = qnorm(1-alpha/2)

p.sample.lwr <- p.hat.sample - z*sqrt(p.hat.sample*(1-p.hat.sample)/dat4$count)
p.sample.upr <- p.hat.sample + z*sqrt(p.hat.sample*(1-p.hat.sample)/dat4$count)
p.hat$fit>= p.sample.lwr
p.test <- p.sample.lwr <= p.hat$fit & p.hat$fit <= p.sample.upr
CI <- data.frame(sample.p.hat = round(p.hat.sample,6), p.low = round(p.sample.lwr,6),
                  p.up = round(p.sample.upr,6), p = round(p.hat$fit,6),p.test = p.test)
CI
length(CI$p.test[which(p.test==TRUE)])

xtable(CI,digits=6)



#######
# fit.null3
#######
p.hat2 <- predict(fit.null3, type="response", se.fit = TRUE)
y.hat2 <- predict(fit.null3, type="link", se.fit = TRUE)

#sample from Y.hat ~ Bin(m,p.hat)
set.seed(111)
y.hat.sample2 <- rbinom(n = 30, size = dat4$count, prob = p.hat2$fit)
y.hat.sample2
p.hat.sample2 <- y.hat.sample2/dat4$count

# confidence interval for y.hat
alpha = 0.5
z = qnorm(1-alpha/2)

p.sample.lwr2 <- p.hat.sample2 - z*sqrt(p.hat.sample2*(1-p.hat.sample2)/dat4$count)
p.sample.upr2 <- p.hat.sample2 + z*sqrt(p.hat.sample2*(1-p.hat.sample2)/dat4$count)
p.hat2$fit>= p.sample.lwr2
p.test2 <- p.sample.lwr2 <= p.hat2$fit & p.hat2$fit <= p.sample.upr2
CI2 <- data.frame(sample.p.hat = round(p.hat.sample2,6), p.low = round(p.sample.lwr2,6),
                  p.up = round(p.sample.upr2,6), p = round(p.hat2$fit,6),p.test = p.test2)
CI2
length(CI2$p.test[which(p.test==TRUE)])



#######
# fit.base
#######
p.hat3 <- predict(fit.base, type="response", se.fit = TRUE)
y.hat3 <- predict(fit.base, type="link", se.fit = TRUE)

#sample from Y.hat ~ Bin(m,p.hat)
set.seed(111)
y.hat.sample3 <- rbinom(n = 30, size = dat4$count, prob = p.hat3$fit)
y.hat.sample3
p.hat.sample3 <- y.hat.sample3/dat4$count

# confidence interval for y.hat
alpha = 0.5
z = qnorm(1-alpha/2)

p.sample.lwr3 <- p.hat.sample3 - z*sqrt(p.hat.sample3*(1-p.hat.sample3)/dat4$count)
p.sample.upr3 <- p.hat.sample3 + z*sqrt(p.hat.sample3*(1-p.hat.sample3)/dat4$count)
p.hat3$fit>= p.sample.lwr3
p.test3 <- p.sample.lwr3 <= p.hat3$fit & p.hat3$fit <= p.sample.upr3
CI3 <- data.frame(sample.p.hat = round(p.hat.sample3,6), p.low = round(p.sample.lwr3,6),
                  p.up = round(p.sample.upr3,6), p = round(p.hat3$fit,6),p.test = p.test3)
CI3
length(CI3$p.test[which(p.test==TRUE)])


