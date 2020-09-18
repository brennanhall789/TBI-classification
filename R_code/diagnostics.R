n <- nrow(dat4)
# ########
# #fit4
# ########
# df <- summary(fit4)$df.residual
# mu.hat <- predict(fit4, type = "response")
# eta.hat <- predict(fit4, type = "link")
# V.fun <- function(p,m) {p*(1-p)/m}
# V <- V.fun(mu.hat, dat4$count)
# r <- (dat4$prop - mu.hat)/sqrt(V)
# X <- sum(r^2)
# (phi <- X/(df))
# 
# par.res <- residuals(fit4, type="partial")
# 
# plot(fit4,which=4)
# 
# 
# glm.diag.plots(fit4)  #useless
# 
# avPlot(fit4,"sexM")

##########
# fit.base
##########
df <- summary(fit.base)$df.residual
mu.hat <- predict(fit.base, type = "response")
eta.hat <- predict(fit.base, type = "link")
V.fun <- function(p,m) {p*(1-p)/m}
V <- V.fun(mu.hat, dat4$count)
r <- (dat4$prop - mu.hat)/sqrt(V)
X <- sum(r^2)
phi.base <- X/(df)
phi.base

summary(fit.base)

xtable(fit.base)



######### 
#fit4.aic
#########

df <- summary(fit4.aic)$df.residual
mu.hat <- predict(fit4.aic, type = "response")
eta.hat <- predict(fit4.aic, type = "link")
V.fun <- function(p,m) {p*(1-p)/m}
V <- V.fun(mu.hat, dat4$count)
r <- (dat4$prop - mu.hat)/sqrt(V)
X <- sum(r^2)
phi4 <- X/(df)
phi4

xtable(fit4.aic)
anova(fit.base, fit4.aic, test="Chisq")


##########
#fit.null3
##########

df <- summary(fit.null3)$df.residual
mu.hat <- predict(fit.null3, type = "response")
eta.hat <- predict(fit.null3, type = "link")
V.fun <- function(p,m) {p*(1-p)/m}
V <- V.fun(mu.hat, dat4$count)
r <- (dat4$prop - mu.hat)/sqrt(V)
X <- sum(r^2)
phi.null3 <- X/(df)
phi.null3

xtable(fit.null3)

a1 <- anova(fit.null3, fit4.aic, test = "Chisq")
xtable(a1)


#####
# summary
#####
aic <- c(s.base$aic, s.null3$aic,s.aic4$aic)
phi <- c(phi.base, phi.null3, phi4)


models <- data.frame(AIC = aic, phi = phi)


texreg(fit.base, fit.null3, fit4.aic)

########
# fit4.aic part2
########

res4.dev <- residuals(fit4.aic,type="dev")
res4.pear <- residuals(fit4.aic,type="pearson")
dat4[abs(res4.dev)>2,]
dat4[abs(res4.pear)>2,]

cook4 <- cooks.distance(fit4.aic)
lev4 <- hatvalues(fit4.aic)
i<-order(-lev4)
dat4[i[1:3],]
cook4[13]
plot(fit4.aic,which=4)

## good normal qq plot indicates m-asymptotics converge properly
## can use this fact for other distribs such as chisquare for X^2 (pearson stat)
glm.diag.plots(fit4.aic)




# ##########
# #fit.pois
# ##########
# df <- summary(fit.pois)$df.residual
# mu.hat <- predict(fit.pois, type = "response")
# eta.hat <- predict(fit.pois, type = "link")
# V.fun <- function(u) {u}
# V <- V.fun(mu.hat)
# r <- (dat4$prop - mu.hat)/sqrt(V)
# X <- sum(r^2)
# (phi <- X/(df))
# 
# p <- mu.hat/dat4$count
# summary(p)
