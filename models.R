modsex <- glm(prop ~ sex, data = dat4, family = binomial, weights = count)
modyear <- glm(prop ~ year, data = dat4, family = binomial, weights = count)
modsport <- glm(prop ~ sport, data = dat4, family = binomial, weights = count)
summary(modsex)
summary(modyear)  
summary(modsport) 
modsex$coefficients
modsport$coefficients
modyear$coefficients
#### some coefs for sport are close compared to fit4, coefs for year
##### are 1/10th smaller, coef for sexM is very close

fit1 <- glm(tbi ~ sex+sport+year+sex:sport, family=binomial, data=dat1)
fit <- glm(prop ~ sex*sport + year + sex:year, data = dat4, family = binomial, weights = count)
summary(fit)
fit.log <- glm(prop ~ sex+sport + sex:sport, data = dat4, family = binomial(link="log"), weights = count)
summary(fit.log)
fit.pois <- glm(tbiCount ~ offset(log(count)) + sex+sport+year+sex:sport, data = dat4, family = poisson)
summary(fit.pois) 
fit.loglog <- glm(prop ~ sex+sport +year+ sex:sport, data = dat4, family = binomial(link="log"), weights = count)
summary(fit.loglog)
#### we conclude with using logit link because we are interested a proportion/odds ratio result
##### and the other links are indistinguishable for small sample sizes.

fit.base <- glm(prop ~ year+sport+sex, data = dat4, family = binomial, weights = count)
fit.null <- glm(prop ~ 1, data = dat4, family = binomial, weights = count)
fit.full <- glm(prop ~ year*sport*sex, data = dat4, family = binomial, weights = count)

fit4.aic <- stepAIC(fit.base,
                scope = list(upper=prop ~ year*sex*sport, lower=prop ~ year+sex+sport),
                direction="both")
fit4.aic <- glm(prop ~ year+sport + sex + sport:sex, data = dat4, family = binomial, weights = count)

fit4.aic.y <- stepAIC(fit.null,
                    scope = list(upper=prop ~ year*sport*sex, lower=prop ~ 1),
                    direction="both")
summary(fit4.aic)

add1(fit.null, prop~year*sport*sex, test = "Chisq")
fit.null1 <- update(fit.null, ~.+sex)
add1(fit.null1, prop~year*sport*sex, test="Chisq")
fit.null2 <- update(fit.null1, ~.+sport)
add1(fit.null2, prop~year*sport*sex, test="Chisq")
fit.null3 <- update(fit.null2, ~.+sex:sport)
add1(fit.null3, prop~year*sport*sex, test="Chisq")

summary(fit.null3)  # fit.null3 is same as fit.aic IF no lower limit on vars

anova(fit.null3, fit.null2, test="Chisq")   # null2 different from null3 => cannot reduce to null2
anova(fit.null3, fit4, test="Chisq")  # null.3 not different than fit4 and also has lower aic

## check differences in estimate's std.error b/w null.3 and fit4
s.base <- summary(fit.base)
s.null2 <- summary(fit.null2)
s.null3 <- summary(fit.null3)
s.aic4 <-  summary(fit4.aic)

round(s.null3$coefficients[,2] - s.aic.y$coefficients[c(1:6,8:11),2],4) 
##### fit.null3 has lower SE for all estimates except male:gymn


## how do we lower se of male:gymn? 
### does not seem like we can. likely due to no cases of tbi=1 for male:gymn


###
# test "relative risk (of tbi)" with log link
###


############ 
# if we end with 2 models, compare each estimated proportions 
# via chisquare (prop.test) test
############
############
# can also compare prediction error by resampling from model and testing 
# efficiency of replicating data
############