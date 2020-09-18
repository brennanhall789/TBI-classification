############
#Reformatting dataset to have prop = proportion of tbi per grouping, count=total trials=m_i
############
dat1 <-read.table("collegiate.txt", col.names = c("sex","sport","year","tbi","count"))
dat4 <- read.table("collegiate4.txt", col.names = c("sex","sport","year","tbiY","tbiCount","prop","count"))
dat4$year <- as.factor(dat4$year)
summary(dat4)

#############
# plotting interaction
#############
par(mfrow=c(1,3))

plot(dat4$sex, dat4$prop, ylab="Proportion", xlab="Gender")  
plot(dat4$sport, dat4$prop, xlab="Sport",cex.axis=.8)
plot(dat4$year, dat4$prop,xlab="Year")  
title(outer=T,line=-2,
      "Boxplots of proportion of TBI",cex.main=2.5) 

prop.fem <- dat4[which(dat4$sex=="F"),]
prop.male <- dat4[which(dat4$sex=="M"),]
par(mfrow=c(1,2))
# sex vs sport
plot(prop.fem$prop ~ prop.fem$sport,xlab="Sport", ylab="Proportion",cex.axis=.7,ylim=c(0,.0022))
title("Proportion of TBI among Female",line=.2,cex.main=1)
plot(prop.male$prop ~ prop.male$sport,xlab="Sport", ylab="Proportion",cex.axis=.7,ylim=c(0,.0022))
title("Proportion of TBI among Male",line=.2,cex.main=1)
par(adj=0.5)
title(outer=T,line=-2,
      "Proportion of TBI across Sport per Gender",cex.main=1.8) 

# sex vs year
plot(prop.fem$prop ~ prop.fem$year,xlab="Year", ylab="Proportion",cex.axis=.7,ylim=c(0,.0022))
title("Proportion of TBI among Female",line=.2,cex.main=1)
plot(prop.male$prop ~ prop.male$year, xlab="Year", ylab="Proportion",cex.axis=.7,ylim=c(0,.0022))
title("Proportion of TBI among Male",line=.2,cex.main=1)
par(adj=0.5)
title(outer=T,line=-2,
      "Proportion of TBI across Year per Gender",cex.main=1.8) 


par(mfrow=c(1,2))
interaction.plot(dat4$sport,dat4$sex, dat4$prop,legend = F, xlab="Sport",ylab="Average proportion")  
legend("topleft", legend=c("Male","Female"),lty=c(1,2),cex=.8)
interaction.plot(dat4$year,dat4$sport, dat4$prop, legend = F, xlab="Year",ylab="Average proportion") 
legend("left", legend=c("Male","Female"),lty=c(1,2),cex=.8)
# interaction.plot(dat4$sex,dat4$year, dat4$prop,legend=F, xlab="Gender",ylab="Average proportion")
# legend("left", legend=c("Male","Female"),lty=c(1,2),bty="n")


prop.1990 <- dat4[which(dat4$year=="1990"),]
prop.1991 <- dat4[which(dat4$year=="1991"),]
prop.1992 <- dat4[which(dat4$year=="1992"),]

par(mfrow=c(1,3))
plot(prop.1990$prop ~ prop.1990$sport,xlab="Sport", ylab="Proportion",cex.axis=.7,ylim=c(0,.0022))
title("Proportion of TBI for 1990",line=.2,cex.main=1.2)
plot(prop.1991$prop ~ prop.1991$sport,xlab="Sport", ylab="Proportion",cex.axis=.7,ylim=c(0,.0022))
title("Proportion of TBI for 1991",line=.2,cex.main=1.2)
plot(prop.1992$prop ~ prop.1992$sport,xlab="Sport", ylab="Proportion",cex.axis=.7,ylim=c(0,.0022))
title("Proportion of TBI for 1992",line=.2,cex.main=1.2)
par(adj=0.5)
title(outer=T,line=-2,
      "Proportion of TBI across Sport per Year",cex.main=1.8) 



## NOTE: Only 1 observation of prop per all 3 groups
## ie, only 1 obs for prop for sex=F, sport=FootB, and year=1990
## need samples from multiple universities



plot(log(dat4$prop)~dat4$sex)

