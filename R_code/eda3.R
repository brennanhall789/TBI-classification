dat3 <- read.table("collegiate3.txt", col.names = c("sex","sport","year","x","tbiY","tbiN"))
suc <- dat3[,6]
fail <- dat3[,5]
dat3[,5] <- suc
dat3[,6] <- fail
dat3 <- dat3[,-4]

dat3$year <- as.factor(dat3$year)

tbi <- cbind(dat3[,4],dat3[,5])

par(mfrow=c(2,1))
tbiM <- subset(dat3, dat3$sex=="M")
tbiF <- dat3[c(1:15),]
plot(tbiM$tbiY ~ tbiM$sport)
plot(tbiF$tbiY ~ tbiF$sport)

plot(tbiM$tbiN ~ tbiM$sport)
plot(tbiF$tbiN ~ tbiF$sport)

plot(tbiM$tbiY ~ factor(tbiM$year))
plot(tbiF$tbiY ~ factor(tbiF$year))
