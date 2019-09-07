library(effsize)
effects <- seq(0,100,0.2)
d <- rep(NA,length(effects))
d2 <- rep(NA,length(effects))
r2 <- rep(NA,length(effects))
r22 <- rep(NA,length(effects))
for(i in 1:length(effects)){
  pop1 <- rnorm(10000,100,10)
  pop2 <- rnorm(10000,100+effects[i],10)
  y <- c(pop1,pop2)
  x <- c(rep(1,10000),rep(0,10000))
  d[i] <- cohen.d(y~as.factor(x))$estimate
  r2[i] <- summary(lm(y~x))$r.squared
  x <- rnorm(10000,100,10)
  y <- x*(effects[i]/10)+rnorm(10000,100,10)
  d2[i] <- (2*coef(lm(y~x))[2]*sd(x))/sd(y)
  r22[i] <- summary(lm(y~x))$r.squared
}
# are they equivalent?
dev.new(height=4,width=4,noRStudioGD = T)
par(oma=c(1,1,0,0))
plot(d~r2,type="l",xlim=c(0,0.7),ylim=c(0,3),xlab="r squared",ylab="d")
lines(d2~r22,col="red")
legend("topleft",
       legend=c("Binary predictor","Continuous predictor, \nd = (2b*sd(x))/sd(y)"),
       lty=1,col=c(1,2),cex=0.7,bty="n")
