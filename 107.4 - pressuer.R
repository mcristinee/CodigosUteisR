# pressao r temperatura

pressure

p <- pressure$pressure
t <- pressure$temperature

#plot(p~t, pch=13)
reg1 <- lm(p~t)
#par(mfrow=c(2,2))
#plot(reg1)
#par(mfrow=c(1,1))
summary(reg1)$adj.r.squared

reg2 <- update(reg1, .~. +I(t^2))
#par(mfrow=c(2,2))
#plot(reg2)
#par(mfrow=c(1,1))
summary(reg2)$adj.r.squared

reg3 <- update(reg2, .~. +I(t^3))
#par(mfrow=c(2,2))
#plot(reg3)
#par(mfrow=c(1,1))
summary(reg3)$adj.r.squared
r2=summary(reg3)$r.squared
r2

