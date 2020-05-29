# Wiki Exercicios Regressao Linear

## 1 -Uma estimativa da incerteza na previsão do modelo

babies <- read.table("babies.csv", header=T, sep=";")
str(babies)
summary(babies)
peso <- lm(babies$bwt~babies$gestation)
summary(peso)
par()

par(mar=c(5,4,4,2), cex.lab=1.5, ann=F, mfrow=c(1,1), bty="n")
#rect(par()$usr[1], par()$usr[3], par()$usr[2], par()$usr[4], col=rgb(1,1,1,0.5)))
plot(bwt~gestation, data=babies, pch=16, col=rgb(0,0,1,0.3))
abline(peso, col="red")
#abline(peso1,babies$gestation,col="orange")
newx=seq(min(babies$gestation)-10,max(babies$gestation)+10, length.out = 1174) 
pre <- predict(peso,newdata = data.frame(gestation=newx), interval = "confidence", level = 0.95)
lines(babies$gestation,pre[,2], col="grey", lty=1)
lines(babies$gestation,pre[,3], col="grey", lty=1)



##ESTIMATIVA

tempo <- seq(min(babies$gestation)-10,max(babies$gestation)+10, length.out = 100) 

tempo
dif <- babies$gestation-mean(babies$gestation)
SSX <- sum(dif^2)
s2 <- var(tempo)
n <- length(tempo)
x <- tempo
x1 <- mean(tempo)
sey <- sqrt(s2*(1/n+(x-x1)^2)/SSX)
sey

limsup <- x+sey
liminf <- x-sey
plot(bwt~gestation, data=babies, pch=16, col=rgb(0,0,1,0.3))
lines(liminf, col="green", lty=1)
lines(limsup, col="green", lty=1)

length(liminf)


#2  - GALILEU
rm(list=ls())
init.h = c(600, 700, 800, 950, 1100, 1300, 1500)
h.d = c(253, 337, 395, 451, 495, 534, 573)

plot(h.d~init.h, pch=16, col=rgb(0.7,0,0.7,0.8))
mod1 <- lm(h.d~init.h)
mod2 <- update(mod1,.~. +I(init.h^2))
anova(mod1,mod2)
abline(mod1)
cf.m2 <- coef(mod2)
curve(cf.m2[1]+cf.m2[2]*x+cf.m2[3]*x^2, add=T, lty=2)

mod3 <- update(mod2,.~. +I(init.h^3))
summary(mod3)
cf.m3 <- coef(mod3)
curve(cf.m3[1]+cf.m3[2]*x+cf.m3[3]*x^2+cf.m3[4]*x^3, add=T, lty=3)

anova(mod2,mod3)
summary(mod3)
par(mar=c(5,4,4,1), mfrow=c(2,2), bty='n', ann='n')
plot(mod3)

#QUando pensamos em um modelo menos complexo e com melhor ajuste dos residuos a normalidade
#Devemos entao, considerar o modelo polinomial = a+bx+cx^2 como mais adequado.


#3 bbies

babies <- read.table("babies.csv", header=T, sep=";")
str(babies)

mod1 <- lm(bwt~gestation, data=babies)
plot(bwt~gestation, data=babies)
abline(mod1)
mod2 <- lm(bwt~height, data=babies)
plot(bwt~height, data=babies)
abline(mod2)
summary(mod2)
mod3 <- lm(bwt~weight, data=babies)
plot(bwt~weight, data=babies)
abline(mod3)
summary(mod3)
mod4 <- lm(bwt~smoke, data=babies)
plot(bwt~smoke, data=babies)
abline(mod4)
summary(mod4)
mod5 <- lm(bwt~parity, data=babies)
plot(bwt~parity, data=babies)
abline(mod5)
summary(mod5)
mod6 <- lm(bwt~age, data=babies)
plot(bwt~age, data=babies)
abline(mod6)

par(mfrow=c(2,2), ann="n", bty="n")
plot(mod1)
plot(mod2)
plot(mod3)
plot(mod4)
plot(mod5)
plot(mod6)

mod7 <- lm(bwt~gestation+age+gestation:age, data=babies)
summary(mod7)
mod8 <- lm(bwt~gestation+age+smoke+weight, data=babies)
summary(mod8)
plot(mod8)
?cor
plot(cor(babies))
      