#REgressao linear Multipla

solo <- rep(c("are", "arg", "hum"), each=10)
adubo <- rep(rep(c(FALSE, TRUE), each=5), 3)
meansolo <- rep(c(9.9, 11.5, 14.3), each=10)
efeitoadubo <- rep(c(0, 2.7, 0, 0.7, 0, 0.2), each=5)
residuo <- rnorm(30, 0, 1)
dadosolo <- data.frame(solo, adubo, prod = meansolo + efeitoadubo + residuo)
str(dadosolo)

par( mar=c(4,4,2,2),   cex.lab=1.5, cex.axis=1.2, las=1, bty="n")
boxplot(prod ~  adubo + solo, data = dadosolo, ann= FALSE, xaxt= "n",  outline= FALSE, col = rep(c(rgb(0,0,0, 0.1),rgb(0,0,0, 0.5)), 3)  )
mtext(c("arenoso", "argiloso", "húmico"), side = 1, at= c(1.5, 3.5, 5.5), line = 1, cex = 2)
legend("bottomright", legend= c("sem", "com"),title = "Adubo", bty= "n", pch = 15, cex = 1.5,col = c(rgb(0,0,0, 0.1),rgb(0,0,0, 0.5))) 

##MODELOS
soloFull <- lm(prod ~ adubo + solo + solo:adubo, data = dadosolo)
summary(soloFull)
solo01 <- lm(prod ~ adubo + solo , data = dadosolo)
anova(solo01, soloFull)
solo00 <- lm(prod ~ 1 , data = dadosolo)
anova(solo00, soloFull)
anova(soloFull)

par(mfrow = c(2,2), mar=c(4,4,2,2), cex.lab=1.2,
    cex.axis=1.2, las=1,  bty="n")
plot(soloFull)


###BABIES
bebes <- read.table("babies.csv", header= TRUE, as.is = TRUE, sep= ";")
str(bebes)
mlfull <- lm(bwt ~ gestation + age + smoke
             + gestation:age + gestation:smoke
             + age: smoke + gestation:age:smoke, data = bebes) 
summary(mlfull)

ml01 <- lm(bwt ~ gestation + age + smoke
           + gestation:age + gestation:smoke
           + age: smoke, data = bebes) 
anova(ml01, mlfull)
summary(ml01)

## sem age:smoke
ml02 <- lm(bwt ~ gestation + age + smoke
           + gestation:age + gestation:smoke, data = bebes) 
anova(ml01, ml02)
## sem gestation:smoke
ml03 <- lm(bwt ~ gestation + age + smoke
           + gestation:age  + age:smoke, data = bebes)
anova(ml01, ml03)
## sem gestation:age
ml04 <- lm(bwt ~ gestation + age + smoke
           + gestation:smoke + age: smoke, data = bebes) 
anova(ml01, ml04)

summary(ml02)
confint(ml02)
anova(ml02)

par(mfrow = c(2,2), mar=c(4,4,2,2), cex.lab=1.2,
    cex.axis=1.2, las=1,  bty="n")
plot(ml02)




