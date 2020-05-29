#107.3 - Residuos de Iris


irisset <- subset(iris, iris$Species=="setosa", select = c('Sepal.Length', 'Sepal.Width', 'Petal.Length'))
lm.iris <- lm(irisset$Sepal.Width~irisset$Sepal.Length)
summary(lm.iris)

#plot(irisset$Sepal.Width~irisset$Sepal.Length, pch=17)
#abline(lm.iris, col='red')
#par(new=T)
#plot(irisset$Sepal.Width~irisset$Sepal.Length-irisset$Petal.Length, pch=15)

(lm.iris.coef=coef(lm.iris))
#par(mfrow=c(2,2))
#plot(lm.iris)
#par(mfrow=c(1,1))
relar <- lm(irisset$Sepal.Width~irisset$Petal.Length)
relar <- residuals(relar)


recom <- lm(irisset$Sepal.Length~irisset$Petal.Length)
recom <- residuals(recom)

lm.iris.nopetal <- lm(relar~recom)
summary(lm.iris.nopetal)

(lm.iris.nopetal.coef <- coef(lm.iris.nopetal))

anova(lm.iris, lm.iris.nopetal)


