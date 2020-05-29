#REGRESSAO LINEAR SIMPLES

altura2anos <- c(39,30,32,34,35,36,36,30)
alturaad <- c(71,63,63,67,68,68,70,64)
esperado <- (altura2anos*2)
alturas <- data.frame(altura2anos,alturaad)
alturas.lm <- lm(alturaad~altura2anos)
summary(alturas.lm)



(alturas.conf <- confint(alturas.lm, level = 0.95))

#espe <- lm(esperado~altura2anos)
#plot(alturaad~altura2anos, pch=16)
#abline(alturaslm, col="blue")
#abline(espe, col="red", dashed)
#text(33,70.8,"35.17+0.92x \n R-squared = 0.87" )



#Há uma ligaçao significativa entre as alturas
