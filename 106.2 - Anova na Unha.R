###ANOVA NA UNHA

mudas <- read.table("altura-mudas.csv", header=T, sep=',')
mudas
tamboril <- subset(mudas, mudas$especie=="tamboril", select=c("especie","bloco", 'substrato','altura'))
tamboril
medtam <- mean(tamboril$altura)
ss.total <- sum((tamboril$altura-medtam)^2)
ss.total

#ss.intra somatorio do desvio quadratico intra grupos
#médias para os grupos

subs <- as.factor(tamboril$substrato)
subs


mtam=mean(tamboril$altura)
mtamsubs <- tapply(tamboril$altura, as.factor(tamboril$substrato), FUN=mean)
mtamsubs
sd <- tapply(tamboril$altura, as.factor(tamboril$substrato), FUN=sd)

ss.intra <- sum((tamboril$altura-rep(mtamsubs, each=6))^2) 
ss.intra

# ss.entre  soma dos desvios quadraticos entre grupos


ss.entre <- ss.total-ss.intra

desvio.mediointra <- ss.intra/50
desvio.medioentre <- ss.entre/9


razao <- desvio.medioentre/desvio.mediointra
razao

prob <- pf(razao,9,50, lower.tail=FALSE)
prob
porc <- ss.entre/ss.total*100
porc



