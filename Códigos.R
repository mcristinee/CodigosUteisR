## Gráficos e Análises preliminares ANOVA
#106.2

mudas <- read.table("altura-mudas.csv", header=T, sep=',')
mudas

boxplot(mudas$altura~mudas$especie)
str(mudas)
pai <- mudas$altura[mudas$especie=="paineira"]
tam <- mudas$altura[mudas$especie=="tamboril"]

hist(pai)
hist(tam)

length(mudas$altura)
espe <- data.frame(pai,tam)
espe
mediaespe= apply(espe,2,mean)
mediaespe
vetor.cor <- rep(c(1,4), each=60)
media.geral <- mean(mudas$altura)

#GRAFICO INTRA GRUPOS
vetor.obs <- 1:120
(vetor.dados <- c(pai,tam))

plot(vetor.obs,vetor.dados,pch=(rep(c(15,16),each=60)),
     col=vetor.cor,main="Variação Intra Grupos",ylab="Variável Resposta", 
     xlab="Observações")
for(i in 1:60)
{
  lines(c(i,i),c(vetor.dados[i],mediaespe[1]),col=vetor.cor[i])
}
for(i in 61:120)
{
  lines(c(i,i),c(vetor.dados[i],mediaespe[2]),col=vetor.cor[i])
}
lines(c(1,60),c(mediaespe[1],mediaespe[1]),col=1)
lines(c(61,120),c(mediaespe[2],mediaespe[2]),col=4)

### Gráfico entre Grupos

####GRAFICOS
plot(vetor.obs,vetor.dados,pch=(rep(c(15,16),each=60)),col=vetor.cor,ylab="Variável Resposta", xlab="Observações")
for(i in 1:120)
{
  lines(c(i,i),c(vetor.dados[i],mean(vetor.dados)),col=vetor.cor[i])
}
abline(h=media.geral)


###ANOVA NA UNHA

tamboril <- tam
medtam <- mediaespe[2]
ss.total <- sum((mudas$altura-media.geral)^2)
ss.total
#ss.intra somatorio do desvio quadratico intra grupos
#médias para os grupos
mtam=mean(tam)
mpai=mean(pai)

sstam <- sum((tam-mtam)^2)
sspai <- sum((pai-mpai)^2)
ss.intra <- c(sstam,sspai)

# ss.entre  soma dos desvios quadraticos entre grupos
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


