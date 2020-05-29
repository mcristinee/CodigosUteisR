#Criando TABELAS DE DADOS E DATA.FRAMES


#4.2 CERVEJAS
cervejas <-c("chope","lata","garrafa","chope","garrafa", "garrafa","lata","lata","nenhuma","lata","garrafa","garrafa", "garrafa","lata","lata","lata","garrafa","lata","chope","nenhuma", "garrafa","garrafa","garrafa","chope","garrafa","garrafa","chope","garrafa","lata","lata")


#Represente este resultado como um gráfico de barras e um dotplot (função dotchart).

  
cer <- table(cervejas)
barplot(cer)
dotchart(cer)

#Qual tem maior razão dado/tinta?
  #O melhor para visualizaçao dos dados com pouco gasto de tinta é o dotchart


#4.3 - Caixetas
caix <- read.table("caixeta.csv", header=T, sep=",")
head(caix)  
caix$dap = (pi/4)* (caix$cap/10)
hist( caix$dap )
hist( caix$dap[ caix$local == "chauas" ] )
quartz()
hist( caix$dap[ caix$local == "jureia" ] )
quartz()
hist( caix$dap[ caix$local == "retiro" ] )

#Sim, há diferencá entre a frequencia tambem na largura das classes.  
#Chauas apresenta maior frequencia na primeira classe e nao na segunda como Jureia e Retiro.


#4.4 Boxplot
#Neste exercício, use o conjunto de dados Inventário em Florestas Plantadas de 
#Eucalyptus grandis.
dir()
EG <- read.table("egrandis.csv", header=T, sep=";")
graphics.off()
#Utilize o gráfico boxplot para analisar o DAP de árvores de E. grandis em 
#função das variáveis região (regiao) e rotação (rotacao).

boxplot(EG$dap~EG$rotacao*EG$regiao, outline=F)

#Avalie a normalidade da altura do conjunto total de árvores com um 
#gráfico quantil-quantil contra a distribuição normal.

qqnorm(EG$ht)
qqline(EG$ht)
#Dados nao normais que podem ser tambem analisados por histograma

hist(EG$ht)

#4.5 MAIS CAIXETA

#Aqui usaremos novamente o objeto caixeta, criado no tutorial 
#Exploração de uma Variável Categórica.

#Analise a relação dap-altura ('dap' e 'h') em função do caixetal (local) com a função plot, 
#mas somente para as árvores 2) de caixeta (Tabebuia cassinoides).

caix <- read.table("caixeta.csv", header=T, sep=",", as.is=T)
caix$dap <- (pi/4)* (caix$cap/10)


tabeuia= subset(caix, caix$especie=="Tabebuia cassinoides", select=c("arvore","h","local","dap")) 
tabeuia$local
retiro <- subset(tabeuia, tabeuia$local=='retiro')
chauas <- subset(tabeuia, tabeuia$local=='chauas')
jureia <- subset(tabeuia, tabeuia$local=='jureia')
par(mfrow=c(1,3))
plot(retiro$dap~retiro$h, ylab="DAP", xlab="Altura")
plot(jureia$dap~jureia$h,ylab="DAP", xlab="Altura")
plot(chauas$dap~chauas$h,ylab="DAP", xlab="Altura")

#tambem pode ser visualizado no xyplot do lattice
library(lattice)
xyplot(tabeuia$dap~tabeuia$h|tabeuia$local,ylab="DAP", xlab="Altura")

#Para a mesma relação do item anterior, verifique linearidade com a função scatter.smooth

scatter.smooth(retiro$dap~retiro$h, ylab="DAP", xlab="Altura", col="blue")
scatter.smooth(jureia$dap~jureia$h,ylab="DAP", xlab="Altura", col='red')
scatter.smooth(chauas$dap~chauas$h,ylab="DAP", xlab="Altura", col='brown')


#Utilizando o pacote lattice, analise a relação dap-altura ('dap' e 'h') em função do caixetal (local),
#mas somente para as árvores 3) de caixeta (Tabebuia cassinoides).

library(lattice)
xyplot(tabeuia$dap~tabeuia$h|tabeuia$local,ylab="DAP", xlab="Altura")



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

#107.4

library(MASS)
data("Animals")
str(Animals)

anim.m2 <- lm(log(brain)~log(body),data=Animals, 
              subset=!(log(Animals$body)>8&log(Animals$brain)<6))
anim.m0 <- lm(log(brain)~1, data=Animals, 
              subset=!(log(Animals$body)>8&log(Animals$brain)<6))
anova(anim.m2, anim.m0)

#qual a diferença entre a anova abaixo e a anterior?
#A anova de um único modelo, retorna a partiçao de variancia enquanto a de 2 modelos retorna a particáo de variancia comparando os modelos. 

anova(anim.m2)


#QUal a relaçao dos valores obtidos por estes comandos. 
summary(anim.m0)
mean(log(Animals$brain[!(log(Animals$body)>8&log(Animals$brain)<6)]))
sd(log(Animals$brain[!(log(Animals$body)>8&log(Animals$brain)<6)]))

#summary está me mostrando os coeficientes e dados do objeto anim.m0. 
# Onde o intercept é igual a média calculada no vetor pq nao podemos calcula mean(anim.m0)
#o Erro padrao mostrado pelo summary é o mesmo que o calculado no vetor. 

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

#107.5
aves <- read.table("aves_cerrado.csv", header = T, sep=";")
aves
A<- aves[is.na(aves$urubu)|is.na(aves$carcara)|is.na(aves$seriema),]
aves$seriema[is.na(aves$seriema)] <- 0
aves$urubu[is.na(aves$urubu)] <- 0
aves$carcara[is.na(aves$carcara)] <- 0
table(aves$fisionomia)
aves$fisionomia[aves$fisionomia=="ce"] <- "Ce"
summary(aves)


aves.ce <- subset(aves, aves$fisionomia=="Ce", select = c('carcara', 'seriema'))
aves.cc <- subset(aves, aves$fisionomia=="CC", select = c('carcara', 'seriema'))
aves.cl <- subset(aves, aves$fisionomia=="CL", select = c('carcara', 'seriema'))

str(aves.ce)
str(aves.cc)
str(aves.cl)

mod.ce <- lm(aves.ce$seriema~aves.ce$carcara)
mod.cc <- lm(aves.cc$seriema~aves.cc$carcara)
mod.cl <- lm(aves.cl$seriema~aves.cl$carcara)
summary(mod.ce)
summary(mod.cc)
summary(mod.cl)

p.ce <- summary(mod.ce)$coefficients[2,4]
p.cc <- summary(mod.cc)$coefficients[2,4]
p.cl <- summary(mod.cl)$coefficients[2,4]

coef.ce <- coef(mod.ce)
coef.cc <- coef(mod.cc)
coef.cl <- coef(mod.cl)


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

###ANOVA pela Funçao

aov.tamboril <- aov(tamboril$altura~as.factor(tamboril$substrato))

summary(aov.tamboril)

## Criando Funçoes 

#Graus Celsius

Fa <- round(rnorm(30,90 , 9),1)
Fa

conversor <- function(x){ 
  celsius <- 5/9*((x)-32)
  data.frame(x,celsius)
}

conversor(Fa)

#109.3 - Indices de Diversidade
#rm(list=ls())

shannon <- function(x){
  x[x==0] <- NA
  x <- na.omit(x)
  N <- sum(x) 
  step2 <- (x/N)*log(x/N)
  H=sum(step2)
  H=H*-1
  return(H)
}

simpson <- function(x){
  x[x==0] <- NA
  x <- na.omit(x)
  N <- sum(x) 
  d <- (x/N)^2
  D <- sum(d)
  return(D)
}

diversidade <- function (x, INDEX=c("simpson", "shannon")){
  INDEX <- match.arg(INDEX)
  if(INDEX=="simpson"){
    D <- apply(x,2,FUN="simpson")
    return(D=D)
  }
  if(INDEX=="shannon"){
    H <- apply(x,2,FUN="shannon")
    return(H=H)
  }
}


m=round(matrix(runif(100,1,22), ncol=10),0)
m[3:8,1:3] <- NA
diversidade(m,"sh")
diversidade(m,"si")

x <- rpois(60,2)
x[2:5] <- NA
x

##FUNCAO NOVA
library(scales)

#Nao utilizai a ed.shapes pq nao vimos em aula. 
#Só rodei a minha funcao com análise exploratória a partir de histograma e boxplot
explore<- function(dados1, dados2){
if(class(dados1)!= 'numeric'|class(dados2)!= 'numeric'){
  stop("Os Dados precisam ser numericos")
}
if(length(dados1)!= length(dados2)){
    stop("Os vetores precisam ter o mesmo comprimento")
}
  par(mfrow=c(1,2),mar=c(5,4,4,2), cex.axis=1.2,cex.lab=1.2,las=1,bty="l", tcl=0.3,cex=1.2)
  hist(dados1,freq=F, main="", xlab='',ylab='', ylim=c(0,0.6),breaks=10,
     col=alpha('blue', 0.45), border=F,xlim=range(c(dados2,dados1)))
hist(dados2, freq=F,main="", xlab='',ylab='',border = F,col=alpha('red', 0.45), breaks=10,add=T)
coordx <- min(c(dados1,dados2))
coordy <- 0.6
legend(x=coordx,y=coordy, legend=c("Var1","Var2"), pch=19, col=alpha(c("blue","red"),0.3) , bty="n", cex=1)
mtext("Densidade probabilistica", side=2, las=0, line = 2.5, cex=1.3)
mtext("Variavel analisada", side=1, las=0, line = 2.5, cex=1.3)
boxplot(dados1,dados2,pch=16,xlab="")
mtext("Variavel analisada", side=2, las=0, line = 2.5, cex=1.3)
mtext(c("Var1","Var2"), at=c(1,2), side=1, las=0, line = 1.5, cex=1.3)
res <- summary(dados1)
res2 <-summary(dados2) 
o <- cbind(dados1,dados2)
correl <- cor(o,method =  "spearman")
resultado <- list(res, res2, correl[1,2])
names(resultado) <- c("sumario variavel 1", 'sumario variavel 2', "correlacao")
return(resultado)
}

#TESTE
test <- explore(rnorm(1000,10,1),rnorm(1000,8,1))

#### MONTE CARLO

##REAmostragem e simulacao

L <- LETTERS[1:10]

sample(L)
sample(L, replace=T, 40)
sample(L,prob=c(0.1,0.2,0.05,0.05,0.2,0.1,0.05,0.05,0.1,0.1), replace=T)

macho=c(120,107,110,116, 114, 111, 113,117,114,112)
femea=c(110,111,107, 108,110,105,107,106,111,111)
macho
femea
sexo=rep(c("macho", "femea"), each=10)
sexo 
mf=c(macho,femea)
mf
macho.m=mean(macho)
macho.m
femea.m=mean(femea)
femea.m
macho.m-femea.m
dif.mf=diff(tapply(mf,sexo,mean))
dif.mf

s1.mf=sample(mf)
s1.mf
diff(tapply(s1.mf,sexo,mean))
##+1
s2.mf=sample(mf)
s2.mf
diff(tapply(s2.mf,sexo,mean))
##+2
diff(tapply(sample(mf),sexo,mean))
##+3
diff(tapply(sample(mf),sexo,mean))
##+1000
### e agora? fazer na mão 1000 vezes? ###
res <- rep(NA,1000)
res[1] <- dif.mf
for(i in 2:1000){
  res[i] <- diff(tapply(sample(mf), sexo, mean))
}
hist(res)
abline(v=res[1], col="red", lty=5)
abline(v=res[1]*-1, col="red", lty=5)


(bicaudal <- sum(res>=res[1]|res<=res[1]*-1))
(pbi <- bicaudal/length(res))

unicaudal <- sum(res>=res[1])
(puni <- unicaudal/length(res))

macho
macho.m

mean(sample(macho,replace=T))
nsim <- 1000
boot <- rep(NA,nsim)
boot[1] <-  macho.m
for(i in 2:nsim){
  boot[i] <- mean(sample(macho,replace=T))
}
boot

conf <- quantile(boot, prob=c(0.025,0.975))
conf

hist(boot, col="grey70")
abline(v=conf[1], lty=5, col="red")
abline(v=conf[2], lty=5, col="red")

###Tesourinha e deriva continental

data.coef<-matrix(c(NA, .30, .14, .23, .30, -0.04, 0.02, -0.09, NA, NA, .50,.50, .40, 0.04, 0.09, -0.06, NA, NA, NA, .54, .50, .11, .14, 0.05, rep(NA, 4), .61, .03,-.16, -.16, rep(NA, 5), .15, .11, .03, rep(NA, 6), .14, -.06, rep(NA, 7), 0.36, rep(NA, 8)), nrow=8, ncol=8)
rownames(data.coef) <- c("Eur_Asia", "Africa", "Madag", "Orient", "Austr", "NewZea", "SoutAm", "NortAm")
colnames(data.coef) <- c("Eur_Asia", "Africa", "Madag", "Orient", "Austr", "NewZea", "SoutAm", "NortAm")
data.coef

#OUTRAS duas matrizes com a distancia atual e antes da deriva
dist.atual<-matrix(c(NA,1,2,1,2,3,2,1, NA, NA, 1,2,3,4,3,2, NA, NA, NA,3,4,5,4,3, rep(NA, 4),1,2,3,2, rep(NA, 5), 1,4,3, rep(NA, 6), 5,4, rep(NA, 7), 1, rep(NA, 8)), nrow=8, ncol=8)
dist.atual
dist.deriva<- matrix(c(NA,1,2,1,2,3,2,1, NA, NA, 1,1,1,2,1,2, NA, NA, NA,1,1,2,2,3, rep(NA, 4),1,2,2,2, rep(NA, 5), 1,2,3, rep(NA, 6), 3,4, rep(NA, 7), 1,  rep(NA, 8)), nrow=8, ncol=8)
# colocando nomes nas matrizes
rownames(dist.atual) <- colnames(dist.atual)<- c("Eur_Asia", "Africa", "Madag", "Orient", "Austr", "NewZea", "SoutAm", "NortAm")

colnames(dist.deriva)<- rownames(dist.deriva)<-  c("Eur_Asia", "Africa", "Madag", "Orient", "Austr", "NewZea", "SoutAm", "NortAm")
# olhando as matrizes
dist.atual
dist.deriva


#correlacao para verificar se valores de uma variam na mesma direção da outra (+1), 
#em direção contrária (-1) ou não são correlacionadas (0). 
cor12<-cor(as.vector(data.coef), as.vector(dist.atual), use="complete.obs")
cor13<-cor(as.vector(data.coef), as.vector(dist.deriva), use="complete.obs")
cor12 ## correlação com a distancia atual
cor13 ## correlação com a distancia antes da deriva

data.sim<-data.coef # copia da matriz que será aleatorizada
data.sim 

# preenchendo o triangulo superior da matriz com os dados correspondentes do triangulo inferior
data.sim[upper.tri(data.sim)] <- t(data.coef)[(upper.tri(data.coef))] 

data.sim # olhando a matriz
data.sim[8:1, 8:1] # uma matriz baguncada mas que mantem certa estrutura
sim.pos<-sample(1:8) # posicoes permutadas 
sim.pos
data.sim<-data.sim[sim.pos, sim.pos] # aqui uma matriz verdadeiramente permutada
cor12.sim<-cor(as.vector(data.sim), as.vector(dist.atual), use="pairwise.complete.obs")
cor13.sim<-cor(as.vector(data.sim), as.vector(dist.deriva), use="pairwise.complete.obs")
cor12.sim 
cor13.sim
cor12 ## correlação observada com a distancia atual
cor13 ## correlação observada com a distancia antes da deriva
########################################################
### Repetir a simulação muitas vezes ###################
#######################################################
res.cor=data.frame(sim12=rep(NA, 5000), sim13=rep(NA,5000))
str(res.cor)
res.cor[1,]<-c(cor12, cor13)
str(res.cor)
for(s in 2:5000)
{
  sim.pos<-sample(1:8)
  data.sim<-data.sim[sim.pos, sim.pos]
  res.cor[s,1]<-cor(as.vector(data.sim), as.vector(dist.atual), use="pairwise.complete.obs")
  res.cor[s,2]<-cor(as.vector(data.sim), as.vector(dist.deriva), use="pairwise.complete.obs")
}
str(res.cor)
par(mfrow=c(2,1), mar=c(5,4,4,2))
hist(res.cor[,1])
abline(v=res.cor[1,1], col="red")
hist(res.cor[,2])
abline(v=res.cor[1,2], col="red")
#### calculando o P ###########
p12=sum(res.cor[,1]<= res.cor[1,1])/(dim(res.cor)[1])
p12
p13=sum(res.cor[,2]<= res.cor[1,2])/(dim(res.cor)[1])
p13

vetor <- (1:60)
num <- 10000
ts <- rep(NA,num)
for(i in 1:num){
  ts[i] <- sample(vetor)  
}
sort(table(ts),decreasing=TRUE)[1:6]



###TESTE de PERMUTAÇAO
rm(list=ls())

lagartos <- data.frame(sexo=factor(rep(c("m","f"),c(24,21))),
                       ncoleop=c(256,209,0,0,0,44,49,117,6,0,0,75,34,13,0,90,0,32,0,205,332,0,31,0,0,89,0,0,0,163,289,3,843,0,158,443,311,232,179,179,19,142,100,0,432))
lagartos
#DADOS NAO ATENDEM PREMISSAS DO TESTE T
tapply(lagartos$ncoleop,lagartos$sexo,summary)
(medias <- tapply(lagartos$ncoleop,lagartos$sexo,mean))
(dif.obs <- abs( diff( as.vector(medias))))
tapply(lagartos$ncoleop,lagartos$sexo,var)

qqnorm(lagartos$sexo)

#entao usaremos um teste de permutaçao
##cria um vetor para armazenar os resultados

results <- c()

##Permuta os valores das medidas, calcula a diferença absoluta entre as médias e 
##armazena no vetor "results". Repete a operação mil vezes
for(i in 1:1000){
results[i] <- abs( diff( tapply(sample(lagartos$ncoleop),lagartos$sexo,mean) ) )
 }

sum(results >= dif.obs)

plot(density(results),xlab="Diferença Absoluta das Médias",ylab="Freq Relativa", main="")
abline(v = dif.obs, col="red")

##PERMUTACOES EM MATRIZ
sp1 <- c(1,1,0,0,0,0)
sp2 <- c(1,1,1,0,0,0)
sp3 <- c(0,0,1,1,0,0)
sp4 <- c(0,0,1,1,1,0)
sp5 <- c(0,0,0,0,1,1)
sp6 <- c(0,0,0,0,1,1)
cc2 <- matrix(c(sp1,sp2,sp3,sp4,sp5,sp6), nrow=6,ncol=6,byrow = T)
cc2
rownames(cc2) <- c('sp1','sp2', 'sp3', 'sp4', 'sp5', 'sp6')


#Numero de especies por local
(S.obs <- apply(cc2,2,sum))
##Maximo de especies em coexistencia
max(S.obs)

#PERMUTAR VALORES NAS LINHAS
apply(cc2,1,sample)

#Ops! a função apply retorna sempre seus resultados em colunas, transpondo a matriz original. 
#Para evitar isso, transpomos o resultado, voltando as espécies para as linhas: 
t(apply(cc2,1,sample))

#Para repetir várias vezes podemos criar Uma funcao para calcular o maximo de especies
#em co-ocorrencia
max.c <- function(x){ max(apply(x,2,sum)) }
max.coc <- max.c(cc2)
max.coc

##Um vetor para guardar os resultados
 result.c <- c()
## Mil simulações em um loop
 for(i in 1:1000){
     result.c[i] <- max.c( t(apply(cc2,1,sample)))
 }

sum(result.c<=max.coc)

#23% das amostras retornaram uma co-ocorrencia acima da calculada para amostra, portanto
#nao podemos afirmar que o máximo de co-ocorrencias é 3.

#Reamostragem com reposiçao - BOOTSTRAP
rm(list=ls())
data(BCI,package = "vegan")
BCI[1:3,25:28]

#funcao para calcualr indice de SHannon em cada um das parcelas
H <- function(x){
     y <- x[x>0]
     pi <- y/sum(y)
     -sum(pi*log(pi))
   }
(H.BCI <- apply(BCI,1,H))

hist(H.BCI,xlab="Índice de Shannon")
qqnorm(H.BCI);qqline(H.BCI)

##Uma amostra ao acaso de 20 parcelas
bci.sample <- BCI[sample(1:nrow(BCI),20),]
ha <-  H(bci.sample)
H.am <-apply(BCI, 1, ha)
##Estimativas da média e intervalo de confiança baseados na distribuição de t de Student:
t.test(H.am)




m1 <- matrix(data=rnbinom(n=10000,size=2,mu=2),nrow=10,ncol=1000)
m2 <- matrix(data=rnbinom(n=10000,size=2,mu=3),nrow=10,ncol=1000)
matrizona <- rbind(m1,m2)
tratamento <- factor(x=rep(c("a","b"),each=10))
(signif.t <- function(valores,fator){(t.test(formula=valores~fator))$p.value})
(sum(apply(X=matrizona,MARGIN=2,FUN=signif.t,fator=tratamento)<=0.05))

#PODE SER UMA COMPARACAO ENTRE POPULACOES OU COMUNIDADES.


#====================================================================
#SCRIPT DE EXEMPLOS DA AULA DE ANALISE EXPLORATORIA 2019

#para rodar os exemplos desse script, voce precisara
#dos arquivos de dados de caixeta, egrandis, esaliga e aves
#que estao disponiveis no site do curso:
#http://ecologia.ib.usp.br/bie5782/doku.php?id=dados:start
#Voce tambem vai precisar dos dados dos pardais, que estao
#no material suplementar do artigo de Zuur et al. 2010
#https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/j.2041-210X.2009.00001.x

#====================================================================
#ajustando o diretÃ³rio de trabalho
#ajuste o diretorio adequado no seu computador usando setwd()
getwd()

#conferindo os arquivos da pasta
dir()

#limpando a area de trabalho
rm(list=ls())
#--------------------------------------------------------------------
#BLOCO 1 - CONFERENCIA, NAs, zeros 

#leitura dos dados de aves
aves = read.table("aves_cerrado.csv", sep=";", header=TRUE, row.names = 1)

#dando aquela primeira conferida

#str, head, tail
str(aves)
head(aves)
tail(aves)
#localizanto NAs com which() e is.na()
is.na(aves$urubu)
which(is.na(aves$urubu))

#eliminando ou substituindo NAs com na.omit()

aves$urubu[is.na(aves$urubu)] = 0
which(is.na(aves$urubu)) #nao sobrou nenhum NA

x = c(1,2,3,56,78,9,0,NA, NA) #criando um vetor com alguns NAs
na.omit(x) #usando na.omit()
#--------------------------------------------------------------------
#BLOCO 2 - explorando dados quantitativos/continuos

#lendo os dados de esaligna
esa = read.table("esaligna.csv", sep=",", header=TRUE)

#conferindo
str(esa)
head(esa)

#medidas descritivas do conjunto de dados

#media e desvio, esa$folha
mean(esa$dap)

#range
range(esa$dap)

#resumo dos cinco numeros
summary(esa$ht)
summary(esa)

#GRAFICOS UNIVARIADOS

#histograma
hist(esa$folha, col="grey")

#boxplot
boxplot(esa$dap, col="grey")

#plot(density())
density(esa$folha)
plot(density(esa$folha))

#dotchart e dotchart genÃ©rico
dotchart(aves$urubu)
plot(x = aves$urubu, y=1:nrow(aves))


#--------------------------------------------------------------------
#BLOCO 3 - normalidade

#histograma e curva com esa$ht
hist(esa$ht)

#diminuir o numero de breaks
hist(esa$ht, breaks=5, col="grey", freq = FALSE)

#adicionando a curva esperada segundo a distribuicao normal
curve(dnorm(x=x, mean = mean(esa$ht), 
            sd = sd(esa$ht)), col="purple",
      add=TRUE)

#qqnorm() e qqline()
qqnorm(esa$ht)
qqline(esa$ht)

#qqnorm e qqline com dados simulados

#simulando dados de mesmo comprimento, media e desvio
hts = rnorm(nrow(esa), mean(esa$ht), sd(esa$ht))

#desenhando o grafico com qqnorm() e qqline()
qqnorm(hts, pch=16)
qqline(hts)

#density + curva
plot(density(esa$ht))
curve(dnorm(x=x,mean(esa$ht), sd(esa$ht)),
      col="red", add=TRUE)
#--------------------------------------------------------------------
#BLOCO 4 - dados qualitativos/categoricos

#lendo os dados de caixeta
cax = read.table("caixeta.csv", sep=",", header=TRUE)
head(cax)

#table por espÃ©cie
table(cax$especie)

#table por espÃ©cie e local
table(cax$especie, cax$local)

#tableception
table(table(cax$especie))

#barplot de table(cax$local)
table(cax$local)
barplot(table(cax$local))
barplot(table(cax$especie))

#xtabs com TitanicDF
data(Titanic)
class(Titanic)

TitanicDF = data.frame(Titanic)
head(TitanicDF)

xtabs(TitanicDF$Freq~TitanicDF$Survived+TitanicDF$Age)
xtabs(Freq~Survived+Age, data=TitanicDF)

#--------------------------------------------------------------------
#BLOCO 5 - dados bivariados

#inventando dados para o plot de florzinhas
flor = cbind(rep(1:10, 1:10), rep(1:10, 1:10))
flor
plot(flor)
sunflowerplot(flor)

#lendo os dados de egrandis
egrandis = read.table("egrandis.csv", sep=";", header=TRUE)
head(egrandis)

#plot de dap e ht

#scatter.smooth de esa ht e dap
scatter.smooth(esa$ht~esa$dap)

#para embonecar a linha, tem p argumento lpars
scatter.smooth(esa$ht~esa$dap, lpars = list(lwd=3, col="red"))

#funcao cor
cor(esa[,4:8])

#sunflowerplot, egrandis dap e ht
sunflowerplot(dap~ht, data=egrandis)

#boxplot - dap por regiao em egrandis
boxplot(dap~regiao, data=egrandis)

#apply com anscombe e esa[,4:8]

data("anscombe")
anscombe
apply(anscombe, 2, mean) #media de todas as colunas anscombe
apply(anscombe[,1:4], 1, mean) #media so das colunas 1-4

#tapply e aggregate
tapply(cax$h, 
       list(cax$local),
       mean)

tapply(cax$h, 
       list(cax$local, cax$parcela),
       mean)

tapply(cax$h, 
       list(cax$local, cax$parcela, cax$especie),
       mean)

aggregate(cax$h, 
          list(cax$local, cax$parcela),
          sd)

#pacote lattice
library(lattice)

#xyplot de egrandis dap por ht por regiao
xyplot(dap~ht|regiao, data=egrandis)

#histogram de egrandis vol por regiao
histogram(~vol|regiao, 
          data=egrandis[egrandis$regiao %in% c("Salto", "Botucatu"),])

#--------------------------------------------------------------------
#BLOCO 6 - dados multivariados

pardal = read.table("SparrowsElphick.txt", sep="\t", dec=".", head=TRUE)

#o retorno das funcoes plot e cor
plot(pardal[,1:7])
cor_pardal = cor(pardal[,1:7])
cor_pardal

#correlacoes com corrplot
#se tiver que instalar o pacote, rode o seguinte comando
#install.packages(corrplot)

#depois eh so carregar o pacote e usar
library(corrplot)
corrplot(cor_pardal)

#matriz de similaridade, dados de barro colorado
library(vegan)
data(BCI)
BCI = as.matrix(BCI)
str(BCI)

#funcao vegdist, method="jaccard", linhas 1:10
BCI_dist = vegdist(BCI[1:10,], method = "jaccard")
corrplot(as.matrix(BCI_dist))


#correlacoes e pca das colunas de 1:7
pca = princomp(pardal[,1:7])
biplot(pca)

str(pca)
str(pca$scores)

#plot dos componentes 1 e 2
plot(x = pca$scores[,1], y = pca$scores[,2],
     col = pardal$SpeciesCode, pch=19)

#plot do objeto do pca
plot(pca)



#Graficos
par(cex.axis=1,cex.lab=1.2,las=1,bty= "l", tcl=-0.2,cex=1.3,mar=c(4,4,3,2), pch=16, family="serif")


