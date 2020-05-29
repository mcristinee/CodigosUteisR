# Aula 6 - Análises LIneares
rm(list=ls())
macho=c(120,107,110,116, 114, 111, 113,117,114,112)
femea=c(110,111,107, 108,110,105,107,106,111,111)

media.m=mean(macho)
media.m
media.f=mean(femea)
media.f
chacal=c(macho,femea)
sexo=factor(rep(c("macho","femea"),each=10))

boxplot(chacal~sexo, col="grey")

plot(1:20,chacal, pch=rep(c(15,16),each=10),col=rep(1:2,each=10))
for(i in 1:10)
{
  lines(c(i,i),c(chacal[i],media.m),col=1)
}
for(j in 11:20)
{
  lines(c(j,j),c(chacal[j],media.f),col=2)
}

lines(c(1,10),c(media.m,media.m),col=1)
lines(c(11,20),c(media.f,media.f),col=2)

mean(macho)
mean(femea)
mean(macho)-mean(femea)
sd(macho)
sd(femea)

dif=abs(mean(femea)-mean(macho))
dif

hist(chacal, freq=FALSE,xlim=c(95,125))
curve(exp=dnorm(x, mean=mean(chacal),sd=sd(chacal)),from=95,to=125, col="red", add=T)

rnorm(10,mean=mean(chacal),sd=sd(chacal))
round(rnorm(10,mean=mean(chacal),sd=sd(chacal)))
abs(round(rnorm(10,mean=mean(chacal),sd=sd(chacal))))
abs(round (mean(rnorm(10,mean=mean(chacal),sd=sd(chacal)))-mean(rnorm(10,mean=mean(chacal),sd=sd(chacal)))))

for(i in 1:10){
  cat( "\t \n", i)
  }

res=rep(NA,10)
for(i in 1:10){
  res[i]=abs(mean(round(rnorm(10,mean=mean(chacal),sd=sd(chacal))))-	mean(round(rnorm(10,mean=mean(chacal),sd=sd(chacal)))))
}
res

########FUncao ANIMADA - PROF adalardo
graphics.off()
quartz()
source('simula.R')
dif.chacal=simula(macho, femea)

dif.chacal=simula(macho,femea,nsim=2000)
table(dif.chacal$diferencas)
n=sum(abs(dif.chacal$diferencas)>=dif)
n
n/length(dif.chacal$diferencas)

dif.chacal.uni=simula(macho,femea, teste="uni")
dif.chacal.uni=simula(macho,femea, teste= "uni", nsim=2000)
table(dif.chacal.uni$diferencas)

n.maior=sum(round(dif.chacal.uni$diferencas,1)>=round(dif,1))
n.menor=sum(round(dif.chacal.uni$diferencas,1)<=round((dif*-1),1))
n.maior
n.menor
n.sim=2000

## Qual a probabilidade de errar ao fazer a afirmação que as mandibulas são diferentes?

p.bi=(n.maior+n.menor)/2000
p.bi



######TESTE T
(v1 <- var(macho))
(v2 <- var(femea))
(n1 <- length(macho))
(n2 <- length(femea))

(s12 <- sqrt((v1/n1)+(v2/n2)))
dif

(tvalor <- dif/s12) 

res.t <- simula(macho,femea,2000,"t")

## agora vamos calcular as probabilidades
maior.menor.t=sum(res.t$diferencas>=tvalor | res.t$diferencas<=-tvalor)
maior.menor.t
prob.t=maior.menor.t/2000
prob.t

####ANOVA

are=c(6,10,8,6,14,17,9,11,7,11)
are
arg=c(17,15,3,11,14,12,12,8,10,13)
arg
hum=c(13,16,9,12,15,16,17,13,18,14)
hum
solos=data.frame(are,arg,hum)
solos
str(solos)
boxplot(solos)
media.solos<-apply(solos,2,mean)
vetor.obs=1:30
vetor.dados=c(are,arg,hum)
media.geral=mean(c(are,arg,hum))
media.geral
dif.geral=solos-media.geral
dif.geral
sum(dif.geral)
round(sum(dif.geral),10)
ss.solos=dif.geral^2
ss.solos
ss.total=sum(ss.solos)
ss.total
vetor.cor<-rep(1:3, each=10)
vetor.medias<-rep(media.solos, each=10)
####GRAFICOS
plot(vetor.obs,vetor.dados,ylim=c(0,20),pch=(rep(c(15,16,17),each=10)),col=vetor.cor,ylab="Variável Resposta", xlab="Observações")
for(i in 1:30)
{
  lines(c(i,i),c(vetor.dados[i],mean(vetor.dados)),col=vetor.cor[i])
}
abline(h=media.geral)

#GRAFICO INTRA GRUPOS
plot(vetor.obs,vetor.dados,ylim=c(0,20),pch=(rep(c(15,16,17),each=10)),col=vetor.cor,main="Variação Intra Grupos",ylab="Variável Resposta", xlab="Observações")
for(i in 1:30)
{
  lines(c(i,i),c(vetor.medias[i],vetor.dados[i]),col=vetor.cor[i])
}
lines(c(1,10),c(media.solos[1],media.solos[1]),col=1)
lines(c(11,20),c(media.solos[2],media.solos[2]),col=2)
lines(c(21,30),c(media.solos[3],media.solos[3]),col=3)

#CALCULOS
solos
media.solos
ss.are=sum((are-media.solos["are"])^2)
ss.are
ss.arg=sum((arg-media.solos["arg"])^2)
ss.arg
ss.hum=sum((hum-media.solos["hum"])^2)
ss.hum
ss.intra=ss.are+ss.arg+ss.hum
ss.intra


plot(vetor.obs,vetor.medias,ylim=c(5,16),pch=(rep(c(15,16,17),each=10)),col=vetor.cor,main="Variação Entre Grupos",ylab="Variável Resposta", xlab="Observações")
for(i in 1:30)
{
  lines(c(i,i),c(vetor.medias[i],mean(vetor.medias)),col=vetor.cor[i])
}
abline(h=media.geral)
points(vetor.obs,vetor.dados,ylim=c(0,20),pch=(rep(c(0,1,2),each=10)),col=vetor.cor,cex=0.5)

#### Cálculo dos valores

media.solos=apply(solos,2,mean)
media.solos
media.geral
ss.entre=10*sum((media.solos-media.geral)^2)
ss.entre

ss.intra+ss.entre
ss.total

ms.entre=ss.entre/2
ms.intra=ss.intra/27
ms.entre
ms.intra
F.solos=ms.entre/ms.intra
F.solos
p.solos=pf(F.solos,2,27, lower.tail=FALSE)
p.solos


####GRAFICOS
curve(expr=df(x, 2,27),main="Distribuição F de Fisher (df=2,27)", xlab="Valor F",ylab="Densidade Probabilística (df)",xlim=c(0,10))
abline(v=F.solos,col="red")
abline(h=0, lty=2)
xf=seq(F.solos,10,0.01)
ydf=df(xf,2,27)
polygon(c(F.solos,xf),c(0,ydf),col="red")
text(locator(1),paste("pf(x) =",round(pf(F.solos,2,27,lower.tail=F),4)),cex=0.8, col="red")















