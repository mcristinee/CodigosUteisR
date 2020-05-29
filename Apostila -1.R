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