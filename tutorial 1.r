##Gerando dados
set.seed(42)
area <- rnbinom(10,mu=500,size=0.5)
riqueza <- rpois(10,lambda=(area^0.38))

##Tutorial
area <- c(300, 350, 961, 295, 332, 47,  122, 11, 53, 2749)
riqueza <- c(1, 7, 20, 7, 8, 4, 8, 3, 5, 23)
area
riqueza
summary(area)
summary(riqueza)
mean(x=area)
varea <- var(area)
varea
sqrt(varea)
sd(x=area)
mean(riqueza)
var(riqueza)
sd(riqueza)
plot(x=area, y=riqueza, xlab="Area (ha)", ylab="N?mero de Esp?cies")
modelo1 <- lm(riqueza~area)
summary(modelo1)
previsto <- fitted(modelo1)
riqueza - previsto
residuals(modelo1)
par(mfrow=c(2,2))
plot(modelo1)
par(mfrow=c(1,1))
plot(x=area, y=riqueza, xlab="Area (ha)", ylab="N?mero de Esp?cies")
abline(modelo1)
plot(x=area, y=riqueza, xlab="Log Area (ha)", ylab="Log N?mero de Esp?cies", log="xy")
modelo2 <- lm(log(riqueza,base=10)~log(area,base=10))
par(mfrow=c(2,2))
plot(modelo2)
par(mfrow=c(1,1))
plot(area, riqueza, xlab="Log Area (ha)", ylab="Log N?mero de Esp?cies", log="xy")
abline(modelo2)


# O que este comando faz?
## Fim!

#Operaçao com vetores

a <- 1:10
b <- -(1:10) #- fora do parenteses para números negativos
sum(a+b)


#Criando sequencias

a <- seq(from=0, to=1, length = 9)
b <- seq(from=0, to=0.5, length = 9)
a/b
b/a
1+b/a
(1+b)/a

#Criando sequencias repetidas 

a <- rep( c(1,2), each=3 )
b <- rep( c(3,4), 6)
a 
b 

a+b

#Calculando média
pesos <- c(78.4, 79.8, 76.0, 75.3, 77.4, 78.6, 77.9, 78.8, 79.2, 75.2, 75.0, 79.4)
pesos

pesos.dif <- diff(pesos) #diff dá a diferença entre o valor atual e o próximo
pesos.dif

length(pesos)
length(pesos.dif) #retorna um vetor de comprimento "n-1"

max(pesos)
min(pesos)
mean(pesos)

range(pesos) #retorna o valor mínimo e o máximo

#Mëdia sem a funcao mean - se da pela formula
sum(pesos)/length(pesos)

#Valores faltantes ou nao numericos
#calcular a média de log na base 10 de pesos
mean(log(pesos.dif, base=10))

#NaN - valores nao numericos, o R retornou o erro NaNs produzidos pq havia log de numero repetido

pesos.dif
log(pesos.dif, base=10)
(falta.um <- c(rep(10,9),NA))
mean(falta.um)
sqrt(-1:9)
mean(sqrt(-1:9), na.rm=T)

mean(falta.um, na.rm=TRUE) #na.rm desconsidera os valores de Nan
var(sqrt(-1:9), na.rm=TRUE)

#Indice de Shannon

abund <- c(13, 23, 29, 29, 30, 48, 72, 76, 83, 84, 86, 97,102, 229, 343)
tot <- sum(abund)
tot
pi <- abund/tot
pi
log.pi <- log(pi)
log.pi
pi.log.pi <- pi*log.pi
pi.log.pi
H <- - sum(pi.log.pi)
H

#Todo este calculo pode ser resumido a uma linha
-sum( abund/sum(abund) * log( abund/sum(abund) ) )

#criamos um pi no workspace (global environment) que faz com q apareca apenas o nosso
#pi, para buscar o da geometria na base -> base::pi
search()
pi
base::pi

#amostra normal
normal.1 <- rnorm(10,10,2.5)
normal.1

#Calculo de minimo e maximo de valores desta amplitude
range(normal.1)
diff( range(normal.1) ) #A diferenca entre a max e a min. 

#com amostras maiores
diff( range ( rnorm(100, mean = 10, sd = 0.5) ) )
diff( range ( rnorm(1000, mean = 10, sd = 0.5) ) )
diff( range ( rnorm(10000, mean = 10, sd = 0.5) ) )


#QUI-quadrado - sem formula, na unha
disponivel <- c(60,28,9,2.5,0.5)
consumido <- c(544,285,117,54,12)
tot.itens <- sum(consumido)
esperado <- tot.itens*disponivel/100
desvio <- consumido - esperado
desvio

d.quad <- desvio^2/esperado
qui2 <- sum(d.quad)
qui2
pchisq(q=qui2, df=4, lower.tail=FALSE)

## Faz o grafico da funcao Qui-quadrado com 4 graus de liberdade,
## veja ajuda funcao curve
curve(dchisq(x, df=4),0,70, xlab="Qui-quadrado, 4 g.l.", ylab="Densidade probabilística")
## Sobrepoe uma linha vermelha a partir 
##do valor calculado do Qui-quadrado
curve(dchisq(x, df=4), 56.93, 70, add=T, col="red", lwd=2)

#Arredontamento
ceiling( 4.3478 )
floor( 4.3478 )
round( 4.3478 )
round( 4.3478 , digits=3)
round( 4.3478 , digits=2)

factorial( 4 )         # Fatorial de 4
choose(10,3)          # Coeficientes binomiais: combinação de 10 3-a-3

##Exercicio apostila - 2.1 Estimadores de Pollard
n <- 30
S <- 2531.754 #a soma do quadrado das distâncias de cada árvore ao centro de seu quadrante

den <- (4*(4*n-1))/(pi*S)
den
var <- den/(4*n-2)
var


#Exercicio 2.2 - fustes

DAP <- 13.5
Per <- pi*(DAP/2)^2
Per
DAP1 <- 7
Per1 <- pi*(DAP1/2)^2
Per1
pi*(12/2)^2
DAP2 <- 9
Per2 <- pi*(DAP2/2)^2
Per2
DAP3 <- 12
Per3 <- pi*(DAP3/2)^2
Per3

pi*(7/2)^2 + pi*(9/2)^2 + pi*(12/2)^2
fustes <- sum(Per1,Per2,Per3)
fustes
#Exercicio 2.3 - revisitado
#Se uma árvore possui três fustes com DAPs de: 7cm, 9cm e 12cm, 
#qual o diâmetro (único) que é equivalente à sua área transversal? 
raio <- sqrt(fustes/pi)
raio
diauni <- raio*2
diauni
per=pi*raio^2
per


## Exercicio 2.4 - Biomassa das árvores

est.1=exp(-1.7953)*15^2.2974
est.1

est.2 <- -2.6464+1.9960*log(15)+0.7558*log(12)
exp(est.2)


est.1 <- exp(-1.7953)*15^2.2974
##Segunda estimativa
log.est.2 <- -2.6464+1.9960*log(15)+0.7558*log(12)
## Comparacao
est.1
exp(log.est.2)

#Exercicio 2.7
#Enumerar FUstes

dap <- c(5, 6, 7, 5, 10, 11, 6, 8, 9, 7)
dap
dap.index <- 1:length(dap) #Copiei da resposta
names.dap <- dap.index
dap

#2.8 - diversos fustes

dap <- c(5, 6, 7, 5, 10, 11, 6, 8, 9, 7)
at<- pi*(dap/2)^2
med <- sum(dap)/length(dap)
med
vari= (length(dap)*sum(dap^2)-sum(dap)^2)/(length(dap)*(length(dap)-1))
vari

#2.9 - Bits e Bytes

2^(1:10)

#2.10 - COntas de Luz
Rel=c(9839,	10149,	10486,	10746,	11264,	11684,	12082,	12599,	13004,	13350,	13717,	14052)
Rel
names <- c('jan','fev','mar','abr','mai','jun','jul','ago','set','out','nov','dez')

luz <- data.frame(Rel,names)
luz

cons <- diff(Rel)
range(cons)
cons
mean(cons)
median(cons)
var(cons)

#2.11 - quais argumentos das funcpes
#args retorna os argumentos da funcao
args(ls)

args(mean)
args(cumsum)
args(range)
args(sd)

#2.12 - argumentos das funcoes
args(sort)
args(log)
args(seq)

#2.13 - Amplitude normal (100, 1000, 100000) - tende a aumentar

diff(range(rnorm(100,10,5)))
diff(range(rnorm(1000,10,5)))
diff(range(rnorm(10000,10,5)))

#2.14 - Intervalo normal
#Normal padronizada - media 0 e desvio 1
#2.15 Normal entre 1.96 e -1.96

pnorm(1.96,0,1)*100

##AULA 3 MANIPULACAO DE DADOS
caixeta = read.table("caixeta.csv", header=T, sep=",", as.is=T)
str(caixeta)
class(caixeta)
dim(caixeta)
names(caixeta)
str(caixeta)
head(caixeta)

spp <- factor(caixeta$especie)

unique(spp)

(caixeta.local <- table(caixeta$especie, caixeta$local))
str(caixeta.local)
dimnames(caixeta.local)
caixeta.local [,"chauas"]

caixeta.mat=matrix(caixeta.local,ncol=3)
colnames(caixeta.mat)<-colnames(caixeta.local)
rownames(caixeta.mat)<-rownames(caixeta.local)
caixeta.mat
str(caixeta.mat)
table(caixeta.local==caixeta.mat)


caixeta.vf=caixeta.mat>0 #adiona teste lógico, vai responder com verdadeiro ou falso
caixeta.vf
riqueza=apply(caixeta.vf,2,sum) #adiciona a mtriz, depois indica linha ou coluna, depois a funçao
riqueza

##INDEXAR e MANIPULAR

chauas = caixeta.mat[,"chauas"]
which(chauas>0)
chauas.bin <- chauas
chauas.bin[which(chauas>0)]<-1 #transformando em presença e ausencia
chauas.bin

str(caixeta)
nome.spp <- unique(caixeta$especie)
nome.local <- unique(caixeta$local)
caixeta[caixeta$especie==nome.spp[1],]
caixeta[caixeta$local==nome.local[2],]
caixeta[caixeta$especie==nome.spp[1] & caixeta$local==nome.local[2],]

caixeta[caixeta$especie==nome.spp[1] & caixeta$local==nome.local[1],]

###testes de variancia

var.2=function(x)(var(x)*(length(x)-1))/length(x)
valores = rnorm(10000, mean=1, sd=2)
valores.m = matrix(valores,ncol=1000)
##Estimador com correção
variancias1 = apply(valores.m, 2, var) 
## Estimador sem correção
variancias2 = apply(valores.m, 2, var.2)
mean(variancias1)
mean(variancias2)


#Operaçao com matriz
mat.trans <- matrix(c(0.43,0.33,0,0,0.61,0.3,0.56,0,0.96),3,3)
mat.trans

pop.inicio <- c(50,25,10)
names(pop.inicio)<-c("plântula", "jovem", "adulto")
pop.inicio


pop.1 <- mat.trans%*%pop.inicio
pop.1
pop.2 <- mat.trans%*%pop.1
pop.2
pop.3 <- mat.trans%*%pop.2
pop.3
pop <- data.frame(t0=pop.inicio, t1=pop.1, t2=pop.2, t3=pop.3)
pop  

matplot(x=0:3, t(pop), type="l", xlab="tempo", ylab="numero de indivíduos")
legend("topright", legend=names(pop.inicio), bty="n", lty=1:3, col=1:3)

eigen(mat.trans)

distan <- c(3949,3000,3927,1273,3188,1827)
nome <- c("Atenas a Madri",'Atenas a Paris', 'Atenas a Estocolmo','Madri a Paris', 'Madri a Estocolmo', 'Paris a Estocolmo')
table(distan,nome)

distanm <- matrix(distan,1,6)
distanm
colnames(distanm) <- nome
rownames(distanm) <- "Distancia(KM)"

library(datasets)
eurodist


