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
