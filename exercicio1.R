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

#### MODIFICADO COMO SOLICITADO NO TUTORIAL
area <- c(300, 350, 961, 295, 332, 47,  122, 11, 53, 2749)
riqueza <- c(1, 7, 20, 7, 8, 4, 8, 3, 5, 23)
ls()
source("tutorial 1.r", echo=T, print.eval = T)

getwd() # Verificar pasta de trabalho
dir()  #verificar arquivos dentro do diretrório
setwd() #Indicar o caminho (mudar) a pasta de trabalho do workspace
ls() #verificar o workspace (No RSTUDIO corresponde ao Environment)

load("letras.rdata")
letras.rdata

ls()
save.image()

pares <- c(2,4,6,8)
impares <- c(1,3,5,7,9)
todos.os.numeros <- c(pares,impares)

ls()
save.image()

##CRIANDO OS OBJETOS

a <- 1
b <- a

b <- a <- 1

a =1 ->b

###Listar e mover objetos
A1 <- c(1,2,3)
A2 <- c(10,20,30)
b <- c(A1,A2)
ls()

ls(pattern="A") #pattern funciona como a funçao "grep" no bash

a.1 <- A1
a.2 <- A2
ls()
rm( list=c("A1","A2") ) ## podemos utilizar list=pattern ou apenas rm(<nome1>,<nome2>)
ls()

#Trabalhando com datas - intessante para contagem de dias por ex.
copa.70 <- "21/06/70"
copa.94 <- "17/07/94" 

copa.94 - copa.70  #nao funciona pq esta como character e nao date. tem q mudar com factor

class(copa.70)
class(copa.94)

copa.70 <- as.Date(copa.70,format="%d/%m/%y")
copa.94 <- as.Date(copa.94,format="%d/%m/%y")
class(copa.70)
class(copa.94)
copa.94 - copa.70


#trabalhando com fatores e niveis

herb <- c("A","M","M","A","A","M","M","B","A","A","A","A","B","A")

herb.f <- factor(herb)
herb.f #indica quantos níveis, mas nao conta as uniddades em cada nível.

(herb.t <- table(herb.f)) #usa table para fazer a contagem
plot(herb.t) #faz um gráfico, porem os fatores nao estoa ordenados. tem que criar levels

herb.f <- factor(herb, levels=c("N","B","M","A"))
herb.t <- table(herb.f)
herb.t
plot(herb.t) #Nao entendi a existencia da classe N

#PACOTES
###para carregar pacotes usamos library(), para saber quais estao instalados search
search()


x1 <- rnorm(n=15, mean=1, sd=3)
hist(x1)

library(MASS)
truehist(x1)













