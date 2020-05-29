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

load("letras.rdata")

pares <- c(2,4,6,8)
impares <- c(1,3,5,7,9)
todos.os.numeros <- c(pares,impares)
# O que este comando faz?
## Fim!
