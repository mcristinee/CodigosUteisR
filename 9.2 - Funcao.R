##FUNCAO
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

