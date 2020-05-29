### Exercicios 1 - REAMOSTRAGEM
rm(list=ls())
dir()
eutad <- read.table('palmadulto.txt', header=T, sep="\t")
eutad

dist=matrix(NA, ncol=102, nrow=102)
for(i in 1:101)
{
  for(j in (i+1):102)
  {
    difx2=(eutad$gx[i]-eutad$gx[j])^2
    dify2=(eutad$gy[i]-eutad$gy[j])^2
    dist[i,j]<-sqrt(difx2 + dify2)
    dist[j,i]<-sqrt(difx2 + dify2)
  }
  
}
dist[1:3,5]
str(dist[1,1])
(nn<-apply(dist, 1, min, na.rm=TRUE))
(mnn<-round(mean(nn),1))

resultado <- rep(NA,1000)
resultado[1] <- mnn
resultado[1:5]

for(m in 2:1000)
{
  xsim=runif(102,0,302)
  ysim=runif(102,0,302)
  dist.sim = matrix(NA, ncol = 102, nrow = 102)
  for(l in 1:101)
  {
    for(k in (l+1):102)
    {
      difx2=(xsim[l]-xsim[k])^2
      dify2=(ysim[l]-ysim[k])^2
      dist.sim[l,k]=sqrt(difx2 + dify2)
      dist.sim[k,l]=sqrt(difx2 + dify2)
    }
    
  }
  resultado[m]= round(mean(apply(dist.sim, 1, min, na.rm=TRUE)),1)
  
} 
resultado
hist(resultado)
abline(v=resultado[1], lty=5, col='red')


unicaudal <- sum(resultado>=resultado[1])
pu <- unicaudal/length(resultado)
pu




#tem 82% de chance de um resultado igual ou maior ser obtido por causa da aleatoriedade.
#logo, podemos dizer que a distribuicao dos palmitos é aletória


#EXercicio 2

animais <- read.table('animais.txt', header=T, sep=";", dec=',', as.is = T)
animais

B1 <- coef(lm(log(animais$brain)~log(animais$body), na.action = na.omit))[2]
nsim <- 1000
B.sim <- rep(NA,nsim)
B.sim[1] <- round(B1,1)
B.sim[1:5]
for(i in 2:nsim){
  sim.brain <- sample(log(animais$brain), replace=T)
  Rl <- lm(sim.brain~animais$body)
  B.sim[i] <- coef(Rl)[2]
  hist(B.sim)
  abline(v=B.sim[1], lty=5, col='red')
}
B.sim

hist(B.sim)
abline(v=B.sim[1], lty=5, col='red')
unicaudal <- sum(abs(B.sim)>=abs(B.sim[1]))
pu <- unicaudal/length(B.sim)
pu

#A chance de esta relacao demonstrada na regressao, ser "obra do acas"é praticamente nula. 

