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

