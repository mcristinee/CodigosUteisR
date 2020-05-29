#Graus Celsius

Fa <- round(rnorm(30,90 , 9),1)
Fa

conversor <- function(x){ 
  celsius <- 5/9*((x)-32)
  data.frame(x,celsius)
}

conversor(Fa)



