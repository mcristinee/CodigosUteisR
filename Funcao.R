# Índices

diversidade <- matrix(rnorm(100,8,3.5),10,10)
diversidade

shannon <- function(x,rmNA=T){
  sum(x/length(x))*log(x/length(x))
}
shannon(diversidade)
a <- sum(diversidade/length(diversidade))
b <- (diversidade/length(diversidade))
b


simpson <- function(x,rmNA=T){
  sum((x/length(x))^2)
}
simpson(diversidade)
