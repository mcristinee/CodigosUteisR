#Exercicio - Variancia

pesos <- c(78.4, 79.8, 76.0, 75.3, 77.4, 78.6, 77.9, 78.8, 79.2, 75.2, 75.0, 79.4)

mean(pesos)
pesos.d2 <- (pesos-mean(pesos))^2
pesos.d2s <- sum(pesos.d2)
pesos.v <- pesos.d2s/(length(pesos)-1)
pesos.ds= sqrt(pesos.d2s)
(pesos.s= sqrt(pesos.v))
rm(list=ls())


                   ?write.table
