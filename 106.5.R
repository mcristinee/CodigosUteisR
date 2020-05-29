#106.1 Crie seus dados

conj1 <- rnorm(10,6,3)
conj2 <- rnorm(10,7.5,3.2)
source("simula.r")
sim.dif <- simula(conj1,conj2)
difer <- abs(mean(conj1)-mean(conj2))
sim.maior <- simula(conj2,conj1,teste = 'uni')
sim.maior 
t.dif <- t.test(conj2,conj1)
t.maior <- t.test(conj2,conj1,alternative = "greater")
