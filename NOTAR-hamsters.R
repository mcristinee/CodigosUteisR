dieta <- rep(c("A","B","C"),each=3,times=2)
cor <- rep(c("claro","escuro"),each=9)
pesos <- c(0.1,1.1,3.7,5.7,-1.2,-1.5,3.0,-0.4,0.6,1.5,-0.1,2.0,0.6,-3.0,-0.3,-0.2,0.3,1.5)
hamsters <- data.frame(dieta,cor,pesos,row.names = paste("hamster",seq(1,18)))
str(hamsters)

sum(hamsters$pesos)

media <- mean(hamsters$pesos)
(media.por.cor <- tapply(hamsters$pesos,INDEX=hamsters$cor,FUN=mean))
(media.por.dieta <- tapply(hamsters$pesos,INDEX=hamsters$dieta,FUN=mean))
media.cruzada <- aggregate(hamsters$pesos,by=list(hamsters$dieta,hamsters$cor), FUN=mean)

