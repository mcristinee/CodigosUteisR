

e.grandis <- read.table("egrandis.csv", header = T, as.is=F,sep=";")
(breviarium <- summary(e.grandis))
(cont.1 <- table(e.grandis$ano))
(cont.2 <- table(e.grandis$rotacao,e.grandis$regiao))
(bofete <- e.grandis[e.grandis$regiao=='Bofete',])
write.table(bofete,"bofete.txt",sep='\t',row.names = F)

