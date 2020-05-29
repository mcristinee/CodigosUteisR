#103.3 Criando uma matriz

(matriz.normal <- matrix(rnorm(15,10,sqrt(3.6)),3,5))
(nomeslinhas <- paste(rep("L"),seq(1:3)))

(rownames(matriz.normal) <- c('L1','L2',"L3"))
(colnames(matriz.normal) <- c("C1","C2","C3","C4","C5"))
matriz.normal
(media <- apply(matriz.normal,1,FUN=mean))
(var <- apply(matriz.normal,1,FUN=var))
linha <- data.frame(media,var)
colnames(linha) <- c('media','var')
linha

(med <- apply(matriz.normal,2,FUN=mean))
(vari <- apply(matriz.normal,2,FUN=var))
coluna <- data.frame(med,vari)
colnames(coluna) <- c('media','var')
coluna

