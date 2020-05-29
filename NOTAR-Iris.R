
class(iris)
class(iris3)
iris

(mean.iris <- aggregate(iris[,1:4],by=list(iris$Species),FUN=mean))
(mean.iris3 <- apply(iris3,MARGIN=c(2,3),FUN=mean))

(rownames(mean.iris3) <- c("comprimento_sepala", "largura_sepala", "comprimento_petala", "largura_petala"))
mean.iris3
