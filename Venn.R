#install.packages('VennDiagram')

library(VennDiagram)
sp1 <- rep(c("A",'a','b','c'),5)
sp2 <- rep(c("d",'e','f','g'),5)
sp3 <- rep(c("A",'b','f','g'),5)
sp4 <- rep(c("a",'c','d','e'),5)

venn.diagram(list("1"=sp1,"2"=sp2,"3"=sp3,"4"=sp4),cat.cex=3,filename="teste.png", fill=c(1,2,3,4), alpha=rep(0.4,4))
