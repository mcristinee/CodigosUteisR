#rm(list=ls())
esaligna <- read.table("esaligna.csv",header=T,sep=",", as.is=T)


plot(esaligna$dap,esaligna$ht,cex.axis=1.5,cex.lab=1.7,las=1,bty="l", tcl=0.3,
     main="Eucalypthus saligna", cex.main=1.8, xlab="",ylab="",pch=16,cex=1.4,col="red")
mtext("Altura (m)", 2, line = 2.7,cex=1.7)
mtext("Diametro a altura do peito (cm)", 1, line = 2.7,cex=1.7)

