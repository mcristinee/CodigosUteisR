#5.1 Editando alguns parâmetros gráficos
dev.off()
#Leia os dados Conjunto de Dados: 
#Biomassa de Árvores de Eucalyptus saligna em um objeto chamado saligna
dir()
saligna <- read.table("esaligna.csv",header=T,sep=",", as.is=T)
head(saligna)

#Use o simbolo de círculo preenchido aumentado em 40%, na cor vermelha - 
#Legendas dos eixos com nomes das variáveis e suas unidades da seguinte forma:
#x: Diâmetro à altura do peito (cm)
#y: Altura (m)
#Tamanho das fontes e legendas dos eixos com 50% e 70% de aumento que o padrão,

#Marcações da escala dos eixos (ticks marks) para dentro da 
#área do gráfico com tamanho de 0.3 do tamanho da linha.
#Valores do eixo y na horizontal
#Apenas as linhas referentes/ aos eixos x e y (formato “L”)
#Título contendo: *Eucalypthus saligna* com aumento de 80%
#Salve o gráfico no formato png no arquivo saligna.png no diretório de trabalho atual.


#png("saligna%02d.png", width = 1024,height = 600,pointsize = 20)
plot(saligna$dap,saligna$ht,cex.axis=1.5,cex.lab=1.7,las=1,bty="l", tcl=0.3,
     main="Eucalypthus saligna", cex.main=1.8, xlab="",ylab="",pch=16,cex=1.4,col="red")
mtext("Altura (m)", 2, line = 2.7,cex=1.7)
mtext("Diametro a altura do peito (cm)", 1, line = 2.7,cex=1.7)
#savePlot(filename="saligna.png",device = "2" ) -> nao funcionaou no mac, 
#por isso usei o png da funçao


#5.2 Dois gráficos juntos

##Use as variáveis “dap” e “talhao” para construir dois gráficos, colocando-os lado a lado. 


par(mfrow=c(2,2),cex.axis=1.5,cex.lab=1.7,las=1,bty="l", tcl=0.3,cex=1.4,mar=c(5.1,6.1,4.1,2.1))
#O primeiro deve ser um boxplot da variável “dap” em função do fator “talhão”. 

boxplot(saligna$dap~saligna$talhao, ylab="", xlab="")
mtext("Talhao",1, line = 2.7,cex=1.7)
mtext("Diametro a altura \n do peito (cm)", 2, line = 2.7,cex=1.7, las=0)
text(1,22,"a")

#O segundo deve ter apenas a média e uma barra de desvio-padrão do dap, 
#para cada talhão.
med <- aggregate(saligna$dap~saligna$talhao,FUN=function(x) c(mean(x),sd(x)))
med
media <- c(10.36,12.98,11.63,15.38,13.58,13.15)
dp <- c(3.73,6.02,4.26,5.48,5.15,6.28)
plot(media, pch=16, ylim=c(0,21),xlab="",ylab="")
par(new=T)
plot(media+dp, bty="n",ann=F, xaxt="n", yaxt='n', ylab="", xlab="", ylim=c(0,21),pch="_")
par(new=T)
plot(media-dp, bty="n", xaxt="n", yaxt="n",ylab='', xlab='',ylim=c(0,21),pch="_")
segments(x0=c(1, 2, 3, 4, 5, 6),y0=media-dp, y1=media+dp)
mtext("Talhao",1, line = 2.7,cex=1.7)
mtext("Diametro a altura \n do peito (cm)", 2, line = 2.7,cex=1.7, las=0)
text(1.5,20,"b")

dev.off()
#Mantenha o mesmo padrão de formatação do exercício anterior.
#Insira também uma letra para dizer qual é o gráfico “a” e qual é o “b”
# (tanto faz, quem é um e quem é outro).

chal=read.table("5_3.csv", header = T,sep=",",as.is=T)
chal

par(mfrow=c(1,2),las=1,bty="l", tcl=0.3,mar=c(5.1,5.1,4.1,2.1))
plot(y1~x1, data=chal,pch=17,col='black', xlab="", ylab="",
     cex.axis=1.1,cex.lab=1.1,las=1,bty="l", tcl=0.3,cex=1.4,
     mar=c(5.1,5.1,4.1,2.1))
model=lm(y1~x1, data=chal)
abline(model)

mtext("Euclidean Distances",2, line = 2.7,cex=1.7,las=0)
mtext("Log(Patch size)(ha)", 1, line = 2.7,cex=1.7, las=0)
text(2.5,2.7,'a')

boxplot(x2~y2, data=chal, ylab="", xaxt='n',xlab='', outline=T, ylim=c(0,3.5))
axis(side=1, at=c(1,2,3,4,5,6),labels=c("","","","","",""))
mtext(c("Small","Medium\nEdge","Medium\nInterior","Large\nEdge","Large\nInterior","Control"),at=c(1,2,3,4,5,6),side=1,line=1.7)
text(1,3.5,"*");text(2,3.5,"*");text(3,3.5,"**")
text(4,3.5,"*");text(5,3.5,"***");text(6,3.5,"b")

#Nao Salvei o Ultimo, uso mac entao nao consegui salvar de modo satisfatorio. 
#No RStudio, uso o salvar como na tela de zoom.



