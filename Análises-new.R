rm(list=ls())
dev.off()
library(readxl)
library(VennDiagram)
library(sciplot)
library(scales)
library(tidyverse)
#install.packages('ggpubr')
library(ggpubr)
#devtools::install_github("riatelab/cartography")
# CRAN version
library(cartography)
library(RAM)


Trin <- read_excel("ListaOriginal.xlsx")
Trin$SUMMER[is.na(Trin$SUMMER)] <- 0
Trin$WINTER[is.na(Trin$WINTER)] <- 0
Trin$SUMMER <- as.numeric(Trin$SUMMER)
Trin$SUMMER[is.na(Trin$SUMMER)] <- 1
Trin$WINTER <- as.numeric(Trin$WINTER)
Trin$WINTER[is.na(Trin$WINTER)] <- 1

Trin$total <- as.numeric(Trin$`SEAWEED TAXA - TRINDADE ISLAND (BRAZIL)`)
Trin$total[is.na(Trin$total)] <- 1


aggregate(Trin$total~as.factor(Trin$Group), FUN=sum)

aggregate(Trin$SUMMER~Trin$Ocorrencia+Trin$Group, FUN = sum)
aggregate(Trin$WINTER~Trin$Ocorrencia+Trin$Group, FUN = sum)



Verao <- Trin$`SEAWEED TAXA - TRINDADE ISLAND (BRAZIL)`[Trin$SUMMER==1]
Inverno <- Trin$`SEAWEED TAXA - TRINDADE ISLAND (BRAZIL)`[Trin$WINTER==1]

#write.csv(Trin, file="PresVsAus.csv")
#write.table(Verao, file="listaveraoVENN",row.names = F)
#write.table(Inverno, file="listainvVENN", row.names = F)

#venn.diagram(list("Summer"= Verao,"Winter"= Inverno), resoution= 300,cat.cex=2,
 #            filename="VeraoInvernoVENN.png", fill=c("Grey10","white"), alpha=rep(0.34,2), cat.pos=c(330,30),
  #           cat.dist=0.1, height = 1448,width = 2048)
str(Trin)

##### Venn das Raras
rarever<- Trin$`SEAWEED TAXA - TRINDADE ISLAND (BRAZIL)`[Trin$SUMMER==1&Trin$Ocorrencia=="New Record"]
rareinv <- Trin$`SEAWEED TAXA - TRINDADE ISLAND (BRAZIL)`[Trin$WINTER==1&Trin$Ocorrencia=="New Record"]

#venn.diagram(list(summer= rarever,winter= rareinv), resoution= 300, height = 1448,width = 2048,cat.cex=2,
 #            filename="rare.png", fill=c("Grey10","white"), alpha=rep(0.34,2), cat.pos=c(330,30), label = T,
  #           cat.dist=0.1)




#write.table(rarever, file="NewRecVer.txt", row.names = F)
#write.table(rareinv, file="NewRecInv.txt", row.names = F)

library(RAM)
#group.venn(list(ver=rarever, inv=rareinv), label=TRUE, 
 #          fill = c("Grey10", "Grey40"),
  #         cat.pos = c(330, 30),
   #        lab.cex=0.5)


ver=aggregate(Trin$SUMMER~Trin$Ocorrencia+Trin$Group, FUN=sum)
colnames(ver) <-  c("NR","grupo","verao")
inv=aggregate(Trin$WINTER~Trin$Ocorrencia+Trin$Group, FUN=sum)
colnames(inv) <-  c("NR","grupo","inverno")
cor <- c("green","yellow","red","blue")

graf<- ggplot(ver, aes(x=ver$grupo, y=ver$verao, fill=ver$NR))
graf <- graf +  geom_bar(stat="identity")+scale_y_continuous(expand = c(0, 0), limits = c(0, 70))
graf <- graf + scale_x_discrete(limits=c("Chlorophyta","Phaeophyceae","Rhodophyta","Cyanobacteria"))
graf <- graf +  theme_classic(base_size = 15)
graf <- graf + labs(x = "", y="Richness")
graf <- graf +geom_text(aes(label=ver$verao), vjust=2, color="black",
                position = position_dodge(1), size=4)
graf <- graf + scale_fill_brewer(palette= "Greys")
graf <- graf + guides(fill=guide_legend(""))
graf <- graf+theme(legend.position = "none")
graf + theme(legend.title = element_text(size=12), 
             legend.text = element_text(size=10))


grafi<- ggplot(inv, aes(x=inv$grupo, y=inv$inverno, fill=inv$NR))
grafi <- grafi +  geom_bar(stat="identity")+scale_y_continuous(expand = c(0, 0), limits = c(0, 70))
grafi <- grafi + scale_x_discrete(limits=c("Chlorophyta","Phaeophyceae","Rhodophyta","Cyanobacteria"))
grafi <- grafi +  theme_classic(base_size = 15)
grafi <- grafi + labs(x = "", y="Richness")
grafi <- grafi +geom_text(aes(label=inv$inverno), vjust=2, color="black",
                        position = position_dodge(1), size=4)
grafi <- grafi + scale_fill_brewer(palette= "Greys")
grafi <- grafi + guides(fill = guide_legend(""))
grafi <- grafi + theme(legend.position = "bottom")
grafi + theme(legend.title = element_text(size=12), 
             legend.text = element_text(size=10))


png("Richness.png", width = 2028, 1448, res = 200)
ggarrange(graf,grafi,labels=c("Cruise S","Cruise W"),
          ncol=2,nrow=1, common.legend = T, legend = "bottom", 
          hjust=-1.2 )
dev.off()

getwd()

