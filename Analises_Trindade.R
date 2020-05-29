library(readxl)
library(sciplot)
library(ggbiplot)

#install.packages('devtools')
#library(devtools)
#install_github("vqv/ggbiplot")

####APENAS ILHA
#Analises preliminares metais Trindade
setwd('/Volumes/HD/OneDrive/Documentos/PPGOFQG/Projeto/Analises')
ilha = read_excel("Analises_ICP_Final.xlsx", sheet = "Apenas Ilha")
i.num = ilha[,(15:22)]
ponto.i=factor(ilha$`Local da Coleta`)
ic.i=factor(ilha$Local)
id.i=factor(ilha$`Especie e Genero`)
fam.i=factor(ilha$Familia)
oco.i=factor(ilha$Lado)
ocor.i=factor(oco.i,levels=c('Leeward','Windward'))
estacao.i=factor(ilha$Estacao)
grupo.i=factor(ilha$Grupo)
rep.i=ilha$numeroamostra
NS.i=factor(ilha$NS)
diver=ilha$Riqueza
cd.i=ilha$Cd
pb.i=ilha$Pb
zn.i=ilha$Zn
cu.i=ilha$Cu
as.i=ilha$As
hg.i=ilha$Hg
i.num
PCAI=prcomp(log(i.num+.1), scale=TRUE)

ggbiplot(PCAI, obs.scale = .5, var.scale = .5,groups =ilha$NS, ellipse = TRUE, varname.size=5, varname.adjust=2)+
 scale_color_manual(values = c('blue','orange'),name = 'Sampled sites')+
  #geom_point(aes(x=PCA1$x, y=PCA1$rotation) Data= PCA1, size=2))+
  theme(legend.direction = 'vertical',legend.position = 'right',legend.text = element_text(size = 10, colour = "black"),legend.key = element_rect(fill = "white", colour='white'),legend.title = element_text(face = "bold"))+
  theme(axis.text = element_text(colour = "black", size=10),axis.title.y = element_text(size = rel(1.3), angle = 90),axis.title.x = element_text(size = rel(1.3), angle = 0))+
  theme(panel.background = element_rect(fill='white', colour="grey"))+
  theme(panel.border = element_rect(linetype="solid", fill = NA))+
  theme(panel.grid.major = element_line(colour="grey", linetype = "dashed"))
ggsave("PCA.eps")

postscript("bargraph.eps")

par(mfrow=c(2,3),las=0,cex.lab=1.5,cex.axis=1.4,mar=c(5,5,4,2))
bargraph.CI(NS.i, zn.i,grupo.i,lc=F, main="Zn", ylab=expression(µg.g^{-1}), col=c('Ivory','Burlywood2'),
            ylim=c(0,50)); abline(h=0)+
text(1.51,45.80,"A");text(2.49,45.80,"A");text(4.49,20.15,"B");text(5.5,20.15,"B")
bargraph.CI(NS.i, cu.i,grupo.i,lc=F, main="Cu", ylab=expression(µg.g^{-1}), col=c('Ivory','Burlywood2'),
            ylim=c(0,25)); abline(h=0)+
text(1.51,23.80,"A");text(2.49,23.80,"A");text(4.49,8.15,"B");text(5.5,8.15,"B")
bargraph.CI(NS.i, pb.i,grupo.i,lc=F, main="Pb", ylab=expression(µg.g^{-1}), col=c('Ivory','Burlywood2'),
            ylim=c(0,3)); abline(h=0)+
text(1.51,2.80,"A");text(2.49,2.80,"A");text(4.49,1.15,"B");text(5.5,1.15,"B")
bargraph.CI(NS.i, as.i,grupo.i,lc=F, main="As", ylab=expression(µg.g^{-1}), col=c('Ivory','Burlywood2'),
            ylim=c(0,50)); abline(h=0)+
text(3.5,42.50,"INTERACTION")
bargraph.CI(NS.i, cd.i,grupo.i,lc=F, main="Cd", ylab=expression(µg.g^{-1}), col=c('Ivory','Burlywood2'),
            ylim=c(0,0.2)); abline(h=0)
text(1.5,0.18,"A");text(2.48,0.05,"B");text(4.49,0.18,"A");text(5.5,0.05,"B")
bargraph.CI(NS.i, hg.i,grupo.i,lc=F, main="Hg", ylab=expression(ng.g^{-1}), col=c('Ivory','Burlywood2'),
            ylim=c(0,7)); abline(h=0)
legend("topright",legend=c("Phaeophyceae", "Rhodophyta"),fill=c('Ivory','Burlywood2'), bty="n",cex=1.2)
dev.off()
analise.zn=aov(log(zn.i+.1) ~ NS.i + grupo.i + NS.i*grupo.i)
summary(analise.zn)
analise.cu=aov(log(cu.i+.1) ~ factor(NS.i) + factor(grupo.i) + factor(NS.i)*factor(grupo.i))
summary(analise.cu)
analise.pb=aov(log(pb.i+.1) ~ factor(NS.i) + factor(grupo.i) + factor(NS.i)*factor(grupo.i))
summary(analise.pb)
analise.as=aov(log(as.i+.1) ~ factor(NS.i) + factor(grupo.i) + factor(NS.i)*factor(grupo.i))
summary(analise.as)
TukeyHSD(analise.as,"factor(NS.i):factor(grupo.i)")
analise.cd=aov(log(cd.i+.1) ~ factor(NS.i) + factor(grupo.i) + factor(NS.i)*factor(grupo.i))
summary(analise.cd)
analise.hg=aov(log(hg.i+.1) ~ factor(NS.i) + factor(grupo.i) + factor(NS.i)*factor(grupo.i))
summary(analise.hg)





###ILHA E CONTINENTE
#Analises preliminares metais Trindade
metais.trindade = read_excel("D:/OneDrive/Documentos/PPGOFQG/Projeto/Analises/Analises_ICP_Final.xlsx", sheet = "Estat_total")
metais.trindade
trin.m = metais.trindade[,(14:21)]
trin.me = metais.trindade [,(16:21)]
ponto=factor(metais.trindade$`Local da Coleta`)
id=factor(metais.trindade$`Especie e Genero`)
fam=factor(metais.trindade$Familia)
oco=factor(metais.trindade$Lado)
ocor=factor(oco,levels=c('Leeward','Windward','ES','PR'))
NS1=factor(metais.trindade$NS)
NS=factor(NS1,levels=c("North","South","PR", "ES"))
estacao=factor(metais.trindade$Estacao)
lat=metais.trindade$Latitude
long=metais.trindade$Longitude
grupo=factor(metais.trindade$Grupo)
rep=metais.trindade$numeroamostra
temp=metais.trindade$`T?C`
sal=metais.trindade$Sal
cd=metais.trindade$Cd
pb=metais.trindade$Pb
zn=metais.trindade$Zn
cu=metais.trindade$Cu
as=metais.trindade$As
hg=metais.trindade$Hg
cores.gru=col=c('lightgoldenrod','darksalmon')
Mor=metais.trindade$Morfotipo

par(mfrow=c(2,3))
bargraph.CI(NS,cu,metais.trindade$Morfotipo)+abline(h=0)
bargraph.CI(NS,zn,metais.trindade$Morfotipo)+abline(h=0)
bargraph.CI(NS,pb,metais.trindade$Morfotipo)+abline(h=0)
bargraph.CI(NS,as,metais.trindade$Morfotipo)+abline(h=0)
bargraph.CI(NS,cd,metais.trindade$Morfotipo)+abline(h=0)
bargraph.CI(NS,hg,metais.trindade$Morfotipo)+abline(h=0)






#PCA- DADOS METAIS e Salinidade

PCA1=prcomp(log(trin.m+.1), scale=TRUE)
PCA2=prcomp(log(trin.me+1),scale=TRUE)
#Tr <- ggbiplot(PCA1, obs.scale = 1, var.scale = 1,groups =metais.trindade$NS, ellipse = TRUE, varname.size=5, varname.adjust=2,labels = metais.trindade$Grupo,labels.size = 4)+
ggbiplot(PCA2, obs.scale = 1, var.scale = 1,groups =metais.trindade$NS, ellipse = TRUE, varname.size=5, varname.adjust=2)+
scale_color_manual(values = c('red','orange','blue','forestgreen'),name = 'Sampled sites')+
#geom_point(aes(x=PCA1$x, y=PCA1$rotation) Data= PCA1, size=2))+
theme(legend.direction = 'vertical',legend.position = 'right',legend.text = element_text(size = 10, colour = "black"),legend.key = element_rect(fill = "white", colour='white'),legend.title = element_text(face = "bold"))+
theme(axis.text = element_text(colour = "black", size=10),axis.title.y = element_text(size = rel(1.3), angle = 90),axis.title.x = element_text(size = rel(1.3), angle = 0))+
theme(panel.background = element_rect(fill='white', colour="grey"))+
theme(panel.border = element_rect(linetype="solid", fill = NA))+
theme(panel.grid.major = element_line(colour="grey", linetype = "dashed"))

ddT = read_excel("Analises_ICP_Final.xlsx", 'Estat_total',header=T)
ddN=subset(ddT,NS=='North')

for (i in 1:21)
  ponto
  var=zn[NS=="ES"]
  n=length(var)
  n
  media=mean(var)
  dp=sd(var)
  min=min(var)
  max=max(var)
  min;max;media;dp
  
  #mediana=median(var)
  #mediana
  #MAD=2*median(abs(var-median(var)))
  #MAD
   #######################
  
 # q15=quantile(var,c(.15)) #Sexto elemento
  #q15
  #vars=sort(var)
  #pos=length(which(vars<q15))
#  if ((abs(q15-vars[pos])) >= (abs(q15-vars[pos+1])))
 # {posF=pos+1} else {posF=pos }
  #mediana15=median(sort(var)[1:posF])
  #mediana15
  #MAD15=2*median(abs(sort(var)[1:posF]-median(sort(var)[1:posF])))
  #MAD15
  #cv=sd(var)/mean(var)*100
  #q85=quantile(var,c(.85))
##n;media;dp;min;max;mediana;MAD;mediana15;MAD15;cv;q85



###ANalises levando em consideracao apenas as amostras da Ilha
##Estacao X Grupos
par(mfrow=c(2,2),las=0,cex.lab=1.5,cex.axis=1.4,mar=c(5,5,4,2))
bargraph.CI(NS.i, zn.i,estacao.i,lc=F, main="Zn", ylab=expression(?g.g^{-1}), col=c('thistle1','grey81'),
            ylim=c(0,70)); abline(h=0)
bargraph.CI(NS.i,cu.i,estacao.i,lc=F, main="Cu",ylab=expression(?g.g^{-1}),col=c('thistle1','grey81'),
            ylim=c(0,10));abline(h=0)
bargraph.CI(NS.i,pb.i,estacao.i,lc=F, main="Pb",ylab=expression(?g.g^{-1}), col=c('thistle1','grey81'),
            ylim=c(0,2));abline(h=0)
bargraph.CI(NS.i,as.i,estacao.i,lc=F, main="As",ylab=expression(?g.g^{-1}), col=c('thistle1','grey81'),
            ylim=c(0,50));abline(h=0)
bargraph.CI(NS.i,cd.i,estacao.i,lc=F, main="Cd",ylab=expression(?g.g^{-1}), col=c('thistle1','grey81'),
            ylim=c(0,0.2));abline(h=0)
bargraph.CI(NS.i,hg.i,estacao.i,lc=F, main="Hg",ylab=expression(ng.g^{-1}), col=c('thistle1','grey81'),
            ylim=c(0,8));abline(h=0)
legend("topright",legend=c("Summer", "Winter"),fill=c('thistle1','grey81'), bty="n",cex=1.2)

analise.zn=aov(log(zn.i+.1) ~ NS.i + estacao.i + NS.i*estacao.i)
summary(analise.zn)
analise.cu=aov(log(cu.i+.1) ~ factor(NS.i) + factor(estacao.i) + factor(NS.i)*factor(estacao.i))
summary(analise.cu)
analise.pb=aov(log(pb.i+.1) ~ factor(NS.i) + factor(estacao.i) + factor(NS.i)*factor(estacao.i))
summary(analise.pb)
analise.as=aov(log(as.i+.1) ~ factor(NS.i) + factor(estacao.i) + factor(NS.i)*factor(estacao.i))
summary(analise.as)
TukeyHSD(analise.as,"factor(NS.i):factor(estacao.i)")
analise.cd=aov(log(cd.i+.1) ~ factor(NS.i) + factor(estacao.i) + factor(NS.i)*factor(estacao.i))
summary(analise.cd)
analise.hg=aov(log(hg+.1) ~ factor(NS.i) + factor(estacao.i) + factor(NS.i)*factor(estacao.i))
summary(analise.hg)

analise.zn1=aov(log(zn.i+.1) ~ NS.i + grupo.i + NS.i*grupo.i)
summary(analise.zn1)
analise.cu1=aov(log(cu.i+.1) ~ factor(NS.i) + factor(grupo.i) + factor(NS.i)*factor(grupo.i))
summary(analise.cu1)
analise.pb1=aov(log(pb.i+.1) ~ factor(NS.i) + factor(grupo.i) + factor(NS.i)*factor(grupo.i))
summary(analise.pb1)
analise.as1=aov(log(as.i+.1) ~ factor(NS.i) + factor(grupo.i) + factor(NS.i)*factor(grupo.i))
summary(analise.as1)
TukeyHSD(analise.as1,"factor(NS.i):factor(grupo.i)")
analise.cd1=aov(log(cd.i+.1) ~ factor(NS.i) + factor(grupo.i) + factor(NS.i)*factor(grupo.i))
summary(analise.cd1)
analise.hg1=aov(log(hg.i+.1) ~ factor(NS.i) + factor(grupo.i) + factor(NS.i)*factor(grupo.i))
summary(analise.hg1)

#Concentracoes lados da Ilha e pontos do continente
par(mfrow=c(2,3),las=0,cex.lab=2,cex.axis=2,mar=c(5,5,4,2))
bargraph.CI(NS,zn,grupo,col=cores.gru,lc=F, main="Zn",ylim=c(0,50),ylab=expression(?g.g^{-1}));abline(h=0)
text(2.00,48.80,"A");text(4.97,21.87,"B");text(7.99,33.33,"AB");text(10.91,33.46,"B")
bargraph.CI(NS,cu,grupo,col=cores.gru,lc=F, main="Cu",ylim=c(0,30),ylab=expression(?g.g^{-1}));abline(h=0)
text(2.00,23.80,"A");text(4.97,5.65,"B");text(7.99,5.42,"AB");text(10.91,5.53,"AB")
bargraph.CI(NS,pb,grupo,col=cores.gru,lc=F, main="Pb",ylim=c(0,10),ylab=expression(?g.g^{-1}));abline(h=0)
text(2.00,8.80,"Interaction")
bargraph.CI(NS,as,grupo,col=cores.gru,lc=F, main="As",ylim=c(0,200),ylab=expression(?g.g^{-1}));abline(h=0)
text(2.00,180,"Interaction")
bargraph.CI(NS,cd,grupo,col=cores.gru,lc=F, main="Cd",ylim=c(0,1.5),ylab=expression(?g.g^{-1}));abline(h=0)
text(2.07,0.39,"A");text(5.11,0.48,"A");text(8.01,1.29,"B");text(11.06,0.9,"B")
bargraph.CI(NS,hg,grupo,col=cores.gru,lc=F, main="Hg",ylim=c(0,60),ylab=expression(ng.g^{-1}));abline(h=0)
text(2.12,16.64,"A");text(5.01,16.64,"A");text(8.04,16.64,"AB");text(10.91,50.09,"B")
legend("topleft", cex=1.2,legend=c( "Brown",'Red'),fill=cores.gru, bty="n")

#Concentracoes APENAS ILHA lados da Ilha e pontos do continente
par(mfrow=c(2,3),las=0,cex.lab=1.5,cex.axis=1.4,mar=c(5,5,4,2))
bargraph.CI(NS.i,zn.i,grupo.i,col=cores.gru,lc=F, main="Zn",ylim=c(0,50),ylab=expression(?g.g^{-1}));abline(h=0)
bargraph.CI(NS.i,cu.i,grupo.i,col=cores.gru,lc=F, main="Cu",ylim=c(0,30),ylab=expression(?g.g^{-1}));abline(h=0)
bargraph.CI(NS.i,pb.i,grupo.i,col=cores.gru,lc=F, main="Pb",ylim=c(0,10),ylab=expression(?g.g^{-1}));abline(h=0)
bargraph.CI(NS.i,as.i,grupo.i,col=cores.gru,lc=F, main="As",ylim=c(0,200),ylab=expression(?g.g^{-1}));abline(h=0)
bargraph.CI(NS.i,cd.i,grupo.i,col=cores.gru,lc=F, main="Cd",ylim=c(0,1.5),ylab=expression(?g.g^{-1}));abline(h=0)
bargraph.CI(NS.i,hg.i,grupo.i,col=cores.gru,lc=F, main="Hg",ylim=c(0,60),ylab=expression(ng.g^{-1}));abline(h=0)
legend("topleft", cex=1.2,legend=c( "Brown",'Red'),fill=cores.gru, bty="n")


mean(hg)
sd(hg)

#ANOVA Localidades VS Grupos
par(mfrow=c(1,1))
anova.cu=aov(log(cu+.1)~NS*grupo)
summary(anova.cu)
TukeyHSD(anova.cu,'NS:grupo')
TukeyHSD(anova.cu,'NS')
bargraph.CI(NS,cu,grupo, col= cores.gru,lc=F, main="Cu",ylim=c(0,25),ylab=expression(?g.g^{-1}));abline(h=0)
text(1.49,4.59,"a");text(2.49,21.07,"a");text(4.48,1.92,"a");text(5.51,1.67,"a")
text(7.49,2.36,"a");text(8.49,1.92,"a");text(10.51,1.92,"a");text(11.49,3.35,"a")
text(1.93,23.80,"A");text(4.97,5.65,"B");text(7.99,5.42,"AB");text(10.91,5.53,"AB")


anova.zn=aov(log(zn+.1)~NS*grupo)
summary(anova.zn)
TukeyHSD(anova.zn,"NS:grupo")
TukeyHSD(anova.zn,'NS')
bargraph.CI(NS,zn,grupo,col=cores.gru,lc=F, main="Zn",ylim=c(0,50),ylab=expression(?g.g^{-1}));abline(h=0)
text(1.49,44.62,"a");text(2.50,32.57,"a");text(4.48,11.81,"a");text(5.51,14.04,"a")+
text(7.49,26.36,"a");text(8.50,21.00,"a");text(10.51,12.42,"a");text(11.49,25.36,"a")+
text(2.00,48.80,"A");text(4.97,21.87,"B");text(7.99,33.33,"AB");text(10.91,33.46,"B")

anova.pb=aov(log(pb+.1)~NS*grupo)
summary(anova.pb)
TukeyHSD(anova.pb,'NS:grupo')
TukeyHSD(anova.pb,'NS')
bargraph.CI(NS,pb,grupo,col=cores.gru,lc=F, main="Pb",ylim=c(0,10),ylab=expression(?g.g^{-1}));abline(h=0)
text(1.08,9.73,'C')

anova.as=aov(log(as)~NS*grupo)
summary(anova.as)
TukeyHSD(anova.as,'NS:grupo')
TukeyHSD(anova.as,'NS')
bargraph.CI(NS,as,grupo,col=cores.gru,lc=F, main="As",ylim=c(0,200),ylab=expression(?g.g^{-1}));abline(h=0)
text(1.08,190.73,'D')
###MUITOS ZEROS - DISTRIBUICAO MUITO NAO NORMAL
anova.cd=aov(log(cd+.1)~NS*grupo)
summary(anova.cd)
TukeyHSD(anova.cd,'grupo')
TukeyHSD(anova.cd,'NS')
bargraph.CI(NS,cd,grupo,col=cores.gru,lc=F, main="Cd",ylim=c(0,1.5),ylab=expression(?g.g^{-1}));abline(h=0)
text(2.07,0.39,"A");text(5.11,0.48,"A");text(8.01,1.29,"B");text(11.06,0.09,"B")
locator(4)
anova.hg=aov(log(hg+.1)~NS*grupo)
summary(anova.hg)
TukeyHSD(anova.hg,'NS:grupo')
TukeyHSD(anova.hg,'NS')
text(2.12,16.64,"A");text(5.01,15.62,"A");text(8.04,18.19,"B");text(10.91,50.09,"B")
bargraph.CI(NS,hg,grupo,col=cores.gru,lc=F, main="Hg",ylim=c(0,35),ylab=expression(?g.g^{-1}));abline(h=0)
text(1.08,34.3,'F')

summary(anova.zn)
summary(anova.pb)
summary(anova.cd)
summary(anova.cu)
summary(anova.as)
summary(anova.hg)

#salinidade e temperatura
summary(sal[estacao =='Summer'])
sd(sal[estacao=='Summer'])

summary(sal[estacao =='Winter'])
sd(sal[estacao=='Winter'])

summary(temp[estacao =='Summer'])
sd(temp[estacao =='Summer'])
summary(temp[estacao =='Winter'])
sd(temp[estacao =='Winter'])

#ESTAT B?SICA POR PONTOS

#Zinco
summary(zn[NS=='North'])
sd(zn[NS=='North'])
summary(zn[NS=='South'])
sd(zn[NS=='South'])
summary(zn[NS=='ES'])
sd(zn[NS=='ES'])
summary(zn[NS=='PR'])
sd(zn[NS=='PR'])


#Cobre
summary(cu[NS=='North'])
sd(cu[NS=='North'])
summary(cu[NS=='South'])
sd(cu[NS=='South'])
summary(cu[NS=='ES'])
sd(cu[NS=='ES'])
summary(cu[NS=='PR'])
sd(cu[NS=='PR'])

#Chumbo
summary(pb[NS=='North'])
sd(pb[NS=='North'])
summary(pb[NS=='South'])
sd(pb[NS=='South'])
summary(pb[NS=='ES'])
sd(pb[NS=='ES'])
summary(pb[NS=='PR'])
sd(pb[NS=='PR'])

#Arsenio
summary(as[NS=='North'])
sd(as[NS=='North'])
summary(as[NS=='South'])
sd(as[NS=='South'])
summary(as[NS=='ES'])
sd(as[NS=='ES'])
summary(as[NS=='PR'])
sd(as[NS=='PR'])

#Cadmio
summary(cd[NS=='North'])
sd(cd[NS=='North'])
summary(cd[NS=='South'])
sd(cd[NS=='South'])
summary(cd[NS=='ES'])
sd(cd[NS=='ES'])
summary(cd[NS=='PR'])
sd(cd[NS=='PR'])

#Mercurio
summary(hg[NS=='North'])
sd(hg[NS=='North'])
summary(hg[NS=='South'])
sd(hg[NS=='South'])
summary(hg[NS=='ES'])
sd(hg[NS=='ES'])
summary(hg[NS=='PR'])
sd(hg[NS=='PR'])


#Estatistica Basica
#zinco

summary(zn[id=='Canistrocarpus cervicornis'])
sd(zn[id=='Canistrocarpus cervicornis'])
summary(zn[id=='Dictyopteris delicatula'])
sd(zn[id=='Dictyopteris delicatula'])
summary(zn[id=='Ceratodyction sp.'])
sd(zn[id=='Ceratodyction sp.'])
summary(zn[id=='Palisada sp.'])
sd(zn[id=='Palisada sp.'])
id



#COBRE
summary(cu[id=='Canistrocarpus cervicornis'])
sd(cu[id=='Canistrocarpus cervicornis'])
summary(cu[id=='Dictyopteris delicatula'])
sd(cu[id=='Dictyopteris delicatula'])
summary(cu[id=='Ceratodyction sp.'])
sd(cu[id=='Ceratodyction sp.'])
summary(cu[id=='Palisada sp.'])
sd(cu[id=='Palisada sp.'])

#CHUMBO
summary(pb[id=='Canistrocarpus cervicornis'])
sd(pb[id=='Canistrocarpus cervicornis'])
summary(pb[id=='Dictyopteris delicatula'])
sd(pb[id=='Dictyopteris delicatula'])
summary(pb[id=='Ceratodyction sp.'])
sd(pb[id=='Ceratodyction sp.'])
summary(pb[id=='Palisada sp.'])
sd(pb[id=='Palisada sp.'])


#ARSENIO
summary(as[id=='Canistrocarpus cervicornis'])
sd(as[id=='Canistrocarpus cervicornis'])
summary(as[id=='Dictyopteris delicatula'])
sd(as[id=='Dictyopteris delicatula'])
summary(as[id=='Ceratodyction sp.'])
sd(as[id=='Ceratodyction sp.'])
summary(as[id=='Palisada sp.'])
sd(as[id=='Palisada sp.'])

#CADMIO
summary(cd[id=='Canistrocarpus cervicornis'])
sd(cd[id=='Canistrocarpus cervicornis'])
summary(cd[id=='Dictyopteris delicatula'])
sd(cd[id=='Dictyopteris delicatula'])
summary(cd[id=='Ceratodyction sp.'])
sd(cd[id=='Ceratodyction sp.'])
summary(cd[id=='Palisada sp.'])
sd(cd[id=='Palisada sp.'])

#MERCURIO
summary(hg[id=='Canistrocarpus cervicornis'])
sd(hg[id=='Canistrocarpus cervicornis'])
summary(hg[id=='Dictyopteris delicatula'])
sd(hg[id=='Dictyopteris delicatula'])
summary(hg[id=='Ceratodyction sp.'])
sd(hg[id=='Ceratodyction sp.'])
summary(hg[id=='Palisada sp.'])
sd(hg[id=='Palisada sp.'])
#Esp?cies do continente

#Zinco
summary(zn[id=='Sargassum filipendula'])
sd(zn[id=='Sargassum filipendula'])
summary(zn[id=='Sargassum sp.'])
sd(zn[id=='Sargassum sp.'])
summary(zn[id=='Sargassum vulgare var. nanum'])
sd(zn[id=='Sargassum vulgare var. nanum'])
summary(zn[id=='Gracilaria sp'])
sd(zn[id=='Gracilaria sp'])
summary(zn[id=='Padina gymnoospora'])
sd(zn[id=='Padina gymnoospora'])
summary(zn[id=='Palisada perforata'])
sd(zn[id=='Palisada perforata'])

#chumbo
summary(pb[id=='Sargassum filipendula'])
sd(pb[id=='Sargassum filipendula'])
summary(pb[id=='Sargassum sp.'])
sd(pb[id=='Sargassum sp.'])
summary(pb[id=='Sargassum vulgare var. nanum'])
sd(pb[id=='Sargassum vulgare var. nanum'])
summary(pb[id=='Gracilaria sp'])
sd(pb[id=='Gracilaria sp'])
summary(pb[id=='Padina gymnoospora'])
sd(pb[id=='Padina gymnoospora'])
summary(pb[id=='Palisada perforata'])
sd(pb[id=='Palisada perforata'])

#Arsenio
summary(as[id=='Sargassum filipendula'])
sd(as[id=='Sargassum filipendula'])
summary(as[id=='Sargassum sp.'])
sd(as[id=='Sargassum sp.'])
summary(as[id=='Sargassum vulgare var. nanum'])
sd(as[id=='Sargassum vulgare var. nanum'])
summary(as[id=='Gracilaria sp'])
sd(as[id=='Gracilaria sp'])
summary(as[id=='Padina gymnoospora'])
sd(as[id=='Padina gymnoospora'])
summary(as[id=='Palisada perforata'])
sd(as[id=='Palisada perforata'])

#Cobre
summary(cu[id=='Sargassum filipendula'])
sd(cu[id=='Sargassum filipendula'])
summary(cu[id=='Sargassum sp.'])
sd(cu[id=='Sargassum sp.'])
summary(cu[id=='Sargassum vulgare var. nanum'])
sd(cu[id=='Sargassum vulgare var. nanum'])
summary(cu[id=='Gracilaria sp'])
sd(cu[id=='Gracilaria sp'])
summary(cu[id=='Padina gymnoospora'])
sd(cu[id=='Padina gymnoospora'])
summary(cu[id=='Palisada perforata'])
sd(cu[id=='Palisada perforata'])

#Cadmio
summary(cd[id=='Sargassum filipendula'])
sd(cd[id=='Sargassum filipendula'])
summary(cd[id=='Sargassum sp.'])
sd(cd[id=='Sargassum sp.'])
summary(cd[id=='Sargassum vulgare var. nanum'])
sd(cd[id=='Sargassum vulgare var. nanum'])
summary(cd[id=='Gracilaria sp'])
sd(cd[id=='Gracilaria sp'])
summary(cd[id=='Padina gymnoospora'])
sd(cd[id=='Padina gymnoospora'])
summary(cd[id=='Palisada perforata'])
sd(cd[id=='Palisada perforata'])

#mercurio
summary(hg[id=='Sargassum filipendula'])
sd(hg[id=='Sargassum filipendula'])
summary(hg[id=='Sargassum sp.'])
sd(hg[id=='Sargassum sp.'])
summary(hg[id=='Sargassum vulgare var. nanum'])
sd(hg[id=='Sargassum vulgare var. nanum'])
summary(hg[id=='Gracilaria sp'])
sd(hg[id=='Gracilaria sp'])
summary(hg[id=='Padina gymnoospora'])
sd(hg[id=='Padina gymnoospora'])
summary(hg[id=='Palisada perforata'])
sd(hg[id=='Palisada perforata'])

summary(zn[ponto=='Portugueses'])
sd(zn[ponto=='Portugueses'])
summary(pb[ponto=='Portugueses'])
sd(pb[ponto=='Portugueses'])
summary(cu[ponto=='Portugueses'])
sd(cu[ponto=='Portugueses'])
summary(hg[ponto=='Portugueses'])
sd(hg[ponto=='Portugueses'])
summary(hg[ponto=='Orelhas'])
sd(hg[ponto=='Orelhas'])

bargraph.CI(ponto,hg)


#testes de normalidade
hist(zn)
qqnorm(zn)
qqline(zn)
shapiro.test(zn)
#Log
hist(log(zn+.1))
boxplot(log(zn+.1))
qqnorm(log(zn+.1))
qqline(log(zn+.1))
shapiro.test(log(zn+.1))
#sqrt
hist(sqrt(zn+.1))
boxplot(sqrt(zn+.1))
qqnorm(sqrt(zn+.1))
qqline(sqrt(zn+.1))
shapiro.test(sqrt(zn+.1))

#testes de normalidade
hist(cd)
qqnorm(cd)
qqline(cd)
#Log
hist(log(cd+1))
boxplot(cd)
qqnorm(log(cd+1))
qqline(log(cd+1))
#sqrt
hist(sqrt(cd))
boxplot(sqrt(cd))
qqnorm(sqrt(cd))
qqline(sqrt(cd))

#testes de normalidade
hist(cu)
qqnorm(cu)
qqline(cu)
#Log
hist(log(cu+1))
boxplot(cu)
qqnorm(log(cu+1))
qqline(log(cu+1))
#sqrt
hist(sqrt(cu))
boxplot(sqrt(cu))
qqnorm(sqrt(cu))
qqline(sqrt(cu))


#testes de normalidade
hist(as)
qqnorm(as)
qqline(as)
shapiro.test(as)
#Log
hist(log(as+.1))
boxplot(log(as+.1))
qqnorm(log(as+.1))
qqline(log(as+.1))
shapiro.test(log(as+.1))

#sqrt
hist(sqrt(as))
boxplot(sqrt(as))
qqnorm(sqrt(as))
qqline(sqrt(as))
shapiro.test(sqrt(sqrt(as)))

#testes de normalidade
hist(hg)
qqnorm(hg)
qqline(hg)
#Log
hist(log(hg+1))
boxplot(hg)
qqnorm(log(hg+1))
qqline(log(hg+1))
shapiro.test(log(hg+.1))
#sqrt
hist(sqrt(hg))
boxplot(sqrt(hg))
qqnorm(sqrt(hg))
qqline(sqrt(hg))

#testes de normalidade
hist(pb)
qqnorm(pb)
qqline(pb)
shapiro.test(pb)
#Log
hist(log(pb+.1))
boxplot(log(pb+.1))
qqnorm(log(pb+.1))
qqline(log(pb+.1))
shapiro.test(log(pb+.1))
#sqrt
hist(sqrt(pb))
boxplot(sqrt(pb))
qqnorm(sqrt(pb))
qqline(sqrt(pb))
shapiro.test(sqrt(pb))

#An?lise por Pontos da Ilha
par(mfrow=c(1,3),las=3)
bargraph.CI(ponto.i,zn.i,grupo.i, col=cores.gru,lc=F, main="Zinco",ylim=c(0,60),ylab=expression(?g.g^{-1}))
bargraph.CI(ponto.i,cu.i,grupo.i, col=cores.gru,lc=F, main="Cobre",ylim=c(0,50),ylab=expression(?g.g^{-1}))
bargraph.CI(ponto.i,pb.i,lc=F,grupo.i, col=cores.gru, main="Chumbo",ylim=c(0,4),ylab=expression(?g.g^{-1}))
bargraph.CI(ponto.i,as.i,grupo.i,lc=F, main="Arsenio",ylim=c(0,60),ylab=expression(?g.g^{-1}), col=cores.gru)
bargraph.CI(ponto.i,hg.i,grupo.i,lc=F, main="Mercurio",ylim=c(0,20),ylab=expression(ng.g^{-1}), col=cores.gru)
bargraph.CI(ponto.i,cd.i,grupo.i,lc=F, main="Cadmio",ylim=c(0,0.5),ylab=expression(?g.g^{-1}), col=cores.gru);
legend("topright", legend=c("Brown",'Red'),fill=cores.gru, bty="n")

var=as[NS=="North"]
lenght(as[NS=="North"])
mean(as[NS=="North"])
sd(as[NS=="North"])
median(as[NS=="North"])
MAD=2*median(abs(as[NS=="North"]-median(as[NS=="North"])))
MAD
q15=quantile(var,c(.15))
vars=sort(var)
if ((abs(q15-vars[pos])) >= (abs(q15-vars[pos+1])))
{posF=pos+1} else {posF=pos }

#Comentar se der erro
MAD15=2*median(abs(sort(var)[1:posF]-median(sort(var)[1:posF])))
MAD15

cv=sd(var)/mean(var)*100
cv
q85=quantile(var,c(.85))
q85


summary(cu[ponto=='Portugueses'])
summary(zn[ponto=='Portugueses'])
summary(pb[ponto=='Portugueses'])
