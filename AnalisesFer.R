#### - Análises FER
library(readxl)
library(sciplot)
rm(list=ls())


mang <- read_excel("/Volumes/HD/Onedrive/Documentos/ARTIGOS/Manguezais - FER/manguezal.xlsx", 
                   sheet = 'ESTAT')
head(mang)
point <- as.factor(mang$Local)
str(point)
point <- factor(point, levels = c("ROCIO","Valadares","Antonina","Ilha do Mel"))
mangR <- read_excel("/Volumes/HD/Onedrive/Documentos/ARTIGOS/Manguezais - FER/manguezal.xlsx", 
                    sheet = 'Riqueza')
head(mangR)
rich <- aggregate(mangR$Richness~mangR$Season+mangR$Group, FUN=sum)
colnames(rich) <- c("Season","Group","Richness")

png("Manguezais%01d.png",width=2000,height = 1600,pointsize = 10,res = 300)
par(cex.axis=1,cex.lab=1.2,las=1,bty= "l", tcl=-0.2,cex=1.3,mar=c(4,4,3,2), pch=16, family="serif")

bargraph.CI(point,mang$Coverage,as.factor(mang$Season),ylab="", 
            lc=F,xlab="",xaxt="n", ylim=c(0,100),err.width = 0.05)
axis(side=1, at=c(3,8,13,18), labels=c("","","",""))
mtext(c("Paranagua Harbor","Ilha dos Valadares","Antonina","Ilha do Mel"),at=c(3,8,13,18),side=1,line=0.5,cex=1.2)
mtext("Coverage", side=2, line=2.5,las=0, cex=1.2)
mtext("Sampling Points", side=1, line=2,las=0, cex=1.2)
legend(16,100,legend=c("Winter","Autumn","Spring","Summer"), col=c("grey20","grey40","grey60","grey80"), pch=16,cex=1,bty = "n")

bargraph.CI(mangR$Local,mangR$Richness,as.factor(mangR$Season),ylab="",lc=F,xlab="",xaxt="n", ylim=c(0,40),err.width = 0.05)
axis(side=1, at=c(3,8,13,18), labels=c("","","",""))
mtext(c("Paranagua Harbor","Ilha dos Valadares","Antonina","Ilha do Mel"),at=c(3,8,13,18),side=1,line=0.5,cex=1.2)
mtext("Richness", side=2, line=2.5,las=0, cex=1.2)
mtext("Sampling Points", side=1, line=2,las=0, cex=1.2)
legend(16,40,legend=c("Winter","Autumn","Spring","Summer"), col=c("grey20","grey40","grey60","grey80"), pch=16,cex=1,bty = "n")

bargraph.CI(as.factor(rich$Season),rich$Richness,as.factor(rich$Group),ylab="",lc=F,uc=F,xaxt="n",xlab="", ylim=c(0,40),err.width = 0.05)
axis(side=1, at=c(2.5,6.5,10.5,14.5), labels=c("","","",""))
mtext(c("Autumn","Spring","Summer","Winter"),at=c(2.5,6.5,10.5,14.5),side=1,line=0.5,cex=1.2)
mtext("Richness", side=2, line=2.5,las=0, cex=1.2)
mtext("Sampling Points", side=1, line=2,las=0, cex=1.2)
legend(1,40,legend=c("Chlorophyta","Cyanophyta","Rhodophyta"), col=c("grey20","grey50","grey80"), pch=16,cex=1,bty = "n")


dev.off()

#Coverage
anv <- aov(mang$Coverage~as.factor(mang$Local)*as.factor(mang$Season))
summary(anv)  
TukeyHSD(anv)  

#Richness

anv1 <- aov(mangR$Richness~as.factor(mangR$Local)*as.factor(mangR$Season))
summary(anv1)  
TukeyHSD(anv1)  

###### MDS e PERMANOVA
if(!require(vegan)){install.packages('vegan'); library(vegan)}
if(!require(labdsv)){install.packages('labdsv'); library(labdsv)}
if(!require(clustsig)){install.packages('clustsig'); library(clustsig)}
if(!require(pvclust)){install.packages('pvclust'); library(pvclust)}
if(!require(cluster)){install.packages('cluster'); library(cluster)}
if(!require(stats)){install.packages('stats'); library(stats)}
if(!require(dplyr)){install.packages('dplyr'); library(dplyr)}
if(!require(ggplot2)){install.packages('ggplot2'); library(ggplot2)}

## Upload the file (Species Matrix) ##

XX <- read_excel("manguezal.xlsx", sheet = "Perma")


head(XX)

spe <- XX[,-c(1:2)]

head(spe)

str(spe)
## Data Transformation ##
spe_hell <- decostand(spe, method = "hell")

##  Compute dissimilarity matrix ##
# Quantify pairwise compositional dissimilarity between sites based on species occurances.
# - Bray-Curtis dissimilarity (abundance weighted)
# - Jaccard (presence/absence)
# - Gower’s (non-continuous variables)
spe_euc <- vegdist(spe_hell, method="jaccard")
# To see more ?decostand or ?vegdist respectively.

## Hierarchical cluster analysis ##
# There are other methods to clustering. To see more ?hclust
complet <- hclust(spe_euc, method="complete")
UPGMA <- hclust(spe_euc, method="average")
Ward <- hclust(spe_euc, method="ward.D")
UPGMC <- hclust(spe_euc, method="centroid")

## Select the best clutering by Cophenetic Correlation Coefficient (CCC)
cofenetic <- data.frame(Metodo=c("Complet","UPGMA","Ward","UPGMC"),
                        CCC=c(cor(spe_euc,cophenetic(complet)),
                              cor(spe_euc,cophenetic(UPGMA)),
                              cor(spe_euc,cophenetic(Ward)),
                              cor(spe_euc,cophenetic(UPGMC))))
cofenetic
## Bootstrap ##
# SIMPROF
(spe_simprof <- simprof(spe_euc, num.expected=1000, 
                        num.simulated=999, method.cluster="average", 
                        method.distance="euclidean", alpha=0.05))

quartz(title="SIMPROF Cluster Chord - UPGMA",9,6)
simprof.plot(spe_simprof,
             leafcolors=rainbow(length(spe_simprof$significantclusters)))

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
#CLUSTER

matriz=vegdist(x = spe,'jaccard', binary = T)
jaccard=betadiver(spe, "j")
sorensen=betadiver(spe,"sor")

library(magrittr)
library(stringr)
season <- tolower(XX$Season)
rotulo <- paste(XX$Local,season)

J=hclust(matriz, method = 'average' ) 
plot(J, hang=-1,labels = rotulo,ylim=c(-1,1),
     xlab = 'Amostras Macroalgas', ylab = 'Dissimilaridade')
abline(h=.5,col='red')


dev.off()
###     ###     ###     ###     ###     ###     ###     ###     ###     ###

## Upload the file (Environmental matrix)##
site <-as.factor(XX$Local)
## Transform or standardize data ##
spe.log <- decostand(spe, "log")

## Calculate ecological resemblance ##
spe.jac <- vegdist(spe.log, method='jaccard')

## PERMANOVA: Permutational multivariate analysis of variance ##
# perMANOVA
# strata = ‘exchangeable units’ for permutation. Important for nested design
spe.div<-adonis2(spe.jac~site+season, data=site, permutations = 9999, method="jaccard", strata="Region")
spe.div

# Multivariate dispersion
dispersion<-betadisper(spe.jac, group=site)
permutest(dispersion)
plot(dispersion, hull=FALSE, ellipse=TRUE)

# NMDS
MDS<-metaMDS(spe.log, distance="euclidean", k=2, trymax=36, autotransform=TRUE) # k is the number of dimensions
MDS # metaMDS takes eaither a distance matrix or your community matrix (then requires method for 'distance=')
stressplot(MDS)
#pull points from MDS
NMDS1 <- MDS$points[,1]
NMDS2 <- MDS$points[,2]

plot(NMDS1~NMDS2)
spe.plot<-cbind(spe, NMDS1, NMDS2, site)

#plot ordination
fit<-envfit(MDS, spe.log)
arrow<-data.frame(fit$vectors$arrows,R = fit$vectors$r, P = fit$vectors$pvals)
arrow$MOTU <- rownames(arrow)
arrow.p<-arrow %>% filter(arrow$P <= 0.01)

q<-ggplot(data=spe.plot, aes(NMDS1, NMDS2))+
  geom_text(data=spe.plot, aes(NMDS1, NMDS2, label=""),position=position_jitter(.1))+##separates overlapping points
  stat_ellipse(aes(fill=site), alpha=.2,type='t',level=0.91,size=1, geom="polygon")+ ##changes shading on ellipses
  theme_minimal()+
  scale_y_reverse()+
  scale_x_reverse()+
  geom_point(data=spe.plot, aes(NMDS1,NMDS2))+
  #geom_segment(data=arrow.p, aes(x=0, y=0, xend=NMDS1, yend=NMDS2, label=MOTU, lty=MOTU), arrow=arrow(length=unit(.2, "cm")*arrow.p$R), size=1)+ ##add arrows (scaled by R-squared value)
  theme(panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank())
q

