dados.esaligna <- read.table("esaligna.csv",header=T,sep=",")
str(dados.esaligna)
head(dados.esaligna)
summary(dados.esaligna)

dados.esaligna2 <- dados.esaligna
dados.esaligna2$soma <- dados.esaligna2$tronco+dados.esaligna2$folha

dados.esaligna.fin <- dados.esaligna2
dados.esaligna.fin$areabasal <- pi*((dados.esaligna.fin$dap/2)^2)
summary(dados.esaligna.fin)

dados.esaligna.10cm <- dados.esaligna.fin[dados.esaligna.fin$dap>10,]
head(dados.esaligna.10cm)
area.basal <- by(dados.esaligna.10cm$areabasal, INDICES=dados.esaligna.10cm$talhao, FUN=sum) 
head(area.basal)
area.basal
media.area.basal <- by(dados.esaligna.10cm$areabasal, INDICES=dados.esaligna.10cm$talhao, FUN=mean) 



write.table("areas.txt", sep="\t")


