aves <- read.table("aves_cerrado.csv", header = T, sep=";")
aves
A<- aves[is.na(aves$urubu)|is.na(aves$carcara)|is.na(aves$seriema),]
aves$seriema[is.na(aves$seriema)] <- 0
aves$urubu[is.na(aves$urubu)] <- 0
aves$carcara[is.na(aves$carcara)] <- 0
table(aves$fisionomia)
aves$fisionomia[aves$fisionomia=="ce"] <- "Ce"
summary(aves)


aves.ce <- subset(aves, aves$fisionomia=="Ce", select = c('carcara', 'seriema'))
aves.cc <- subset(aves, aves$fisionomia=="CC", select = c('carcara', 'seriema'))
aves.cl <- subset(aves, aves$fisionomia=="CL", select = c('carcara', 'seriema'))

str(aves.ce)
str(aves.cc)
str(aves.cl)

mod.ce <- lm(aves.ce$seriema~aves.ce$carcara)
mod.cc <- lm(aves.cc$seriema~aves.cc$carcara)
mod.cl <- lm(aves.cl$seriema~aves.cl$carcara)
summary(mod.ce)
summary(mod.cc)
summary(mod.cl)

p.ce <- summary(mod.ce)$coefficients[2,4]
p.cc <- summary(mod.cc)$coefficients[2,4]
p.cl <- summary(mod.cl)$coefficients[2,4]

coef.ce <- coef(mod.ce)
coef.cc <- coef(mod.cc)
coef.cl <- coef(mod.cl)

