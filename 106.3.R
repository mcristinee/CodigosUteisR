##106.3
mudas <- read.table("altura-mudas.csv", header=T, sep=',')
mudas

tamboril <- subset(mudas,mudas$especie=="tamboril", select=c('especie', 'bloco','substrato','altura'))
tamboril

aov.tamboril <- aov(tamboril$altura~as.factor(tamboril$substrato))

summary(aov.tamboril)


