#4.2 CERVEJAS
cervejas <-c("chope","lata","garrafa","chope","garrafa", "garrafa","lata","lata","nenhuma","lata","garrafa","garrafa", "garrafa","lata","lata","lata","garrafa","lata","chope","nenhuma", "garrafa","garrafa","garrafa","chope","garrafa","garrafa","chope","garrafa","lata","lata")


#Represente este resultado como um gráfico de barras e um dotplot (função dotchart).

  
cer <- table(cervejas)
barplot(cer)
dotchart(cer)

#Qual tem maior razão dado/tinta?
  #O melhor para visualizaçao dos dados com pouco gasto de tinta é o dotchart


#4.3 - Caixetas
caix <- read.table("caixeta.csv", header=T, sep=",")
head(caix)  
caix$dap = (pi/4)* (caix$cap/10)
hist( caix$dap )
hist( caix$dap[ caix$local == "chauas" ] )
quartz()
hist( caix$dap[ caix$local == "jureia" ] )
quartz()
hist( caix$dap[ caix$local == "retiro" ] )

#Sim, há diferencá entre a frequencia tambem na largura das classes.  
#Chauas apresenta maior frequencia na primeira classe e nao na segunda como Jureia e Retiro.


#4.4 Boxplot
#Neste exercício, use o conjunto de dados Inventário em Florestas Plantadas de 
#Eucalyptus grandis.
dir()
EG <- read.table("egrandis.csv", header=T, sep=";")
graphics.off()
#Utilize o gráfico boxplot para analisar o DAP de árvores de E. grandis em 
#função das variáveis região (regiao) e rotação (rotacao).

boxplot(EG$dap~EG$rotacao*EG$regiao, outline=F)

#Avalie a normalidade da altura do conjunto total de árvores com um 
#gráfico quantil-quantil contra a distribuição normal.

qqnorm(EG$ht)
qqline(EG$ht)
#Dados nao normais que podem ser tambem analisados por histograma

hist(EG$ht)

#4.5 MAIS CAIXETA

#Aqui usaremos novamente o objeto caixeta, criado no tutorial 
#Exploração de uma Variável Categórica.

#Analise a relação dap-altura ('dap' e 'h') em função do caixetal (local) com a função plot, 
#mas somente para as árvores 2) de caixeta (Tabebuia cassinoides).

caix <- read.table("caixeta.csv", header=T, sep=",", as.is=T)
caix$dap <- (pi/4)* (caix$cap/10)


tabeuia= subset(caix, caix$especie=="Tabebuia cassinoides", select=c("arvore","h","local","dap")) 
tabeuia$local
retiro <- subset(tabeuia, tabeuia$local=='retiro')
chauas <- subset(tabeuia, tabeuia$local=='chauas')
jureia <- subset(tabeuia, tabeuia$local=='jureia')
par(mfrow=c(1,3))
plot(retiro$dap~retiro$h, ylab="DAP", xlab="Altura")
plot(jureia$dap~jureia$h,ylab="DAP", xlab="Altura")
plot(chauas$dap~chauas$h,ylab="DAP", xlab="Altura")

#tambem pode ser visualizado no xyplot do lattice
library(lattice)
xyplot(tabeuia$dap~tabeuia$h|tabeuia$local,ylab="DAP", xlab="Altura")

#Para a mesma relação do item anterior, verifique linearidade com a função scatter.smooth

scatter.smooth(retiro$dap~retiro$h, ylab="DAP", xlab="Altura", col="blue")
scatter.smooth(jureia$dap~jureia$h,ylab="DAP", xlab="Altura", col='red')
scatter.smooth(chauas$dap~chauas$h,ylab="DAP", xlab="Altura", col='brown')


#Utilizando o pacote lattice, analise a relação dap-altura ('dap' e 'h') em função do caixetal (local),
#mas somente para as árvores 3) de caixeta (Tabebuia cassinoides).

library(lattice)
xyplot(tabeuia$dap~tabeuia$h|tabeuia$local,ylab="DAP", xlab="Altura")

