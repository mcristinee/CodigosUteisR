

Neste exercício, use o conjunto de dados Inventário em Florestas Plantadas de E
ucalyptus grandis.
dir()
EG <- read.table("egrandis.csv", header=T, sep=";")

EG
boxplot(EG$dap~EG$rotacao*EG$regiao, outline=F)

#Utilize o gráfico boxplot para analisar o DAP de árvores de E. grandis em 
#função das variáveis região (regiao) e rotação (rotacao).

#Avalie a normalidade da altura do conjunto total de árvores com um 
#gráfico quantil-quantil contra a distribuição normal.

qqnorm(EG$ht)
qqline(EG$ht)

hist(EG$ht)
