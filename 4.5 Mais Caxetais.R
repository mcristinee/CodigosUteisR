

#Aqui usaremos novamente o objeto caixeta, criado no tutorial 
#Exploração de uma Variável Categórica.

#Analise a relação dap-altura ('dap' e 'h') em função do caixetal (local) com a função plot, 
#mas somente para as árvores 2) de caixeta (Tabebuia cassinoides).

caix <- read.table("caixeta.csv", header=T, sep=",", as.is=T)
caix$dap <- (pi/4)* (caix$cap/10)


tabeuia= subset(caix, caix$especie=="Tabebuia cassinoides", select=c("arvore","h","local","dap")) 
tabeuia$local


plot(tabeuia$dap~tabeuia$h|tabeuia$local)


Para a mesma relação do item anterior, verifique linearidade com a função scatter.smooth

scatter.smooth( tabeuia$dap,tabeuia$h , col="green" )

Utilizando o pacote lattice, analise a relação dap-altura ('dap' e 'h') em função do caixetal (local),
mas somente para as árvores 3) de caixeta (Tabebuia cassinoides).
library(lattice)

xyplot(tabeuia$dap~tabeuia$h, tabeuia$local)

Dicas

Veja o argumento subsets das funções de gráficos, a função subset e função with
Para calcular o DAP de árvores de múltiplos fustes primeiro calcule as áreas basais de cada fuste, e então some-as. Em seguida, deduza desta área basal total o dap total.

