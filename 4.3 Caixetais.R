


caix <- read.table("caixeta.csv", header=T, sep=",")
head(caix)  
caix$dap = (pi/4)* (caix$cap/10)
hist( caix$dap )
hist( caix$dap[ caix$local == "chauas" ] )
x11()
hist( caix$dap[ caix$local == "jureia" ] )
hist( caix$dap[ caix$local == "retiro" ] )

#Sim, há diferencá entre a frequencia tambem na largura das classes.  
#Chauas apresenta maior frequencia na primeira classe e nao na segunda como Jureia e Retiro.