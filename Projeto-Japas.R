## PORPHYRA
rm(list=ls())

vitc=23.78*10/1000 #mg para 10gr transformado
cd=0.12*10/1000
ca=300*10/1000
Fe=84*10/1000
Mg=626.4*10/1000

IDEvitc=45
IDEca=1000
IDEFe=14
IDEMg=260
IDEcd=0.024

a <- data.frame(
vitc/IDEvitc*100,
ca/IDEca*100,
Fe/IDEFe*100,
Mg/IDEMg*100,
cd/IDEcd*100
  )
a

