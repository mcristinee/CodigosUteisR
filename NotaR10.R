
nome <- c("Atenas", "Madri",'Paris', 'Estocolmo')

Atenas <- c(0,3949,3000,3927)
Madri <- c(3949,0,1273,3188)
Paris <- c(3000,1273,0,1827)
Estocolmo <- c(3927,3188,1827,0)

dist.cid <- matrix(c(Atenas,Madri,Paris,Estocolmo),4,4)

colnames(dist.cid) <- nome
rownames(dist.cid) <- nome
dist.cid
eurodist

