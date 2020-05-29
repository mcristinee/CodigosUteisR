plot.nn <- function(x0,y0,graphic=TRUE,my.par=c(mar=c(4,4,2,2),xlim=range(x0),ylim=range(y0)))
{
  if(class(x0)!="numeric" | class(y0)!="numeric") #premissa 1
  {
    stop("o vetor de dados não é numérico")
  }
  if (length(x0)!=length(y0)) #premissa 2
  {
    stop("os vetores x e y não possuem o mesmo tamanho")
  }
  xy <- data.frame(x0,y0) #gerando data frame com o dado de entrada (coordenadas dos pontos)
  dist.mat <- as.matrix(dist(xy,upper=TRUE)) #gerando a matriz de distâncias de todos os pontos para todos os pontos
  diag(dist.mat) <- NA #NAs atribuídos à diagonal para evitar zeros, que seriam computados como a menor distância
  nn.pos <- apply(dist.mat,1,which.min) #guardando posição do vizinho mais próximo. na matriz de distâncias, vou procurar, por linha, qual a posição (e não o valor) do vizinho mais próximo no data frame (qual a linha do data frame)
  xy$x1 <- x0[nn.pos] # criando nova coluna no data frame para o x1, e indexando o valor de x do vizinho mais próximo
  xy$y1 <- y0[nn.pos] # criando nova coluna no data frame para o y1, e indexando o valor de y do vizinho mais próximo
  if(graphic)
  {
    par(my.par)
    plot(x0,y0)
    segments(x0=xy$x0,y0=xy$y0,x1=xy$x1,y1=xy$y1) #desenhando segmentos
  }
  return(list(dist=dist.mat,xy=xy))
} 

x <- rnorm(100,4,2)
y <- rnorm(100,8,1)




par(pch=19, col='blue')
plot.nn(x,y,graphic=T)
