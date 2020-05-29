
moeda <- c("cara","coroa")
moedaamostra <- sample(moeda,1000,replace=T)
(contagem <- table(moedaamostra))

moedaamostra2 <- sample(moeda,50,replace=T)
contagem2 <- table(moedaamostra2)

moedaamostra3 <- sample(moeda,100000,replace=T)
contagem3 <- table(moedaamostra3)


porcentagem <- contagem/length(moedaamostra)*100
porcentagem2 <- contagem2/length(moedaamostra2)*100
porcentagem3 <- contagem3/length(moedaamostra3)*100

porisso <- 100000
