#RIOS

data(rivers)
riversm <- mean(rivers)
prop.mm <- sum(rivers<riversm)/length(rivers)


quantil.75 <-  quantile(rivers,0.75)
quantil.75

a <- mean(rivers)
b <- mean(rivers,trim=0.25)
c <- median(rivers)
medias=c(a,b,c)
summary(rivers)

