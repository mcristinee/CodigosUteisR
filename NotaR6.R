#2.10 - COntas de Luz


luz=c(9839,	10149,	10486,	10746,	11264,	11684,	12082,	12599,	13004,	13350,	13717,	14052)
luz.cons <- diff(luz)
luz.m <- mean(luz.cons)
luz.md <- median(luz.cons)
luz.v <- var(luz.cons)
luz.range <- range(luz.cons)
