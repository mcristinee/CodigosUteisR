library(MASS)
data("Animals")
str(Animals)

anim.m2 <- lm(log(brain)~log(body),data=Animals, 
              subset=!(log(Animals$body)>8&log(Animals$brain)<6))
anim.m0 <- lm(log(brain)~1, data=Animals, 
              subset=!(log(Animals$body)>8&log(Animals$brain)<6))
anova(anim.m2, anim.m0)

#qual a diferença entre a anova abaixo e a anterior?
#A anova de um único modelo, retorna a partiçao de variancia enquanto a de 2 modelos retorna a particáo de variancia comparando os modelos. 

anova(anim.m2)


#QUal a relaçao dos valores obtidos por estes comandos. 
summary(anim.m0)
mean(log(Animals$brain[!(log(Animals$body)>8&log(Animals$brain)<6)]))
sd(log(Animals$brain[!(log(Animals$body)>8&log(Animals$brain)<6)]))

#summary está me mostrando os coeficientes e dados do objeto anim.m0. 
# Onde o intercept é igual a média calculada no vetor pq nao podemos calcula mean(anim.m0)
#o Erro padrao mostrado pelo summary é o mesmo que o calculado no vetor. 



