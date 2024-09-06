#Regressão Linear

#edit(USPop)

variavel1 <- c(29.7,29.7,31.4,31.8,27.6)
variavel2 <- c(175.3,177.8,185.4,175.3,172.7)

dados.frame <- data.frame(variavel1,variavel2)

#Calcular a correlação

cor(variavel1,variavel2)

#Fazer o gráfico de dispersão
ggplot(dados.frame, aes(x=variavel1, y=variavel2)) + geom_point()

dados.reg <- lm(variavel1 ~ variavel2, data = dados.frame)

summary(dados.reg)

coefficients(dados.reg)

ggplot(dados.frame, aes(x=variavel2, y=variavel1)) + geom_point() + geom_smooth(method = lm)

predict(dados.reg, data.frame(variavel2=c(175.3,185.5)))





