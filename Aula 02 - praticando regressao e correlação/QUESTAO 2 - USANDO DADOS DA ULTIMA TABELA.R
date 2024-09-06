#Regressão Linear

#edit(USPop)

#Váriaveis Velocidade de lançamento, ângulo de lançamento, altura e resultados.
variavel1 <- c(14.12, 13.64, 13.76, 13.54, 13.69, 13.63, 13.62, 13.63,13.51,13.49,13.27,13.15,13.24,13.14,13.43,13.31)
variavel2  <- c(22.31,21.44,21.44,21.37,21.15,21.06,21.03,20.99,20.82,20.69,19.90,19.93,19.84,19.78,19.34,19.11)

dados.frame <- data.frame(variavel1,variavel2)

#Calcular a correlação

cor(variavel1,variavel2)

#Fazer o gráfico de dispersão
ggplot(dados.frame, aes(x=variavel1, y=variavel2)) + geom_point()

dados.reg <- lm(variavel1 ~ variavel2, data = dados.frame)

summary(dados.reg)

coefficients(dados.reg)

ggplot(dados.frame, aes(x=variavel2, y=variavel1)) + geom_point() + geom_smooth(method = lm)


#Regressão de Exponencial 




