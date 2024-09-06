#Váriaveis Velocidade de lançamento, ângulo de lançamento, altura e resultados.
 x1 <- c(14.12, 13.64, 13.76, 13.54, 13.69, 13.63, 13.62, 13.63,13.51,13.49,13.27,13.15,13.24,13.14,13.43,13.31)
 x2 <- c(37.3, 39.4, 37.6, 43.0, 36.1,35.2,35.8,36.5,35.0,36.4,36.6,35.9,35.6,39.4,30.9,30.7)
 x3 <- c(2.11, 2.35, 2.19, 2.34, 2.19,2.31,2.23,2.22,2.40,2.17,2.12,2.36,2.11,2.08,2.00,2.16)
 y  <- c(22.31,21.44,21.44,21.37,21.15,21.06,21.03,20.99,20.82,20.69,19.90,19.93,19.84,19.78,19.34,19.11)



# Dataframe
dados <- data.frame(x1, x2, x3, y)

# Modelo de regressão
modelo <- lm(y ~ x1 + x2 + x3, data = dados)

# modelo
summary(modelo)

# Obtendo a equação de regressão
coeficientes <- coef(modelo)
equacao <- paste("y =", round(coeficientes[1], 2), "+", round(coeficientes[2], 2), "* x1 +", 
                 round(coeficientes[3], 2), "* x2 +", round(coeficientes[4], 2), "* x3")

# Gráficos de dispersão
plot(dados$x1, dados$y, main="Gráfico de Dispersão: y vs x1", xlab="x1", ylab="y", pch=19, col="blue")
abline(lm(y ~ x1, data = dados), col="red")
legend("topright", legend="Linha de Regressão", col="red", lwd=2)

plot(dados$x2, dados$y, main="Gráfico de Dispersão: y vs x2", xlab="x2", ylab="y", pch=19, col="darkgreen")
abline(lm(y ~ x2, data = dados), col="red")
legend("topright", legend="Linha de Regressão", col="red", lwd=2)

plot(dados$x3, dados$y, main="Gráfico de Dispersão: y vs x3", xlab="x3", ylab="y", pch=19, col="orange")
abline(lm(y ~ x3, data = dados), col="red")
legend("topright", legend="Linha de Regressão", col="red", lwd=2)

