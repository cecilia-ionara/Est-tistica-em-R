# Carregando os pacotes necessários
library(carData)
library(ggplot2)

# Usando o conjunto de dados Duncan do pacote carData
data("Duncan")

#visualizando
carData::Duncan

# Criando o gráfico de dispersão para verificar se há correlação linear entre prestígio e salário
ggplot(Duncan, aes(x = prestige, y = income)) +
  geom_point(color = "blue") +
  ggtitle("Diagrama de Dispersão - Prestígio vs Salário") +
  xlab("Prestígio") + ylab("Salário (milhares de dólares)")

# Verificando a correlação entre prestígio e salário
correlacao <- cor(Duncan$prestige, Duncan$income)
print(paste("Correlação entre prestígio e salário: ", correlacao))

# Aplicando a regressão linear
modelo_regressao <- lm(income ~ prestige, data = Duncan)

# Extraindo os coeficientes da regressão linear
coeficientes <- coef(modelo_regressao)
print(paste("Intercepto: ", coeficientes[1]))
print(paste("Coeficiente angular: ", coeficientes[2]))

# Criando o gráfico de dispersão com a linha de regressão
ggplot(Duncan, aes(x = prestige, y = income)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", col = "red") +
  ggtitle("Diagrama de Dispersão com Regressão Linear - Prestígio vs Salário") +
  xlab("Prestígio") + ylab("Salário (milhares de dólares)")

# Fazendo uma predição para uma ocupação com prestígio de 150
prestigio_predicao <- 150
predicao <- predict(modelo_regressao, newdata = data.frame(prestige = prestigio_predicao))

print(paste("Predição do salário para uma ocupação com prestígio de", prestigio_predicao, ":", predicao, "milhares de dólares"))

