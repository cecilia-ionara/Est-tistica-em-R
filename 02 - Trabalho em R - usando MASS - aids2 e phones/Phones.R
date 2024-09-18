# Carregando o pacote 
library(MASS)
library(ggplot2)

# Usando o conjunto de dados phones do pacote MASS
data("phones")

# Convertendo o conjunto de dados em um data.frame e ajustando o ano
phones_df <- data.frame(year = 1950 + phones$year, calls = phones$calls)

# Criando o gráfico de dispersão para verificar se há correlação linear
ggplot(phones_df, aes(x = year, y = calls)) +
  geom_point(color = "blue") +
  ggtitle("Diagrama de Dispersão - Ano vs Número de Chamadas") +
  xlab("Ano") + ylab("Número de Chamadas (milhões)")

# Verificando a correlação entre o ano e o número de chamadas
correlacao <- cor(phones_df$year, phones_df$calls)
print(paste("Correlação entre ano e número de chamadas: ", correlacao))

# Aplicando a regressão linear
modelo_regressao <- lm(calls ~ year, data = phones_df)

# Extraindo os coeficientes da regressão linear
coeficientes <- coef(modelo_regressao)
print(paste("Intercepto: ", coeficientes[1]))
print(paste("Coeficiente angular: ", coeficientes[2]))

# Criando o gráfico de dispersão com a linha de regressão
ggplot(phones_df, aes(x = year, y = calls)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", col = "red") +
  ggtitle("Diagrama de Dispersão com Regressão Linear - Ano vs Número de Chamadas") +
  xlab("Ano") + ylab("Número de Chamadas (milhões)")

# Fazendo uma predição para o ano de 1975 (um ano fora do intervalo dos dados)
ano_predicao <- 1975
predicao <- predict(modelo_regressao, newdata = data.frame(year = ano_predicao))

print(paste("Predição do número de chamadas para o ano de", ano_predicao, ": ", predicao, "milhões"))

