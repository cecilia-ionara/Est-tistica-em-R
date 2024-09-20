# Carregando os pacotes necessários
library(MASS)
library(ggplot2)

# Carregando o conjunto de dados Aids2
data("Aids2")

# Análise do conjunto de dados Aids2
# Visualizando os dados
print(head(Aids2))

# Calculando medidas de tendência central para a idade dos pacientes
media_idade <- mean(Aids2$age, na.rm = TRUE)  # Média
mediana_idade <- median(Aids2$age, na.rm = TRUE)  # Mediana
moda_idade <- as.numeric(names(sort(table(Aids2$age), decreasing = TRUE)[1]))  # Moda

# Calculando medidas de dispersão para a idade dos pacientes
variancia_idade <- var(Aids2$age, na.rm = TRUE)  # Variância
desvio_padrao_idade <- sd(Aids2$age, na.rm = TRUE)  # Desvio Padrão

# Exibindo as medidas calculadas
cat("Média da Idade:", media_idade, "\n")
cat("Mediana da Idade:", mediana_idade, "\n")
cat("Moda da Idade:", moda_idade, "\n")
cat("Variância da Idade:", variancia_idade, "\n")
cat("Desvio Padrão da Idade:", desvio_padrao_idade, "\n")

# Criando gráficos para o conjunto de dados Aids2
ggplot(Aids2, aes(x = status)) +
  geom_bar(fill = "green") +
  ggtitle("Gráfico de Barras - Contagem de Status dos Pacientes") +
  xlab("Status") +
  ylab("Frequência")

ggplot(Aids2, aes(x = "", fill = status)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  ggtitle("Gráfico de Pizza - Status dos Pacientes com AIDS") +
  labs(fill = "Status")

ggplot(Aids2, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  ggtitle("Histograma da Idade dos Pacientes com AIDS") +
  xlab("Idade") +
  ylab("Frequência")

ggplot(Aids2, aes(x = age, y = status)) +
  geom_point(color = "red") +
  ggtitle("Diagrama de Dispersão - Idade vs Status dos Pacientes") +
  xlab("Idade") +
  ylab("Status")

# Análise da coluna 'SG' (Gravidade Específica) do conjunto de dados petrol
# Supondo que o conjunto de dados petrol já esteja carregado
print(head(petrol$SG))  # Exibindo as primeiras linhas da coluna SG

# Ordenando os valores de SG
sg_ordenado <- sort(petrol$SG)
print(sg_ordenado)  # Exibindo os valores ordenados

# Criando o histograma sem plotar (definindo 5 intervalos)
prices <- hist(petrol$SG, plot = FALSE, breaks = 5)

# Extraindo o maior e menor valor do conjunto de dados SG
v <- range(petrol$SG)

# Calculando o número de classes (frequências)
classe <- length(prices$counts)

# Calculando a amplitude das classes
amplitude <- (v[2] - v[1]) / classe
amplitude <- ceiling(amplitude)  # Arredondando para o maior valor

# Definindo os intervalos
intervalo <- seq(v[1], amplitude * classe + v[1], by = amplitude)

# Classificando os valores de SG dentro dos intervalos
sg.cut <- cut(petrol$SG, intervalo, right = FALSE)

# Criando a tabela de frequências
sg.freq <- table(sg.cut)

# Exibindo a tabela de frequências como coluna
print(cbind(sg.freq))

# Transformando em um data frame para facilitar a manipulação
tabela.final <- data.frame(Intervalo = names(sg.freq), Frequencia = as.vector(sg.freq))

# Visualizando a tabela final
edit(tabela.final)
