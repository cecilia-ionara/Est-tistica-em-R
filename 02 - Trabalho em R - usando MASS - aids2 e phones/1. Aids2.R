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

#FREQUENCIA 

# Extraindo o maior e menor valor do conjunto de dados de idade
v_idade <- range(Aids2$age)

# Definindo o número de intervalos (5 intervalos)
classe_idade <- 5

# Calculando a amplitude dos intervalos
amplitude_idade <- (v_idade[2] - v_idade[1]) / classe_idade
amplitude_idade <- ceiling(amplitude_idade)  # Arredondando para o valor superior

# Definindo os intervalos de idade
intervalo_idade <- seq(v_idade[1], amplitude_idade * classe_idade + v_idade[1], by = amplitude_idade)

# Classificando os valores de idade dentro dos intervalos
idade_cut <- cut(Aids2$age, intervalo_idade, right = FALSE)

# Criando a tabela de frequências cruzada entre idade e status (dead/alive)
idade_status_freq <- table(idade_cut, Aids2$status)

# Exibindo a tabela de frequências
print(idade_status_freq)

# Transformando a tabela em um data frame para facilitar a manipulação
tabela_final_idade <- as.data.frame(idade_status_freq)

# Renomeando as colunas para melhor visualização
colnames(tabela_final_idade) <- c("Intervalo", "Status", "Frequencia")

# Visualizando a tabela final
edit(tabela_final_idade)


