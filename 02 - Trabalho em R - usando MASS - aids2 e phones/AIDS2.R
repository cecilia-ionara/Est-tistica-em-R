# Instalando os pacotes necessários
install.packages("MASS")
install.packages("ggplot2")

# Carregando os pacotes
library(MASS)
library(ggplot2)

# Usando o conjunto de dados Aids2 do pacote MASS
data("Aids2")

# O conjunto de dados 'Aids2' está contendo informações sobre pacientes diagnosticados com AIDS na Austrália. 
# informações: paciente com variáveis como sexo, idade, status de sobrevivência e categoria de transmissão.

# Calculando as Medidas de Tendência Central para a idade dos pacientes
media_idade <- mean(Aids2$age, na.rm = TRUE)  # Calculando a Média
mediana_idade <- median(Aids2$age, na.rm = TRUE)  # Calculando a Mediana
moda_idade <- as.numeric(names(sort(table(Aids2$age), decreasing = TRUE))[1])  # Determinando a Moda

# Calculando as Medidas de Dispersão para a idade dos pacientes
variancia_idade <- var(Aids2$age, na.rm = TRUE)  # Calculando a Variância
desvio_padrao_idade <- sd(Aids2$age, na.rm = TRUE)  # Calculando o Desvio Padrão

# Gerando a Tabela de Frequência para o status dos pacientes
tabela_frequencia_status <- table(Aids2$status)  # Contando os vivos e mortos

# Criando os Gráficos
# Criando o Histograma da Idade dos Pacientes
ggplot(Aids2, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  ggtitle("Histograma da Idade dos Pacientes com AIDS") +
  xlab("Idade") + ylab("Frequência")

# Criando o Gráfico de Pizza para o Status dos Pacientes
ggplot(Aids2, aes(x = "", fill = status)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  ggtitle("Gráfico de Pizza - Status dos Pacientes com AIDS") +
  labs(fill = "Status")

# Criando o Diagrama de Dispersão entre Idade e Status
ggplot(Aids2, aes(x = age, y = status)) +
  geom_point(color = "red") +
  ggtitle("Diagrama de Dispersão - Idade vs Status dos Pacientes")

# Criando o Gráfico de Barras para o Status dos Pacientes
ggplot(Aids2, aes(x = status)) +
  geom_bar(fill = "green") +
  ggtitle("Gráfico de Barras - Contagem de Status dos Pacientes")

