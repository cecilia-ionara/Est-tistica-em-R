# Instalando os pacotes necessários
install.packages("MASS")
install.packages("ggplot2")

# Carregando os pacotes
library(MASS)
library(ggplot2)

#Vendo os dados totais
MASS::Aids2

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
edit(Aids2)

# Verificar os status únicos
unique_status <- sort(unique(Aids2$status))
unique_status

# Contar a frequência dos status
status.freq <- table(Aids2$status)

# Calcular a amplitude das classes
classe <- length(status.freq)  # Número de classes (status)
v <- range(as.numeric(status.freq))  # Extrai o maior e menor valor do conjunto de dados

# Amplitude das classes
amplitude <- (v[2] - v[1]) / classe
amplitude <- ceiling(amplitude)  # Arredonda para o maior valor

# Criar intervalos
intervalo <- seq(v[1], v[2] + amplitude, by = amplitude)

# Cortar os status em intervalos (se necessário)
status.cut <- cut(as.numeric(status.freq), breaks = intervalo, right = FALSE)

# Contar as frequências dos cortes
status.freq.cut <- table(status.cut)

# Criar a tabela final
tabela_final <- data.frame(Status = names(status.freq), Frequencia = as.vector(status.freq))

# Exibir a tabela final
print(tabela_final)

# Criando os Gráficos
# Criando o Histograma da Idade dos Pacientes
ggplot(Aids2, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  ggtitle("Histograma da Idade dos Pacientes com AIDS") +
  xlab("Idade") + ylab("Frequência")

#A: Vivo (Alive)
#D: Morto (Dead)
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

