heights <- c(36, 42, 43, 40, 50)
mean(heihts) #Média das alturas

edit(Cars93)
#Média da potencia
Horsepower.USA <- Cars93$Horsepower[Cars93$Origin == "USA"]
mean(Horsepower.USA)

#Usando a funcao with para calcular media 
with(Cars93, mean(Horsepower[Origin =="USA"]))
with(Cards93, mean(Horsepower[Origin == "USA" & Cylinders -- 4]))

#Histograma com dois gráficos
ggplot(Cards93, aes(x=Horsepower)) + geom_histogram(color="black", fill="white", binwidth = 10) +facet_wrap(~Origin)
mean(Horsepower.USA, trim = .05)

