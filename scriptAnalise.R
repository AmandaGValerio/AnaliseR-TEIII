#autores: Amanda, Laura

#importação de bibliotecas necessárias
install.packages("arules")
install.packages("arulesSequences")


library(arules)
library(arulesSequences)

#importação da base de dados
empregados <- data.frame(read.csv(file="survey.csv", header=TRUE, sep=","))

#tratamento dos dados

#busca por relações
