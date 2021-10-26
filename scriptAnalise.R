#autores: Amanda, Laura

#importação de bibliotecas necessárias
install.packages("arules")
install.packages("arulesSequences")
install.packages("readr")


library(arules)
library(arulesSequences)
library(readr)

#importação da base de dados
empregados <- data.frame(read.csv(file="survey.csv", header=TRUE, sep=","))

View(empregados)

#tratamento dos dados
write_delim( 
  empregados,
  delim =",", file = "survey.txt", col_names = FALSE
)

#busca por relações

