#autores: Amanda, Laura

#importação de bibliotecas necessárias
install.packages("arules")
install.packages("arulesSequences")
install.packages("readr")
install.packages("visNetwork")
install.packages("igraph")
install.packages("stringr")
install.packages("dplyr")

library(arules)
library(arulesSequences)
library(readr)
library(visNetwork)
library(igraph)
library(stringr)
library(dplyr)

#importação da base de dados
empregados <- data.frame(read.csv(file="survey.csv", header=TRUE, sep=","))

View(empregados)

#tratamento dos dados

#gerando o eventId com a coluna de Timestamp
empregados$eventID <- str_replace_all(empregados$Timestamp, c("\\s"="", "\\:"="", "-"="")) 

#gerando o id da linha
colnames(empregados)[colnames(empregados) == 'Timestamp'] <- 'sequenceID'
empregados <- empregados %>% mutate(sequenceID = row_number())

#tirando os espaços das strings
empregados$Country <- str_replace_all(empregados$Country, c("\\s"="_", "\\."="_", "-"="_"))
empregados$benefits <- str_replace_all(empregados$benefits, c("\\s"="_", "\\."="_", "-"="_"))
empregados$care_options <- str_replace_all(empregados$care_options, c("\\s"="_", "\\."="_", "-"="_"))
empregados$wellness_program <- str_replace_all(empregados$wellness_program, c("\\s"="_", "\\."="_", "-"="_"))
empregados$seek_help <- str_replace_all(empregados$seek_help, c("\\s"="_", "\\."="_", "-"="_"))
empregados$anonymity <- str_replace_all(empregados$anonymity, c("\\s"="_", "\\."="_", "-"="_"))
empregados$leave <- str_replace_all(empregados$leave, c("\\s"="_", "\\."="_", "-"="_"))
empregados$coworkers <- str_replace_all(empregados$coworkers, c("\\s"="_", "\\."="_", "-"="_"))
empregados$supervisor <- str_replace_all(empregados$supervisor, c("\\s"="_", "\\."="_", "-"="_"))
empregados$mental_vs_physical <- str_replace_all(empregados$mental_vs_physical, c("\\s"="_", "\\."="_", "-"="_"))
empregados$obs_consequence <- str_replace_all(empregados$obs_consequence, c("\\s"="_", "\\."="_", "-"="_"))
empregados$comments <- str_replace_all(empregados$comments, c("\\s"="_", "\\."="_", "-"="_"))

empregados$no_employees <- str_replace_all(empregados$no_employees, c("\\s"="_"))

View(empregados)

#tratando o formato do genero
empregados$Gender <- str_replace_all(toupper(empregados$Gender),c("FEMALE"="F", "MALE"="M"))

empregados$size = 1

write_delim( 
  empregados %>% select( c(sequenceID, eventID, size, Age)) ,
  delim ="\t", path = "player_transactions.txt", col_names = FALSE
)

#iniciando a busca por relações
empregT <- read_baskets("survey.txt", sep = "[ \t]+",info =  c("sequenceID","eventID","size") )

summary(empregT)

empregSeq <- cspade(
  empregT, 
  parameter = list(support = 0.00010, maxlen=3), 
  control   = list(verbose = TRUE)
)

summary(empregSeq)

#trabalhando na visualização
seqResult = as(empregSeq, "data.frame")
seqResult = seqResult %>% mutate(
    sequence = as.character(sequence)
  )
