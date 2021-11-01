#autores: Amanda, Edson, Laura

#importação de bibliotecas necessárias
install.packages("arules")
install.packages("arulesViz")
install.packages("arulesSequences")
install.packages("readr")
install.packages("visNetwork")
install.packages("igraph")
install.packages("stringr")
install.packages("dplyr")

library(arules)
library(arulesViz)
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

#excluindo colunas desnecessárias
empregados$comments <- NULL
empregados$state <- NULL

#gerando o eventId com a coluna de Timestamp
#empregados$Timestamp <- str_replace_all(empregados$Timestamp, c("\\s"="", "\\:"="", "-"="")) 
#empregados$eventID <- empregados$Timestamp
#troquei a geração do eventID para corrigir o bug de factor
empregados <- empregados %>% mutate(eventID = row_number())

#gerando o id da linha
colnames(empregados)[colnames(empregados) == 'Timestamp'] <- 'sequenceID'
empregados <- empregados %>% mutate(sequenceID = row_number())

#criando a coluna size
empregados$size = 1

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
#empregados$comments <- str_replace_all(empregados$comments, c("\\s"="_", "\\."="_", "-"="_"))

empregados$no_employees <- str_replace_all(empregados$no_employees, c("\\s"="_"))

#tratando o formato do genero
#precisa substituir os que estão diferentes, porque na survey não existe um padrão
empregados$Gender <- str_replace_all(empregados$Gender, c("\\s"="", "\\."="", "-"="","\\("="", "\\)"=""))
empregados$Gender <- str_replace_all(toupper(empregados$Gender),c("CIS"="", "TRANS"=""))
empregados$Gender <- str_replace_all(toupper(empregados$Gender),c("FEMALE"="F", "MALE"="M"))

View(empregados)

#escrevendo um novo arquivo com a parte mais importante da análise
write_delim( 
  empregados %>% select( c(sequenceID, eventID, size, remote_work)) ,
  delim =";", path = "survey.txt", col_names = FALSE
)

#lendo o arquivo gerado
empregT <- read_baskets("survey.txt", sep = ";",info = c("sequenceID","eventID","size") )

summary(empregT)

#iniciando a busca por relações
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

#Segunda Análise
#tratamento dos dados

#excluindo da base as idades que estão fora do range 18-80
#isso vai garantir uma análise mais consisa e sem distorções por idades incoerentes
#como idades negativas ou muito maiores que 80 anos
empregadosCorrigido <- subset(empregados, Age>=18)
empregadosCorrigido <- subset(empregadosCorrigido, Age<=70)

summary(empregadosCorrigido)

#separando a base em duas baseado na coluna self_employed
self_employed <- subset(empregadosCorrigido, self_employed=="Yes")
no_self_employed <- subset(empregadosCorrigido, self_employed=="No")

#apagando a coluna self_employed das duas bases
self_employed$self_employed <- NULL
no_self_employed$self_employed <- NULL

no_self_employed$Country <- str_replace_all(no_self_employed$Country, c("\\s"="_", "\\."="_", "-"="_"))
no_self_employed$benefits <- str_replace_all(no_self_employed$benefits, c("\\s"="_", "\\."="_", "-"="_"))
no_self_employed$care_options <- str_replace_all(no_self_employed$care_options, c("\\s"="_", "\\."="_", "-"="_"))
no_self_employed$wellness_program <- str_replace_all(no_self_employed$wellness_program, c("\\s"="_", "\\."="_", "-"="_"))
no_self_employed$seek_help <- str_replace_all(no_self_employed$seek_help, c("\\s"="_", "\\."="_", "-"="_"))
no_self_employed$anonymity <- str_replace_all(no_self_employed$anonymity, c("\\s"="_", "\\."="_", "-"="_"))
no_self_employed$leave <- str_replace_all(no_self_employed$leave, c("\\s"="_", "\\."="_", "-"="_"))
no_self_employed$coworkers <- str_replace_all(no_self_employed$coworkers, c("\\s"="_", "\\."="_", "-"="_"))
no_self_employed$supervisor <- str_replace_all(no_self_employed$supervisor, c("\\s"="_", "\\."="_", "-"="_"))
no_self_employed$mental_vs_physical <- str_replace_all(empregados$mental_vs_physical, c("\\s"="_", "\\."="_", "-"="_"))

no_self_employed$no_employees <- str_replace_all(no_self_employed$no_employees, c("\\s"="_"))
no_self_employed$Gender <- str_replace_all(no_self_employed$Gender, c("\\s"="", "\\."="", "-"="","\\("="", "\\)"=""))
no_self_employed$Gender <- str_replace_all(toupper(no_self_employed$Gender),c("CIS"="", "TRANS"=""))
no_self_employed$Gender <- str_replace_all(toupper(no_self_employed$Gender),c("FEMALE"="F", "MALE"="M"))

datasetTest <- no_self_employed
datasetTest$Country <- NULL
datasetTest$work_interfere <- NULL
datasetTest$no_employees <- NULL
datasetTest$size <- NULL



#rules <- apriori(no_self_employed, parameter = list(supp = 0.6, conf = 0.8))


#escrevendo um novo arquivo com a parte mais importante da análise
write_delim( 
  datasetTest,
  delim =";", path = "dataset.txt", col_names = FALSE
)

#lendo o arquivo gerado
no_self_employedgT <- read_baskets("dataset.txt", sep = ";", info = c("sequenceID","eventID", "Age", "Gender") )

summary(no_self_employedgT)

#iniciando a busca por relações
no_self_employedSeq <- cspade(
  no_self_employedgT, 
  parameter = list(support = 0.0001, maxlen=3), 
  control   = list(verbose = TRUE)
)

summary(no_self_employedSeq)

#trabalhando na visualização
seqResultNoEmp = as(no_self_employedSeq, "data.frame")
seqResultNoEmp = seqResultNoEmp %>% mutate(
  sequence = as.character(sequence)
)
