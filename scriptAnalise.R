#autores: Amanda, Laura

#importação de bibliotecas necessárias
install.packages("arules")
install.packages("arulesSequences")


library(arules)
library(arulesSequences)

#importação da base de dados
empregados <- data.frame(read.csv(file="survey.csv", header=TRUE, sep=","))

View(empregados)

#tratamento dos dados

write_delim( 
  empregados %>% select( c(Age, Gender, Country, state, self_employed, family_history, treatment, work_interfere, no_employees, remote_work, tech_company, benefits, care_options, wellness_program, seek_help, anonymity, leave, mental_health_consequence, phys_health_consequence, coworkers, supervisor, mental_health_interview, phys_health_interview, mental_vs_physical, obs_consequence)),
  delim ="\t", path = "survey.txt", col_names = FALSE
)

#busca por relações

