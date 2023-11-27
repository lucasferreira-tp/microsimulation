#Load packages
source('./fun/setup.R')

# 01. People microdata ----------------------------------------------------
#Set
code_state<-23
code_muni_<-2304400

#Read data
dict_variables<-read_excel("data/dic_variables.xlsx")

data_micro_p<-read.fortran(paste0("data/00_microdados/Amostra_Pessoas_",as.character(code_state),".txt"),
                      format=dict_variables$FORMATO,
                       col.names=dict_variables$VARIAVEIS)
saveRDS(data_micro_p,paste0("data/00_microdados/01_data_micro_p_",as.character(code_state),".RDS"))

#Create a complete database with id
data_micro_p<- data_micro_p %>% filter (V0002==04400)
data_micro_p<- data_micro_p %>% mutate(id_g = 1:nrow(data_micro_p))
saveRDS(data_micro_p,"data/00_microdados/01_data_micro_p.RDS")


# V0653 - No trabalho principal, quantas horas trabalhava habitualmente por semana
# V0662 – Qual o tempo habitual gasto de deslocamento de sua casa até o trabalho
# V0629 – Curso que frequenta
# V0628 – Frequenta escola ou creche (pública/particular)
# V6529	RENDIMENTO MENSAL DOMICILIAR EM JULHO DE 2010 					
# V6530	RENDIMENTO DOMICILIAR, SALÁRIOS MÍNIMOS, EM JULHO DE 2010 					
# V6531	RENDIMENTO DOMICILIAR PER CAPITA EM JULHO DE 2010 					
# V6532	RENDIMENTO DOMICILIAR PER CAPITA, EM Nº DE SALÁRIOS MÍNIMOS, EM JULHO DE 2010 					
# V6527	RENDIMENTO MENSAL TOTAL EM JULHO DE 2010 					
# V6528	RENDIMENTO MENSAL TOTAL EM Nº DE SALÁRIOS MÍNIMOS EM JULHO DE 2010 					
# V6529	RENDIMENTO DOMICILIAR (DOMICÍLIO PARTICULAR) EM JULHO DE 2010  					
# V6530	RENDIMENTO DOMICILIAR (DOMICÍLIO PARTICULAR) EM Nº DE SALÁRIOS MÍNIMOS EM JULHO DE 2010  					
# V6531	RENDIMENTO DOMICILIAR (DOMICÍLIO PARTICULAR) PER CAPITA EM JULHO DE 2010 					
# V6532	RENDIMENTO DOMICILIAR (DOMICÍLIO PARTICULAR) PER CAPITA EM Nº DE SALÁRIOS MÍNIMOS EM JULHO DE 2010 					
