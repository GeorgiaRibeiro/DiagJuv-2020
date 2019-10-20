#------------------------------------------------#
# Levantamento e análise de dados sobre o Recife #
#   Georgia Ribeiro | Github: @GeorgiaRibeiro    #
#           Temática: Trabalho e Renda           #
#------------------------------------------------#

#carregar bibs
library(tidyverse)
library(ggplot2)

#Rendimento/idade
rend.id = read.csv("trabalho_renda/redimento_idade_PNADc.csv", sep = ";")
str(rend.id)

#Criar média por ano
rend.id = mutate(rend.id, ano = str_sub(rend.id$Periodo, -4))
rend.id %>%
  filter(Idade == "Total")
 

