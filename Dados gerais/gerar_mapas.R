#------------------------------------------------#
# Levantamento e análise de dados sobre o Recife #
#   Georgia Ribeiro | Github: @GeorgiaRibeiro    #
#                   Gerar mapas                  #
#------------------------------------------------#

library(tidyverse)
library(dplyr)

#carregar bancos 
cultura = read.csv("cultura_lazer/bairros_bomdebola.csv")
renda = read.csv('trabalho_renda/renda_juv_bairros.csv')

#conferir tipo das variaveis
str(cultura)

str(renda)

#excluir obs vazia
cultura = data.frame(apply(cultura, 2, function(x) gsub("^$|^ $", NA, x)))

#Igualar nome da variavel Bairros
colnames(renda)[1] = c('Bairros')

#alterar tipo das variaveis
cultura$Num_jogador = as.numeric(as.character(cultura$Num_jogador))
cultura$Bairros = as.character(cultura$Bairros)
renda$Bairros = as.character(renda$Bairros)

#Unir tematicas em df para gerar mapas (tentativa 2)
df_bairros = left_join(renda, cultura, by= "Bairros", all = T)
