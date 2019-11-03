#------------------------------------------------#
# Levantamento e análise de dados sobre o Recife #
#   Georgia Ribeiro | Github: @GeorgiaRibeiro    #
#                   Gerar mapas                  #
#------------------------------------------------#

library(tidyverse)
library(dplyr)

#carregar bancos 
cultura = read.csv("cultura_lazer/bairros_bomdebola.csv")
renda = read.csv('trabalho_renda/bairros_renda.csv')

#conferir tipo das variaveis
str(cultura)
str(renda)

#excluir obs vazia
cultura = data.frame(apply(cultura, 2, function(x) gsub("^$|^ $", NA, x)))

#alterar tipo das variaveis
cultura$Num_jogador = as.numeric(as.character(cultura$Num_jogador))
cultura$Bairros = as.character(cultura$Bairros)
renda$Bairros = as.character(renda$Bairros)

#Unir tematicas em df para gerar mapas (tentativa 1)
df_bairros = merge(cultura, renda, by.x = "Bairros", by.y = "Bairros")

#Unir tematicas em df para gerar mapas (tentativa 2)
df_bairros = inner_join(cultura, renda, by= "Bairros", all = T)

#teste
df_bairros = full_join(cultura, renda, by= "Bairros")
  # Observa-se que somente os valores de culura estão passando,
  # é algum problema com o banco de renda
