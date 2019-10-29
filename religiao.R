#------------------------------------------------#
# Levantamento e análise de dados sobre o Recife #
#   Georgia Ribeiro | Github: @GeorgiaRibeiro    #
#               Temática: Religião               #
#------------------------------------------------#

#carregar bibs
library(tidyverse) #Análise de dados
library(ggplot2) #Visualização gráficos
library(plotly) #Visualização graficos interativa
library(tidyr, devtools) #estrutura do banco
library(maptools) #visualização com mapas
library(ggmap)
library(rgdal)
library(xlsx) #exportar df para excel NÃO CONSEGUI INSTALAR

#importar pacotes
source("Dados gerais/funcoes.R")

#= = = População residente por religião
prel = read.csv("religiao/pop_religiao.csv", sep = ";")

#agrupar religiões
prel$Católica = as.numeric(apply(prel[,2:4], 1, sum))
prel[,2:4] = NULL

prel$NS.SD = as.numeric(apply(prel[,18:19], 1, sum))
prel[,18:19] = NULL

#alterar df: tidy para key-value
prel= gather(prel, religiao, populacao, 2:19)
prel$perc_pop = prel$populacao / prel$Total
prel$perc_pop = as.numeric(format(prel$perc_pop,
                                  digits = 2))
