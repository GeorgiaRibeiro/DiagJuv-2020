#------------------------------------------------#
# Levantamento e análise de dados sobre o Recife #
#   Georgia Ribeiro | Github: @GeorgiaRibeiro    #
#        Temática: Participação Política         #
#------------------------------------------------#

#carregar bibs
library(tidyverse)
library(ggplot2)
library(plotly)
library(tidyr)

# ----- Evolucao do eleitorado - TRE/PE -----#
#carregar bancos
eleitorado1 = read.csv("participacao_politica/eleitorado-PE_2002-2003.csv", sep =';')
eleitorado2 = read.csv("participacao_politica/eleitorado-PE_2007-2011.csv", sep =';')
eleitorado3 = read.csv("participacao_politica/eleitorado-PE_2015-2019.csv", sep =';')

#Alterar coluna para períodos
eleitorado1$Set.2002 = eleitorado1$Inicio
eleitorado1$Set.2003 = eleitorado1$Fim
eleitorado1$Inicio = NULL
eleitorado1$Fim = NULL

eleitorado2$Set.2007 = eleitorado2$Inicio
eleitorado2$Set.2011 = eleitorado2$Fim
eleitorado2$Inicio = NULL
eleitorado2$Fim = NULL

eleitorado3$Set.2015 = eleitorado3$Inicio
eleitorado3$Set.2019 = eleitorado3$Fim
eleitorado3$Inicio = NULL
eleitorado3$Fim = NULL

#Selecionar apenas Recife
eleitorado1 = eleitorado1 %>%
  filter(Abrangencia == "RECIFE")
eleitorado2 = eleitorado2 %>%
  filter(Abrangencia == "RECIFE")
eleitorado3 = eleitorado3 %>%
  filter(Abrangencia == "RECIFE")

#Transformar df (tidy para "key-value")
eleitorado1 = gather(eleitorado1,Periodo, Eleitorado, 3:4)
eleitorado2 = gather(eleitorado2,Periodo, Eleitorado, 3:4)
eleitorado3 = gather(eleitorado3,Periodo, Eleitorado, 3:4)

#Unir tabelas
eleitorado_recife = rbind(eleitorado1, eleitorado2, eleitorado3)
str(eleitorado_recife)

#Renomear periodos
eleitorado_recife$Periodo = str_replace(eleitorado_recife$Periodo, "\\.", " ")
eleitorado_recife$Periodo = str_remove(eleitorado_recife$Periodo, "Setembro")

#Incluir populacao recife (Censo 2000 e 2010)
eleitorado_recife = mutate(eleitorado_recife, 
                           Pop = as.integer(c("1422905", "1422905", "1422905", "1537704", "1537704", "1537704")))

#Incluir % de eleitores
eleitorado_recife = mutate(eleitorado_recife,
                           Perc_eleit = (Eleitorado / Pop))
eleitorado_recife$Perc_eleit = as.numeric(format(eleitorado_recife$Perc_eleit,
                                      digits = 2, format = "f"))

#------ graficos ------#
ggplot(eleitorado_recife, aes(Periodo, Perc_eleit, group =1)) +
  geom_line(color = "dodgerblue3", size=1.1) +
  geom_point(color = "dodgerblue4") +
  ylim (0.5,1) +
  labs (x = "Ano*",
        y = "% Eleitores",
        title = "Evolução do eleitorado - Recife",
        caption = "* Os valores correspondem ao mês de setembro") +
  geom_text(aes(label = Perc_eleit, y = Perc_eleit + 0.01),
            position = position_dodge(0.9),
            vjust = 0,
            color = "gray30",
            size = 4)
