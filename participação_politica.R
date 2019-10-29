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
library(dplyr)

# ----- EVOLUÇÃO ELEITORADO RECIFE POP TOTAL - TRE/PE -----#
#carregar banco
eleitorado_recife = read.csv("participacao_politica/eleitorado_rec_2010-2019.csv",
                             sep = ";", dec = ",")
str(eleitorado_recife)

#Renomear variaveis
colnames(eleitorado_recife) = c('ano', 'fx_etaria', 'masc', 'perc_masc',
                                'fem', 'perc_fem', 'nao_informado', 'perc_ninf',
                                'total', 'perc_total')
#ajustar tipo de variaveis
eleitorado_recife$fx_etaria = as.character(eleitorado_recife$fx_etaria)
eleitorado_recife$ano = as.character(eleitorado_recife$ano)

# --- Visualizar por faixa etária --- #
#filtrando juventude (percentual)
juv = c("16 anos", "17 anos", "18 a 20 anos", "21 a 24 anos")
eleit_juv = eleitorado_recife %>%
  filter(fx_etaria %in% juv) %>%
  select(ano, fx_etaria, perc_fem, perc_masc, perc_total)

#config df para visualização
eleit_juv = gather(eleit_juv, key = genero, value = percentual, 3:5)

#remomear genero
eleit_juv$genero = str_replace(eleit_juv$genero, "perc_masc", "Masculino")
eleit_juv$genero = str_replace(eleit_juv$genero, "perc_fem", "Feminino")
eleit_juv$genero = str_replace(eleit_juv$genero, "perc_total", "Total")

eleit_juv = eleit_juv %>%
  filter(genero == "Total")

# --- Visualizar por gênero --- #
#filtrando juventude (numeros abs)
eleit_juv2 = eleitorado_recife %>%
  filter(fx_etaria %in% juv) %>%
  select(ano, fx_etaria, fem, masc, total)

#config df para visualização
eleit_juv2$fx_etaria = NULL

#agregar faixas etarias
eleit_juv2 = eleit_juv2 %>%
  select(fem, masc, total, ano) %>%
  group_by(ano) %>%
  summarise_all(list(sum))

#calcular percentual por genero
eleit_juv2$perc_fem = eleit_juv2$fem / eleit_juv2$total
eleit_juv2$perc_masc = eleit_juv2$masc / eleit_juv2$total
eleit_juv2[, 3:4] = NULL

#config df para visualização
eleit_juv2 = gather(eleit_juv2, key = genero, value = percentual, 3:4)

#remomear genero
eleit_juv2$genero = str_replace(eleit_juv2$genero, "perc_masc", "Masculino")
eleit_juv2$genero = str_replace(eleit_juv2$genero, "perc_fem", "Feminino")

#limitar decimais no %
eleit_juv2$percentual = as.numeric(format(eleit_juv2$percentual,
                  digits = 2, format = "f"))

#------ graficos ------#
#Faixa Etaria
g_eleit_idade = ggplot(eleit_juv, aes(ano, percentual, group=fx_etaria, colour=fx_etaria)) +
  geom_line()+
  geom_point()+
  labs (x = "Ano*",
        y = "% Eleitores",
        fill = "Faixa Etaria",
        title = "Evolução do eleitores jovens - Recife",
        caption = "* Os valores correspondem ao mês de setembro") +
  tema_massa()
#salvar
g_eleit_idade + ggsave("eleit_idade.png",
               path = "participacao_politica/graficos e mapas",
               width = 7, height = 4, units = "in")

#Genero
g_eleit_gen = ggplot(eleit_juv2, aes(ano, percentual,
                                     fill=genero)) +
  geom_bar(stat="identity", position = 'dodge')+
  labs (x = "Ano*",
        y = "% Eleitores",
        fill = "Genero",
        title = "Evolução do eleitores jovens - Recife",
        caption = "* Os valores correspondem ao mês de setembro") +
  tema_massa()
#salvar
g_eleit_gen + ggsave("eleit_genero.png",
                       path = "participacao_politica/graficos e mapas",
                       width = 7, height = 4, units = "in")


# ----- EVOLUÇÃO ELEITORADO RECIFE POP JOVEM - TRE/PE -----#
