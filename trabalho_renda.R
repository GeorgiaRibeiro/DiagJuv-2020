#------------------------------------------------#
# Levantamento e análise de dados sobre o Recife #
#   Georgia Ribeiro | Github: @GeorgiaRibeiro    #
#           Temática: Trabalho e Renda           #
#------------------------------------------------#

#carregar bibs
library(tidyverse)
library(ggplot2)
library(plotly)

#Rendimento (Brasil e Recife) ~ PNADc trimestral
rend = read.csv("trabalho_renda/redimento_idade_PNADc.csv", sep = ";")
str(rend)
rend$Idade = as.character(rend$Idade)

#--------- Rendimento por ano (media entre trimestres) -------- #
rend = mutate(rend, ano = as.character(str_sub(rend$Periodo, -4)))
rend.idade = rend %>%
  filter(Idade == "Total")
 
renda_ano = aggregate(rend.idade[, 3:4], list(rend.idade$ano), mean)
renda_ano$Recife = renda_ano$Recife..PE 
renda_ano$Recife..PE. = NULL
renda_ano$ano = renda_ano$Group.1 
renda_ano$Group.1 = NULL

#------ graficos ------#
#g1. Renda por ano Brasil(g1_br) e Recife(g1_rec)
g1_br = ggplot(renda_ano, aes(x=ano, y=Brasil, group = 1)) +
  geom_line(color = "dodgerblue3", size=1.1) +
  geom_point(color = "dodgerblue4") +
  labs(x="Ano",
       y="Rendimento (R$)",
       title = "Rendimento por ano - Brasil",
       subtitle = "Dados: PNAD contínua trimestral",
       caption = "Fonte: Elaboração própria") +
  theme_light()

g1_br = ggplotly(g1_br)

g1_rec = ggplot(renda_ano, aes(x=ano, y=Recife, group = 1)) +
  geom_line(color = "dodgerblue3", size=1.1) +
  geom_point(color = "dodgerblue4") +
  labs(x="Ano",
       y="Rendimento (R$)",
       title = "Rendimento por ano - Recife",
       subtitle = "Dados: PNAD contínua trimestral",
       caption = "Fonte: Elaboração própria") +
  theme_light()

g1_rec = ggplotly(g1_rec)
print(g1_rec)

#--------- Rendimento por faixa etária -------- #
rend.etaria = rend %>%
  filter(Idade != "Total")

rend.etaria$Periodo = NULL

rend.etaria$Recife = rend.etaria$Recife..PE 
rend.etaria$Recife..PE. = NULL

rend.etaria$Idade = as.character(rend.etaria$Idade)

#média de rendimento por faixa etarias por ano (APRENDER FAZER LOOP EM NOME DO SENHOR!!!)
R12 = rend.etaria %>%
  filter(ano == "2012") %>%
  select(Brasil, Recife, Idade) %>%
  group_by(Idade) %>%
  summarise_all(list(mean))
R12 = mutate(R12, ano = "2012")

R13 = rend.etaria %>%
  filter(ano == "2013") %>%
  select(Brasil, Recife, Idade) %>%
  group_by(Idade) %>%
  summarise_all(list(mean))
R13 = mutate(R13, ano = "2013")


R14 = rend.etaria %>%
  filter(ano == "2014") %>%
  select(Brasil, Recife, Idade) %>%
  group_by(Idade) %>%
  summarise_all(list(mean))
R14 = mutate(R14, ano = "2014")


R15 = rend.etaria %>%
  filter(ano == "2015") %>%
  select(Brasil, Recife, Idade) %>%
  group_by(Idade) %>%
  summarise_all(list(mean))
R15 = mutate(R15, ano = "2015")


R16 = rend.etaria %>%
  filter(ano == "2016") %>%
  select(Brasil, Recife, Idade) %>%
  group_by(Idade) %>%
  summarise_all(list(mean))
R16 = mutate(R16, ano = "2016")


R17 = rend.etaria %>%
  filter(ano == "2017") %>%
  select(Brasil, Recife, Idade) %>%
  group_by(Idade) %>%
  summarise_all(list(mean))
R17 = mutate(R17, ano = "2017")

renda_etaria = rbind(R12, R13, R14, R15, R16, R17)

#------ graficos ------#
#g2. Renda por faixa etária Brasil(g2_br) e Recife(g2_rec)

g2_br = ggplot(renda_etaria, aes(x=ano, y=Brasil, group=Idade, colour=Idade)) +
  geom_line(size=1.1) +
  geom_point() +
  labs(x="Ano",
       y="Rendimento (R$)",
       fill= "Faixa etária",
       title = "Rendimento por faixa etária - Brasil",
       subtitle = "Dados: PNAD contínua trimestral",
       caption = "Fonte: Elaboração própria") +
  theme_light()

g12_br = ggplotly(g2_br)

g2_rec = ggplot(renda_etaria, aes(x=ano, y=Recife, group=Idade, colour=Idade)) +
  geom_line(size=1.1) +
  geom_point() +
  labs(x="Ano",
       y="Rendimento (R$)",
       fill= "Faixa etária",
       title = "Rendimento por faixa etária - Recife",
       subtitle = "Dados: PNAD contínua trimestral",
       caption = "Fonte: Elaboração própria") +
  theme_light()

g2_rec = ggplotly(g2_rec)
print(g2_rec)

#--------- Rendimento por gênero -------- #

#--------- Rendimento por raça -------- #

#Rendimento (Bairros/Recife) ~ Censo IBGE
#instalar e carregar pacotes para alterar estrutura do banco
install.packages(c("tidyr", "devtools"))
library(tidyr, devtools)

#carregar banco
rend.bairros = read.csv("trabalho_renda/rend_agp_bairros_CENSO.csv", sep=";", dec=",")
#excluir ultima linha
rend.bairros = slice(rend.bairros, 1:99)
#Alterar  tipo dado
rend.bairros$local = as.character(rend.bairros$local)

#tidy para "key-value" #GENIAL
renda_bairros = gather(rend.bairros, Renda, Valor, Até.1.SM:Sem.rendimento)

#Limpar nome dos grupos de SM
renda_bairros$Renda = str_replace_all(renda_bairros$Renda,"\\.", " ")

#------ graficos ------#
#g3. Distribuição de renda por bairro



