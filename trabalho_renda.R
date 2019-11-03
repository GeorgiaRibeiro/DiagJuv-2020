#------------------------------------------------#
# Levantamento e análise de dados sobre o Recife #
#   Georgia Ribeiro | Github: @GeorgiaRibeiro    #
#           Temática: Trabalho e Renda           #
#------------------------------------------------#

#= = = ações bases = = =#
#carregar bibs
library(tidyverse) #Análise de dados
library(ggplot2) #Visualização gráficos
library(plotly) #Visualização graficos interativa
library(htmlwidgets) #salvar html
library(tidyr, devtools) #estrutura do banco
library(maptools) #visualização com mapas
library(ggmap)
library(rgdal)
library(xlsx) #exportar df para excel NÃO CONSEGUI INSTALAR
library(dplyr)

#PNADc TRIMESTRAL
rend = read.csv("trabalho_renda/rend_idade_PNADc.csv", sep = ";")
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
  geom_line()+
  geom_point()+
  ylim(1400,2400) +
    labs(x="Ano",
       y="Rendimento (R$)",
       caption = "Elaboração própria | Dados: PNAD contínua trimestral") +
  geom_text(aes(label = round(Brasil,0)),
            position = position_dodge(0.9), vjust = -1, size = 3) +
  temamassa()
#salvar
g1_br + ggsave("rend_total_ano_br.png",
       path = "trabalho_renda/graficos e mapas",
       width = 7, height = 4, units = "in")

#p1_br = ggplotly(g1_br)
#saveWidget(p1_br, file="rend_total_ano_br.html",
           #selfcontained = F, libdir = "trabalho_renda/graficos e mapas")

g1_rec = ggplot(renda_ano, aes(x=ano, y=Recife, group = 1)) +
  geom_line()+
  geom_point()+
  ylim(2000,3000)+
  labs(x="Ano",
       y="Rendimento (R$)",
       caption = "Elaboração própria | Dados: PNAD contínua trimestral") +
  geom_text(aes(label = round(Recife,0)),
            position = position_dodge(0.9), vjust = -1, size = 3)+
  temamassa()
#salvar
g1_rec + ggsave("rend_total_ano_rec.png",
               path = "trabalho_renda/graficos e mapas",
               width = 7, height = 4, units = "in")

p1_rec = ggplotly(g1_rec)

#--------- Rendimento por faixa etária -------- #
rend.etaria = rend %>%
  filter(Idade != "Total")

rend.etaria$Periodo = NULL

rend.etaria$Recife = rend.etaria$Recife..PE 
rend.etaria$Recife..PE. = NULL

rend.etaria$Idade = as.character(rend.etaria$Idade)

#Rendimento médio/ano por faixa etarias (APRENDER FAZER LOOP EM NOME DO SENHOR!!!)
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
  geom_line()+
  geom_point()+
  ylim(300,3000)+
  labs(x="Ano",
       y="Rendimento (R$)",
       fill= "Faixa etária",
       caption = "Elaboração própria | Dados: PNAD contínua trimestral") +
  geom_text(data = renda_etaria[renda_etaria$Idade != "40 a 59 anos",], show.legend = FALSE,
            aes(label = round(Brasil,0), colour = factor(Idade)),
            hjust = 0.5, vjust = -1, size = 3)+
  geom_text(data = renda_etaria[renda_etaria$Idade == "40 a 59 anos",], show.legend = FALSE,
            aes(label = round(Brasil,0), colour = factor(Idade)),
            hjust = 0.5, vjust = 2, size = 3)+
  tema_massa()


#position = position_dodge(0.9), hjust=0.25, vjust = -1
#salvar
g2_br + ggsave("rend_idade_br.png",
               path = "trabalho_renda/graficos e mapas",
               width = 7, height = 4, units = "in")

p2_br = ggplotly(g2_br)

g2_rec = ggplot(renda_etaria, aes(x=ano, y=Recife, group=Idade, colour=Idade)) +
  geom_line()+
  geom_point()+
  ylim(300, 4900)+
  labs(x="Ano",
       y="Rendimento (R$)",
       fill= "Faixa etária",
       caption = "Elaboração própria | Dados: PNAD contínua trimestral") +
  geom_text(data = renda_etaria[renda_etaria$Idade != "40 a 59 anos",], show.legend = FALSE,
            aes(label = round(Recife,0), colour = factor(Idade)),
            hjust = 0.5, vjust = -1, size = 3)+
  geom_text(data = renda_etaria[renda_etaria$Idade == "40 a 59 anos",], show.legend = FALSE,
            aes(label = round(Recife,0), colour = factor(Idade)),
            hjust = 0.5, vjust = 2, size = 3)+
  tema_massa()
g2_rec + ggsave("rend_idade_rec.png",
               path = "trabalho_renda/graficos e mapas",
               width = 7, height = 4, units = "in")

p2_rec = ggplotly(g2_rec)

#CENSO DEMOG.
#carregar banco
rend.recife = read.csv("trabalho_renda/rend_idade_genero_CENSO.csv", sep = ";", dec = ",")
str(rend.gen)

#Transformar tipo do banco
rend.recife = gather(rend.recife, renda, valor, Até.1.SM:Sem.rendimento)

#Renomear renda
rend.recife$renda = str_replace_all(rend.recife$renda,"\\.", " ")

#--------- Rendimento por gênero -------- #
#agrupar juventude por genero
rend.recife1 = rend.recife %>%
  filter(fx_etaria != "Total", local == "Recife", genero != "Total") %>%
  select(genero, renda, valor) %>%
  group_by(genero, renda) %>%
  summarise_at(vars(valor), sum)

#calcular percentual de jovens
b = rend.recife %>%
  filter(fx_etaria != "Total", local == "Recife", genero == "Total") %>%
  summarise(perc_jovem =sum(valor))

#percentual dos jovens
rend.recife1$percentual =  as.numeric(format(((rend.recife1$valor/b$perc_jovem)*100),
                                             digits = 2, format = "f"))
#------ graficos ------#
g3_gen_v2  = ggplot(rend.recife1, aes(genero, percentual, fill = renda)) +
  geom_bar(position = "dodge", stat = "identity") +
  ylim(0,35)+
  labs(x="Gênero",
       y="% Jovens",
       fill= "Classe de rendimento",
       caption = "Elaboração própria | Dados: Censo 2010/IBGE") +
  geom_text(aes(label = round(percentual,0), colour = (factor(rend.recife1$renda))), show.legend = F,
            position = position_dodge(0.9), vjust = -0.5, size = 3) +
  tema_massa()
g3_gen_v2 + ggsave("rend_genero_censo2.png",
                path = "trabalho_renda/graficos e mapas",
                width = 7, height = 4, units = "in")

p3_gen = ggplotly(g3_gen)
#--------- Rendimento por faixa etária -------- #
rend.recife2 = rend.recife %>%
  filter(genero == "Total", local == "Recife", fx_etaria != "Total")

rend.recife2$percentual = as.numeric((rend.recife2$valor/rend.recife2$Total)*100)

#------ graficos ------#
g4_idade = ggplot(rend.recife2, aes(x=fx_etaria, y=round(percentual,2), fill = renda)) +
  geom_bar(position = "dodge", stat = "identity") +
  ylim(0,100)+
  labs(x="Faixa etária",
       y="% Jovens",
       fill= "Classe de rendimento",
       caption = "Elaboração própria | Dados: Censo 2010/IBGE") +
  geom_text(aes(label = round(percentual,0), colour = (factor(renda))), show.legend = F,
            position = position_dodge(0.9), vjust = -0.5, size = 3)+
  tema_massa()
g4_idade + ggsave("rend_idade_censo.png",
                path = "trabalho_renda/graficos e mapas",
                width = 7, height = 4, units = "in")

p4_idade = ggplotly(g4_idade)

#--------- Rendimento por  raça/cor -------- #
#carregar banco
rend.raca = read.csv("trabalho_renda/rend_raca-cor_CENSO.csv",
                     sep = ";", dec = ",")
str(rend.raca)

#alterar tipo das variaveis
rend.raca$local = as.character(rend.raca$local)
rend.raca$fx_etaria = as.character(rend.raca$fx_etaria)
rend.raca$renda = as.character(rend.raca$renda)

#Transformar tipo do banco
rend.raca = gather(rend.raca, raca, pop, 4:8)

#calcular total de jovens
a = rend.raca %>%
  filter(fx_etaria != "Total") %>%
  summarise(pop_jovem =sum(pop))

#agrupar juventude
rend.raca2 = rend.raca %>%
  filter(fx_etaria != "Total", renda != "Total") %>%
  select(raca, renda, pop) %>%
  group_by(raca, renda) %>%
  summarise_at(vars(pop), sum)

#calcular percentual
rend.raca2$percentual = as.numeric(format(((rend.raca2$pop / a$pop_jovem)*100),
                  digits = 2, format = "f"))

#------ graficos ------#
g5_raca = ggplot(rend.raca2, aes(raca, percentual, fill = renda)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x="Raça/Cor",
       y="% Jovens",
       fill= "Classe de rendimento",
       caption = "Elaboração própria | Dados: Dados: Censo 2010/IBGE") +
  geom_text(aes(label = round(percentual,0), colour = (factor(renda))), show.legend = F,
            position = position_dodge(0.9), vjust = -0.5, size = 3.5)+
  tema_massa()
g5_raca + ggsave("rend_raca-cor_censo.png",
                path = "trabalho_renda/graficos e mapas",
                width = 7, height = 4, units = "in")


#------ tabelas ------#

#PACOTE XLSX NÃO CONSEGUI INSTALAR
write.xlsx(mydata, "c:/mydata.xlsx")

#PACOTE RIO INSTALOU MAS N FUNCIONOU
export(list(total=renda_ano,
            bairros=renda_bairros,
            idade=renda_etaria,
            genero =,
            raça= ), "renda.xlsx")


#= = = Distribuição de classes de rendimento por municípios/Recife ~ Censo IBGE
#carregar banco
rend.bairros = read.csv("trabalho_renda/rend-sm_bairros_CENSO.csv", sep=";", dec=",")

#Alterar  tipo dado
rend.bairros$local = as.character(rend.bairros$local)

#Limpar observações irrelevantes [1. Fonte, 2. Filtrar municípios do Recife]
#1.
rend.bairros = slice(rend.bairros, 1:99)

#2.1. Substituir parentesis para filtrar "Recife (PE)"
rend.bairros$local = str_replace_all(rend.bairros$local,"\\(", ".")
rend.bairros$local = str_replace_all(rend.bairros$local,"\\)", ".")

#2.2. Filtrar
rend.bairros = rend.bairros %>%
  filter(str_detect(local, "Recife .PE."))

#tidy para "key-value" #GENIAL
renda_bairros = gather(rend.bairros, Renda, Valor, Até.1.SM:Sem.rendimento)

#Ajeitar nome das variaveis
renda_bairros$Renda = str_replace_all(renda_bairros$Renda,"\\.", " ")

renda_bairros$local = str_sub(renda_bairros$local, end = -12)
renda_bairros$local = rm_accent(renda_bairros$local, pattern="all")
renda_bairros$local = str_to_upper(renda_bairros$local)

#= = = Rendimento médio por bairros/Recife ~ Censo IBGE
#carregar banco
rendmed.bairros = read.csv("trabalho_renda/rendmedio_bairros_cor_CENSO.csv",
                           sep=";", dec=",")

#Limpar observações irrelevantes [1. Fonte, 2. dados pro Brasil e Recife]
#1.
rendmed.bairros = slice(rendmed.bairros, 1:96)
#2.
rendmed.bairros = rendmed.bairros[-c(1,2),]

#Alterar  tipo dado
rendmed.bairros$Local = as.character(rendmed.bairros$Local)

#Ajeitar nome dos bairros
rendmed.bairros$Local = str_sub(rendmed.bairros$Local, end = -12)
rendmed.bairros$Local = rm_accent(rendmed.bairros$Local, pattern="all")
rendmed.bairros$Local = str_to_upper(rendmed.bairros$Local)
str(rendmed.bairros)

#renomear colunas
colnames(rendmed.bairros) = c('Bairros', 'rm_total','rm_branca', 'rm_preta', 'rm_amarela',
                              'rm_parda', 'rm_indigena')

#EXPORTAR DF PARA GERAR MAPA
write.csv(rendmed.bairros, "bairros_renda.csv", row.names = FALSE)
