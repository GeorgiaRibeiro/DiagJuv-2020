#------------------------------------------------#
# Levantamento e análise de dados sobre o Recife #
#   Georgia Ribeiro | Github: @GeorgiaRibeiro    #
#        Temática: Familia e habitação           #
#------------------------------------------------#

#carregar bibs
library(tidyverse)
library(ggplot2)
library(plotly)
library(tidyr)
library(dplyr)

# ----- Responsavel pelo domicílio juv por genero-----#
dom = read.csv("familia_habitacao/resp_domicilio_por_tipo.csv", sep = ";", dec = ",")
str(dom)

#excluir ultima linha (fonte)
dom = dom[-c(31), ]

#definir tipo das variaveis
dom$local = as.character(dom$local)
dom$genero = as.character(dom$genero)
dom$unid_domestica = as.character(dom$unid_domestica)

#calcular percentual de jovens
dom$Jovens = rowSums(dom[5:6])
dom[5:6] = NULL

#alterar tipo de banco
dom = gather(dom, fx_etaria, perc, 4:5)
#Filtrar juventude do recife
dom = dom %>%
  filter(fx_etaria == "Jovens", local == "Recife") %>%
  select_all()

# --- gráficos --- #
#g1 - Jovens responsável pelo domicílio por gênero
dom1 = dom %>%
  filter(unid_domestica == "Total") %>%
  select_all()
g1 = ggplot(dom1, aes(genero, perc)) +
  geom_bar(stat="identity", position = 'dodge') +
  labs (x = "Gênero",
        y = "% Jovens",
        title = "Jovens responsáveis pelo domicílio - Recife",
        subtitle = "Dados: censo 2010/IBGE",
        caption = "Elaboração própria") +
  geom_text(aes(label = perc, y = perc +0.3),
            position = position_dodge(0.9)) +
  temamassa()
g1 + ggsave("resp_genero.png",
                       path = "familia_habitacao/graficos e mapas",
                       width = 7, height = 4, units = "in")

#g2 - Jovens responsáveis por tipo de domicílio
dom2 = dom %>%
  filter(genero == "Total") %>%
  select_all()

#ordenar eixo x
dom2$unid_domestica = factor(dom2$unid_domestica,
                             levels = dom2$unid_domestica[order(dom$perc)])

g2 = ggplot(dom2, aes(unid_domestica, perc)) +
  geom_bar(stat="identity", position = 'dodge') +
  labs (x = "Espécie de Unidade Doméstica",
        y = "% Jovens",
        title = "Jovens responsáveis por tipo de domicílio - Recife",
        subtitle = "Dados: censo 2010/IBGE",
        caption = "Elaboração própria") +
  geom_text(aes(label = perc, y = perc +0.3),
            position = position_dodge(0.9)) +
  temamassa()
g2 + ggsave("resp_tipo-dom.png",
        path = "familia_habitacao/graficos e mapas",
        width = 7, height = 4, units = "in")

# ----- Condição na família -----#
cond = read.csv("familia_habitacao/resp_familiar_instrucao.csv",
                sep = ";", dec = ".")

#Renomear variaveis
colnames(cond) = c('local','genero','condicao','tot_total',
                   'tot_s.inst_fun.inc','tot_fun.c_med.ind', 'tot_med.c_sup.inc',
                   'tot_sup.c', 'tot_n.det', '15_total',
                   '15_s.inst_fun.inc','15_fun.c_med.ind', '15_med.c_sup.inc',
                   '15_sup.c', '15_n.det', '20_total',
                   '20_s.inst_fun.inc','20_fun.c_med.ind', '20_med.c_sup.inc',
                   '20_sup.c', '20_n.det')
#excluir 1a linha (=nome colunas) e ultima (=fonte)
cond = cond[-c(1, 8), ]

str(cond)

#definir tipos das variaveis
v = cond[1:3]
cond = cond %>%
  mutate_if(is.factor, as.numeric(format(digits = 2, format = "f", cond[4:21])))

#criar fx etária jovem
cond$juv_total = colSums(cond[c(4,10,16)])

#calcular percentual de jovens
cond$Jovens = rowSums(cond[5:6])
cond[5:6] = NULL

#alterar tipo de banco
cond = gather(cond, fx_etaria, perc, 4:5)

#Filtrar juventude do recife
cond = cond %>%
  filter(fx_etaria == "Jovens", local == "Recife") %>%
  select_all()

# --- gráficos --- #
#g3 - Instrução do jovem responsável pelo domicílio por gênero

g1 = ggplot(dom1, aes(genero, perc)) +
  geom_bar(stat="identity", position = 'dodge') +
  labs (x = "Gênero",
        y = "% Jovens",
        title = "Jovens responsáveis pelo domicílio - Recife",
        subtitle = "Dados: censo 2010/IBGE",
        caption = "Elaboração própria") +
  geom_text(aes(label = perc, y = perc +0.3),
            position = position_dodge(0.9)) +
  temamassa()
g1 + ggsave("resp_genero.png",
            path = "familia_habitacao/graficos e mapas",
            width = 7, height = 4, units = "in")
