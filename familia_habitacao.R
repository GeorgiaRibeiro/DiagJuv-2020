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
        caption = "Elaboração própria | Dados: Censo 2010/IBGE") +
  geom_text(aes(label = perc, y = perc +0.3),
            position = position_dodge(0.9)) +
  tema_massa()
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
        caption = "Elaboração própria | Dados: Censo 2010/IBGE") +
  geom_text(aes(label = perc, y = perc +0.3),
            position = position_dodge(0.9)) +
  tema_massa()
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

#-- definir tipos das variaveis - erro
#v = cond[1:3]
#cond[,4:21] = lapply(cond[,4:21], as.numeric)

#-- definir manual
cond$local = as.character(cond$local)
cond$genero = as.character(cond$genero)
cond$condicao = as.character(cond$condicao)

cond$tot_total = as.numeric(format(digits = 0, format = "f", cond$`tot_total`))
cond$`15_total` = as.numeric(format(digits = 0, format = "f", cond$`15_total`))
cond$`20_total` = as.numeric(format(digits = 0, format = "f", cond$`20_total`))
cond$tot_s.inst_fun.inc = as.numeric(format(digits = 0, format = "f", cond$`tot_s.inst_fun.inc`))
cond$`15_s.inst_fun.inc` = as.numeric(format(digits = 0, format = "f", cond$`15_s.inst_fun.inc`))
cond$`20_s.inst_fun.inc` = as.numeric(format(digits = 0, format = "f", cond$`20_s.inst_fun.inc`))
cond$tot_fun.c_med.ind = as.numeric(format(digits = 0, format = "f", cond$`tot_fun.c_med.ind`))
cond$`15_fun.c_med.ind` = as.numeric(format(digits = 0, format = "f", cond$`15_fun.c_med.ind`))
cond$`20_fun.c_med.ind` = as.numeric(format(digits = 0, format = "f", cond$`20_fun.c_med.ind`))
cond$tot_med.c_sup.inc = as.numeric(format(digits = 0, format = "f", cond$`tot_med.c_sup.inc`))
cond$`15_med.c_sup.inc` = as.numeric(format(digits = 0, format = "f", cond$`15_med.c_sup.inc`))
cond$`20_med.c_sup.inc` = as.numeric(format(digits = 0, format = "f", cond$`20_med.c_sup.inc`))
cond$tot_sup.c = as.numeric(format(digits = 0, format = "f", cond$`tot_sup.c`))
cond$`15_sup.c` = as.numeric(format(digits = 0, format = "f", cond$`15_sup.c`))
cond$`20_sup.c` = as.numeric(format(digits = 0, format = "f", cond$`20_sup.c`))
cond$tot_n.det = as.numeric(format(digits = 0, format = "f", cond$`tot_n.det`))
cond$`15_n.det` = as.numeric(format(digits = 0, format = "f", cond$`15_n.det`))
cond$`20_n.det` = as.numeric(format(digits = 0, format = "f", cond$`20_n.det`))

#criar fx etária juventude
cond$juv_total = apply(cond[,c(10,16)],1,sum)

#somar instruções para juventude
cond$juv_s.inst_fun.inc = apply(cond[,c(11,17)],1,sum)
cond$juv_fun.c_med.ind = apply(cond[,c(12,18)],1,sum)
cond$juv_med.c_sup.inc = apply(cond[,c(13,19)],1,sum)
cond$juv_sup.c = apply(cond[,c(14,20)],1,sum)
cond$juv_n.det = apply(cond[,c(15,21)],1,sum)

#Limpar banco
cond[10:21] = NULL

#alterar tipo de banco
cond = gather(cond, tipo, perc, 4:15)

#Separar coluna tipo -> fx_etaria e instrucao
cond = separate(cond, tipo, into = c("fx_etaria", "instrucao"), sep = "\\_")

#Renomear variabeis fx e inst
cond$fx_etaria = str_replace(cond$fx_etaria, "tot", "Total")
cond$fx_etaria = str_replace(cond$fx_etaria, "juv", "Jovens")

cond$instrucao= str_replace(cond$instrucao, "s.inst", "Sem instrução e fundamental incompleto")
cond$instrucao= str_replace(cond$instrucao, "fun.c", "Fundamental completo e médio incompleto")
cond$instrucao= str_replace(cond$instrucao, "med.c", "Médio completo e superior incompleto")
cond$instrucao= str_replace(cond$instrucao, "sup.c", "Superior completo")
cond$instrucao= str_replace(cond$instrucao, "n.det", "Não determinado")

cond$genero = str_replace(cond$genero, "Homens", "Masculino")
cond$genero = str_replace(cond$genero, "Mulheres", "Feminino")

#Filtrar juventude do recife
cond_juv = cond %>%
  filter(fx_etaria == "Jovens", local == "Recife") %>%
  select_all()
cond_juv$perc_juv = as.numeric(format(digits = 0,
                                      format = "f", ((cond_juv$perc/1.53)*100)))

# --- gráficos --- #
#g3 - Instrução do jovem responsável pelo domicílio por gênero

g3_inst_resp = ggplot(cond_juv, aes(genero, perc_juv, fill = instrucao)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x="Gênero",
       y="% Jovens",
       fill= "Grau de instrução",
       caption = "Elaboração própria | Dados: Censo 2010/IBGE") +
  geom_text(aes(label = perc_juv),
            position = position_dodge(0.9), vjust = -0.5, size = 2.75) +
  tema_massa()
g3_inst_resp + ggsave("resp_familiar_inst.png",
                   path = "familia_habitacao/graficos e mapas",
                   width = 7, height = 4, units = "in")
