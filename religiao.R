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

#= = = População residente por religião = = =#
relig = read.csv("religiao/religiao.csv", sep = ";", dec = ",")

# --- ajustes ---#
#Excluir fonte (ultima linha)
relig = relig[-c(67),]

#Renomear Recife
relig$local = str_replace_all(relig$local,"\\(", ".")
relig$local = str_replace_all(relig$local,"\\)", ".")
relig$local = str_remove(relig$local, " .PE.")


#criar faixa etaria Jovens
relig$Jovens = apply(relig[,c(4,5)],1,sum)
relig[4:5] = NULL

#alterar df: tidy para key-value
relig= gather(relig, fx_etaria, perc, 3:4)

#Filtrar Jovens
relig_pop = relig %>%
  filter(fx_etaria ==  "Total")

#Filtrar Jovens
relig_juv = relig %>%
  filter(fx_etaria ==  "Jovens")

#calcular percentual dentro de jovens por região
dfbr = data.frame(relig_juv %>%
                    filter(local == "Brasil"))
dfbr$perc_juv = as.numeric(format(digits = 1, format = "f", (dfbr$perc/17.94)*100))
dfpe = data.frame(relig_juv %>%
                    filter(local == "Pernambuco"))
dfpe$perc_juv = as.numeric(format(digits = 1, format = "f", (dfpe$perc/18.54)*100))
dfrec = data.frame(relig_juv %>%
                    filter(local == "Recife"))
dfrec$perc_juv = as.numeric(format(digits = 1, format = "f", (dfrec$perc/17.27)*100))

#Atualizar df com a coluna de percentual entre jovens
relig_juv = rbind(dfbr, dfpe, dfrec)

#Selecionar maiores respostas jovens e geral
relig_juv = relig_juv %>% filter(religiao != "Total")  %>%
  group_by(local) %>% top_n(6, perc_juv)

relig_pop = relig_pop %>% filter(religiao != "Total")  %>%
  group_by(local) %>% top_n(6, perc)

#ordenar  - Errado
relig_juv[order(relig_juv$perc_juv, decreasing=TRUE), ]
          
# --- gráficos --- #
#g1 - Religiões mais frequentes entre a juventude 

g1_rel1 = ggplot(relig_juv, aes(local, perc_juv, fill = religiao,
                      (x = reorder(perc_juv, desc(perc_juv))))) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_flip()+
  labs(x="",
       y="% Jovens",
       fill= "Religião",
       title = "Religiões mais frequentes entre a juventude",
       subtitle = "Dados: Censo 2010/IBGE",
       caption = "Fonte: Elaboração própria") +
  geom_text(aes(label = perc_juv, y = perc_juv +1.2),
            position = position_dodge(0.9),
            size = 3.5) +
  tema_massa()
g1_rel1 + ggsave("resp_familiar_inst.png",
                      path = "familia_habitacao/graficos e mapas",
                      width = 7, height = 4, units = "in")

#g2 - Religiões mais frequentes entre a população geral
g2_rel = ggplot(relig_pop, aes(local, perc, fill = religiao)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_flip()+
  labs(x="",
       y="% População",
       fill= "Religião",
       title = "Religiões mais frequentes entre a população",
       subtitle = "Dados: Censo 2010/IBGE",
       caption = "Fonte: Elaboração própria") +
  geom_text(aes(label = perc, y = perc +1.2),
            position = position_dodge(0.9),
            size = 3.5) +
  tema_massa()
g2_rel + ggsave("religiao_geral.png",
                 path = "religiao/graficos e mapas",
                 width = 7, height = 4, units = "in")