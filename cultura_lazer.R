#------------------------------------------------#
# Levantamento e análise de dados sobre o Recife #
#   Georgia Ribeiro | Github: @GeorgiaRibeiro    #
#        Temática: Cultura e lazer               #
#------------------------------------------------#

#carregar bibs
library(tidyverse) #Análise de dados
library(ggplot2) #Visualização gráficos
library(plotly) #Visualização graficos interativa
library(tidyr, devtools) #estrutura do banco
library(dplyr)
library(xlsx) #exportar df para excel NÃO CONSEGUI INSTALAR

# --------- Recife Bom de Bola --------- #
#banco
bbola = read.csv("cultura_lazer/jogadores_recbola.csv")
str(bbola)

#redefinir tipo variaveis
bbola$MODALIDADE_EQUIPE = as.character(bbola$MODALIDADE_EQUIPE)
bbola$BAIRRO_JOGADOR = as.character(bbola$BAIRRO_JOGADOR)
bbola$DTNASCIMENTO_JOGADOR = as.character(bbola$DTNASCIMENTO_JOGADOR)

# *.*.*. Idade *.*.*. #
#Ano nascimento
bbola$ano_nasc = as.numeric(str_sub(bbola$DTNASCIMENTO_JOGADOR, 1,4))

#calcular idade 
bbola$idade = as.numeric(2019-bbola$ano_nasc)

#frequencia por idade
id = data.frame(table(bbola$idade))
colnames(id) = c('idade', 'frequencia')
id$idade = as.numeric(as.character(id$idade))

id_15 = id %>% filter(idade < 15)
id_15 = mutate(id_15, fx_etaria = "Menos de 15 anos")
id_24 = id %>% filter(idade %in% 15:24)
id_24 = mutate(id_24, fx_etaria = "15 a 24 anos")
id_45 = id %>% filter(idade %in% 25:44)
id_45 = mutate(id_45, fx_etaria = "25 a 44 anos")
id_63 = id %>% filter(idade %in% 45:63)
id_63 = mutate(id_63, fx_etaria = "45 anos ou mais")

id = rbind (id_15, id_24, id_45, id_63)

#calcular jogadores por faixa
etaria = id %>%
  group_by(fx_etaria) %>%
  summarise(frequencia_fx = sum(frequencia))

#definir tipo de variaveis
etaria$frequencia_fx = as.numeric(as.character(etaria$frequencia_fx))
etaria$fx_etaria = as.factor(etaria$fx_etaria)
etaria$fx_etaria = factor(etaria$fx_etaria,
                          levels = c("Menos de 15 anos","15 a 24 anos",
                                     "25 a 44 anos","45 anos ou mais"))
#calcular percentual
etaria = etaria %>% mutate(percentual = as.numeric((frequencia_fx/sum(frequencia_fx)*100)))

#----- graficos -----#
###### g1 - Jogador por idade
g1_id = ggplot(id, aes(idade, frequencia))+
  geom_line()+
  scale_x_continuous(labels = id$idade, breaks = id$idade)+
  ylim(0,500)+
  labs(x = "Idade",
       y = "Número de jogadores",
       caption = "Elaboração própria | Dados: Dados Recife")
g1_id + ggsave("jogadores_idade.png",
               path = "cultura_lazer/graficos e mapas",
               width = 7, height = 4, units = "in")

#compartilhar plotly
p1_id = ggplotly(g1_id)
plot_ly(p1_id, layout(title = 'Jogadores do Recife Bom de Bola'))
chart_link = api_create(p1_id, filename='jogadores_recife')
chart_link

###### g2 - Jogador por faixa etaria
g2_id  = ggplot(etaria, aes(fx_etaria, percentual))+
  geom_bar(stat = "identity")+
  ylim(0,100)+
  labs(x = "Faixa etária",
       y = "% de jogadores",
       caption = "Elaboração própria | Dados: Dados Recife")+
  geom_text(aes(label = round(percentual,0)), hjust =0.5, vjust=-1,
            position = position_dodge(0.9), size=3.5, colour = 'grey30')+
  tema_massa()

g2_id + ggsave("jogadores_etaria.png",
               path = "cultura_lazer/graficos e mapas",
               width = 7, height = 4, units = "in")

# *.*.*. Bairros *.*.*. #
bairros = bbola %>%
  group_by(BAIRRO_JOGADOR) %>%
  count(BAIRRO_JOGADOR)

#Substituir variaveis vazias - NÃO FUNCIONOU
levels(bairros$BAIRRO_JOGADOR)[levels(bairros$BAIRRO_JOGADOR) == " "] = "NÃO IDENTIFICADO"

#Excluir pernambuco
bairros = bairros %>%
  filter(BAIRRO_JOGADOR != "PERNAMBUCO")

#redefinir nome coluna
colnames(bairros) = c('Bairros', 'Num_jogador')

#EXPORTAR DF PARA GERAR MAPA
write.csv(bairros, "bairros_bomdebola.csv", row.names = FALSE)


