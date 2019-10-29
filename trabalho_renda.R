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
library(tidyr, devtools) #estrutura do banco
library(maptools) #visualização com mapas
library(ggmap)
library(rgdal)
library(xlsx) #exportar df para excel NÃO CONSEGUI INSTALAR
library(dplyr)

#importar pacotes
source("Dados gerais/ferramentas.R")

# layout ggplot (PROVISÓRIO)
tema_massa <- function (base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text.x = element_text(colour="black", size=11,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="black", size=11,angle=0,hjust=1,vjust=0,face="plain"), 
          axis.title.x = element_text(colour="black",size=11,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="black",size=11,angle=90,hjust=0.5,vjust=0.6,face="plain"),
          title = element_text(colour="black",size=14,angle=0,hjust=.5,vjust=.5,face="plain"))
}

#= = = Rendimento (Brasil e Recife) ~ PNADc trimestral
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
    labs(x="Ano",
       y="Rendimento (R$)",
       title = "Rendimento por ano - Brasil",
       subtitle = "Dados: PNAD contínua trimestral",
       caption = "Fonte: Elaboração própria") +
  temamassa()

g1_br = ggplotly(g1_br)

g1_rec = ggplot(renda_ano, aes(x=ano, y=Recife, group = 1)) +
  labs(x="Ano",
       y="Rendimento (R$)",
       title = "Rendimento por ano - Recife",
       subtitle = "Dados: PNAD contínua trimestral",
       caption = "Fonte: Elaboração própria") +
  temamassa()

g1_rec = ggplotly(g1_rec)
print(g1_rec)

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
  labs(x="Ano",
       y="Rendimento (R$)",
       fill= "Faixa etária",
       title = "Rendimento por faixa etária - Brasil",
       subtitle = "Dados: PNAD contínua trimestral",
       caption = "Fonte: Elaboração própria") +
  temamassa()

g12_br = ggplotly(g2_br)

g2_rec = ggplot(renda_etaria, aes(x=ano, y=Recife, group=Idade, colour=Idade)) +
  labs(x="Ano",
       y="Rendimento (R$)",
       fill= "Faixa etária",
       title = "Rendimento por faixa etária - Recife",
       subtitle = "Dados: PNAD contínua trimestral",
       caption = "Fonte: Elaboração própria") +
  temamassa()

g2_rec = ggplotly(g2_rec)
print(g2_rec)

#--------- Rendimento por gênero -------- #

#--------- Rendimento por raça -------- #

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

#exportar tabela em xlsx ?

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

#------ mapa ------#
#importar pacotes
source("Dados gerais/visualizacao_espacial.R")

# carregar shapefile 
shp_recife = shapefile("Dados gerais/Bairros.shp")

mapa.funcao(shp_recife, rendmed.bairros, rendmed.bairros$Total, '', 'R$')

#======== Tema ggplot2 para graficos ========#
temamassa <- function (base_size = 12, base_family = "") {
  theme_minimal(base_size =  base_size, base_family = base_family) %+replace% 
    theme(axis.text.x = element_text(colour= "black",size=11,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="black",size=11,angle=0,hjust=1,vjust=0,face="plain"), 
          axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="black",size=12,angle=90,hjust=0.5,vjust=0.6,face="plain"),
          title = element_text(colour="black",size=14,angle=0,hjust=.5,vjust=.5,face="bold"),
          panel.grid.major = element_line(colour = grey(0.85)), 
          panel.grid.minor = element_line(colour = grey(1)),
          legend.key.size = unit(9, "mm"),
          legend.text = element_text(size = 9, hjust = 3, vjust = 3),
          legend.title = element_text(size = 9),
          axis.line = element_line(size = 1, colour = "grey70"))
  
  #============== Remover acentos ==============#
  
  rm_accent <- function(str,pattern="all") {
    # Rotinas e funções úteis V 1.0
    # rm.accent - REMOVE ACENTOS DE PALAVRAS
    # Função que tira todos os acentos e pontuações de um vetor de strings.
    # Parâmetros:
    # str - vetor de strings que terão seus acentos retirados.
    # patterns - vetor de strings com um ou mais elementos indicando quais acentos deverão ser retirados.
    #            Para indicar quais acentos deverão ser retirados, um vetor com os símbolos deverão ser passados.
    #            Exemplo: pattern = c("´", "^") retirará os acentos agudos e circunflexos apenas.
    #            Outras palavras aceitas: "all" (retira todos os acentos, que são "´", "`", "^", "~", "¨", "ç")
    if(!is.character(str))
      str <- as.character(str)
    
    pattern <- unique(pattern)
    
    if(any(pattern=="Ç"))
      pattern[pattern=="Ç"] <- "ç"
    
    symbols <- c(
      acute = "áéíóúÁÉÍÓÚýÝ",
      grave = "àèìòùÀÈÌÒÙ",
      circunflex = "âêîôûÂÊÎÔÛ",
      tilde = "ãõÃÕñÑ",
      umlaut = "äëïöüÄËÏÖÜÿ",
      cedil = "çÇ"
    )
    
    nudeSymbols <- c(
      acute = "aeiouAEIOUyY",
      grave = "aeiouAEIOU",
      circunflex = "aeiouAEIOU",
      tilde = "aoAOnN",
      umlaut = "aeiouAEIOUy",
      cedil = "cC"
    )
    
    accentTypes <- c("´","`","^","~","¨","ç")
    
    if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
      return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
    
    for(i in which(accentTypes%in%pattern))
      str <- chartr(symbols[i],nudeSymbols[i], str)
    
    return(str)
  }
  