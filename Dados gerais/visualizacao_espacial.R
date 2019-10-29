#====================================================#
# DIAGNOSTICO DA CONDICAO JUVENIL NO RECIFE          #              
#====================================================#
# SAUDE                                              #
#----------------------------------------------------#
# Prefeitura da Cidade do Recife                     #
# Secretaria Executiva de Juventude                  #
#----------------------------------------------------#
# Recife 2019                                        #
# @claudio alves monteiro                            #
#----------------------------------------------------#


#==============================#
# configuracoes
#==============================#

# carregar pacotes
pacotes <- c("ggrepel","readr","readxl", "stringr", 
             "dplyr", "viridis", "maps", "raster",
             "ggmap", "ggrepel", "sp", "maptools",
             'ggplot2')

lapply(pacotes, library, character.only = T)

# Tema para Graficos
tema_massa <- function (base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text.x = element_text(size=12,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(size=12,angle=0,hjust=1,vjust=0,face="plain"), 
          axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title.y = element_text(colour="black",size=12,angle=90,hjust=0.5,vjust=0.6,face="plain"),
          title = element_text(colour="black",size=14,angle=0,hjust=.5,vjust=.5,face="plain"))
}

#==============================#
# importar dados

# limpar string
cleanStr <- function(string){
  library(stringi); library(stringr);
  string = stri_trans_general(string, "latin-ascii")
  string = toupper(string)
  return(string)
}



#===== Funcao p/ Mapa =====#
mapa.funcao <- function(shape, data, variable, maintitle, legendtitle) { 
  library(stringi); library(ggplot2)

  data$variavel = variable
  
  # merge data with shapefile
  shp_data <- merge(shape, data, by = "EBAIRRNOME", all = T)
  
  # definir labels no mapa (3 maiores, 3 menores)
  shp_data$variavel[is.na(shp_data$variavel)] = 0
  shp_data = shp_data[order(shp_data$variavel),]
  shp_data$bairros_detasq = 1
  #shp_data$bairros_detasq[1:4] = ""
  shp_data$bairros_detasq[c(length(shp_data)-5):c(length(shp_data))] = ""
  shp_data$bairros_detasq = with(shp_data, paste0(shp_data$bairros_detasq, shp_data$EBAIRRNOME))
  shp_data$bairros_detasq_cod = grepl(shp_data$bairros_detasq, pattern = "1")
  shp_data$bairros_detasq[shp_data$bairros_detasq_cod == TRUE ] = ""
  
  # tranformar shapefile em polygonsdataframe
  data_fortity = fortify(shp_data, region = "EBAIRRNOME")
  localidade = shp_data@data$EBAIRRNOME
  # extrair centroides dos poligonos
  centroids.df = as.data.frame(coordinates(shp_data))
  names(centroids.df) = c("Longitude", "Latitude")  #more sensible column localidades
  # base para plotagem
  variavel = shp_data@data$variavel
  nomes_centroides = shp_data$bairros_detasq
  map_dataframe = data.frame(localidade, variavel, centroids.df, nomes_centroides)
  
  plot = ggplot(data = map_dataframe, aes(map_id = localidade)) + 
    geom_map(aes(fill = shp_data$variavel),colour = grey(0.96),  map = data_fortity) +
    expand_limits(x = data_fortity$long, y = data_fortity$lat) +
    scale_fill_viridis(name = legendtitle, option = 'B') +
    # scale_fill_gradient(name = legendtitle, low="lightgreen", high= "darkblue")+
    geom_label_repel(aes(label = nomes_centroides, x = Longitude, y = Latitude), size = 2.5, color = "black") +
    labs(title = maintitle, x = "Longitude", y = "Latitude")+
    coord_fixed(1) +
    theme_nothing(legend = T)+
    theme(#legend.position="bottom",
          #legend.key.size = unit(0.7, "cm"),
          #legend.text = element_text(size = 14, hjust = 3, vjust = 3)
          #legend.title = element_text(size = 15, face = "plain"),
          #title = element_text(size = 15, face = "bold")
      )
  
  return(plot)
}
