#REPOSITÓRIO DE UTILIDADES

# ---- Analise de dados ---- #
#Mudar estilo do banco de dados (tidy para "key-value")
df = gather(df, key = Categorias, value = Valores, 3:4) #Colunas a serem alteradas)

#Unir tabelas rbind para mesmas colunas/ cbind para mesmas linhas
Novo_df = rbind(df1, df2, df3)

#Incluir coluna
eleitorado_recife = mutate(eleitorado_recife,
                           Perc_eleit = (Eleitorado / Pop))

#Definir quantidade de decimais
eleitorado_recife$Perc_eleit = as.numeric(format(eleitorado_recife$Perc_eleit,
                                                 digits = 2, format = "f"))
# ~*~ Stringr ~*~ #
df$var = str_replace(df$var, "\\.", " ") #replace_all se o caracter se repetir
df$var = str_remove(df$var, "Setembro")

# ---- DataViz ----#
#Definir intervalo dos eixos
ylim (0.5, 1) #(conagem inicial, contagem final)

