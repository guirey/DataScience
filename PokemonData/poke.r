library(dplyr)                    #library de analise
library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)

setwd("C:/Users/path/bc_poke") #seta o diretorio de trabalho
df <- read.csv("pokemon.csv") #realiza a importação dos dados CSV para a variavel df

View(df)
View(df_agrupado)
str(df)

#pokemons lendarios x Geraçao
#pokemons x tipo

#pokemons monotipo X dual tipo
#pokemons leves X pesados
#pokemons altos x baixinhos

unique(df$type1)

colnames(df)


# Filtrar o dataframe para obter as linhas onde o nome é igual a "Pikachu"
op <- df[df$name == "Steelix", ]

df$single_type <- ifelse(is.na(df$type2), "sim", "nao") #para caracteres nulos
df$single_type <- ifelse(df$type2 == "", "sim", "nao") #para caracteres espaços