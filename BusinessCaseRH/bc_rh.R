install.packages("tidyverse")     
install.packages("readxl")        
install.packages("rmarkdown")      
library(dplyr)                    
library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)

df <- read_excel("C:/Users/path/path/path/Base_dados - Rotatividade RH.xlsx", sheet=1) #importa dados do excel

head(df)    #carrega apenas as 6 primeiras linhas do df
tail(df)    #carrega as 6 ultimas linhas do df
str(df)     #mostra toda a estrutura do df

df$Data_Contratacao <- ymd(df$Data_Contratacao)       #convertendo POSIXct para Date
df$Data_Desligamento <- ymd(df$Data_Desligamento)     #convertendo POSIXct para Date

df_ordenado <- df[order(-df$Salario), ]
maiores_salarios <- head(df_ordenado, 10)
menores_salarios <- tail(df_ordenado, 10)
View(menores_salarios)
View(maiores_salarios)
#4/10 em desligamentos em ambos os casos, nao tem muito a ver com salari

print(tail(df[order(-df$Pontuacao_Desempenho), ], 10)) 




# Calcular a média de desligamentos
media_desligamentos <- mean(df$Meses_de_Servico)

# Criar o gráfico de colunas
ggplot(df, aes(x = Meses_de_Servico)) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Distribuição dos Desligamentos em relação ao Tempo de Contribuição",
       x = "Meses de Serviço",
       y = "Número de Desligamentos") +
  # Adicionar linha de tendência horizontal
  geom_hline(yintercept = media_desligamentos, linetype = "dashed", color = "red") +
  # Ajustar rótulos dos eixos para uma escala de 5 em 5
  scale_x_continuous(breaks = seq(0, max(df$Meses_de_Servico), by = 5)) +
  scale_y_continuous(breaks = seq(0, max(table(df$Meses_de_Servico)), by = 5)) +
  theme_minimal()


# ===================   %% =========== %% ================= #
# Analise Cargo X Idade X Qtd de desligamentos
# Perfil de desligamentos

# Plotar o gráfico
ggplot(df, aes(x = Idade, fill = Nivel_Cargo)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Quantidade de Desligamentos por Idade e Nível de Cargo",
       x = "Idade",
       y = "Quantidade de Desligamentos") +
  scale_fill_manual(values = c("Junior" = "blue", "Pleno" = "green", "Sênior" = "red")) +
  theme_minimal()


# Contar a quantidade de ocorrências de 1 na coluna "Desligamentos"
quantidade_desligamentos <- sum(df$Desligamento == 1)
# Mostrar o resultado
print(quantidade_desligamentos) #144 desligamentos de 506 nao desligamentos

#valores unicos
unique(df$Nivel_Cargo)

# Visualizar os 10 valores mínimos Desempenho
minimos_des <- head(sort(df$Pontuacao_Desempenho), 10)
# Visualizar os 10 valores máximos Desempenho
maximos_des <- tail(sort(df$Pontuacao_Desempenho), 10)

# Visualizar os 10 valores mínimos Satisfacao
minimos <- head(sort(df$Satisfacao_Trabalho), 10)
# Visualizar os 10 valores máximos Satisfacao
maximos <- tail(sort(df$Satisfacao_Trabalho), 10)

print(maximos)
print(minimos)
