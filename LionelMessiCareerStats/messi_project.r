ggplot(df,aes(x=Season, y=Goal_assist))

#separar o result em Home e Away
#escolher padrao pra season e pra data -- ok
#TRATAMENTO DA DATA: IF < 8 CARACTERES: ADD 0 A ESQUERDA
#DPS INVERTER A DATA
#dar select distinct na season

install.packages("tidyverse")     #instalando tidyverse
install.packages("readxl")        #install pra ler excel
install.packages("rmarkdown")     #instalando markdown   


# 1 passo
library(dplyr)                    #library de analise
library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)

# 2 passo
df <- read.csv("C:/Users/path/path/path/messi/messi.csv") 

# 3 passo
View(df) #abre o df

# 4 passo 
str(df)   #mostra toda a estrutura do df
unique(df$Season)  

# 5 passo 

df$Season <- gsub("Dec-13", "13/14", df$Season) # substituir os dados
df$Season <- gsub("11-Dec", "11/12", df$Season) # substituir os dados 

# season limpa

#impar a data agora

df$Date <- ifelse(nchar(df$Date) < 8, paste0("0", df$Date), df$Date) #adicionando 0 nos meses que nao tem no inicio do formato MDY
df$Date <- gsub("/", "-", df$Date)
df$Date <- as.Date(df$Date, format = "%m-%d-%y")  # Converter para o formato de data do R
df$Date <- format(df$Date, "%Y-%m-%d")            # Converter de volta para uma string no formato YYYY-MM-DD



#separando Home e Away do resultado

# Separar os valores em duas colunas
df <- cbind(df, do.call(rbind, strsplit(as.character(df$Result), ":")))

# Renomear as colunas resultantes
colnames(df)[14:15] <- c("Home", "Away")

#limpar o placar

df$Away <- gsub("2 AET", "2", df$Away)
df$Away <- gsub("4 AET", "4", df$Away)
df$Away <- sub("^0", "", df$Away) #removendo 0 a esquerda

#removendo placar completo
df <- subset(df, select = -c(Result))

#criando uma coluna com a vitoria, derrota ou empate

df$Result <- ifelse((df$Home > df$Away & df$Venue == "H") | (df$Home < df$Away & df$Venue == "A"), "Win", 
                    ifelse((df$Home < df$Away & df$Venue == "H") | (df$Home > df$Away & df$Venue == "A"), "Loss", 
                           "Tie"))


# gols x oponente
# gols x tipo de gol
# gols em vitoria, empate e derrota
# gols X temporada
# gols em cada dia do ano/mes 
# gols em cada minuto
# gols X competicao
# jogadores que mais assistiram
# gols fora de casa
# gols em liga
# gols em mata mata (last 16, ..)

#acrescentar o type NULL -> MANO DE DIOS
df$Type[df$Type == ""] <- "MANO DE D10S"


# Criar o gráfico de pizza
ggplot(df, aes(x = "", fill = Type)) +
  geom_bar(width = 1, color = "black") +
  coord_polar("y", start = 0) +
  labs(fill = "Tipo de Gol") +
  theme_void()  # Remove elementos de tema padrão (como grade e eixos)

# Plotar o gráfico de barras com rótulos do eixo x rotacionados
ggplot(df, aes(x = Opponent)) +
  geom_bar() +
  labs(x = "Oponente", y = "Quantidade de Jogos", title = "Contagem de Jogos por Oponente") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotacionar os rótulos do eixo x

df$Date <- ymd(df$Date) # tipo chr para tipo Date

df$Home <- as.numeric(as.character(df$Home))  #chr para int
df$Away <- as.numeric(as.character(df$Away))  #chr para int



df$Injury_Time <- as.integer(ifelse(grepl("\\+", df$Minute), sub(".*\\+(\\d+)", "\\1", df$Minute), NA))   #criando nova coluna com os acrescimos
df$Minute <- sub("\\+\\d+", "", df$Minute) # Substituir os valores após o "+" por uma string vazia na coluna "Minute"

df$Minute <- as.numeric(as.character(df$Minute))  #chr para int

df$Goal_assist <- sub("", "No Assist", df$Goal_assist) #tirando os valores em branco do campo de Assistencia
df$Goal_assist <- sub("^No Assist(.+)$", "\\1", df$Goal_assist) #arrumando a cagada e tirando excesso de No assist




