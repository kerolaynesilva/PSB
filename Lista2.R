# Processamento de Sinais Biomédicos
# Prof. Adriano de Oliveira Andrade, PhD
# adriano@ufu.br
# 

# Instação de pacotes -----------------------------------------------------

# Antes de instalar os packages execute o RStudio como Administrador no Windows

# install.packages("openxlsx")
# install.packages("ggplot2")
# install.packages("edfReader")


# Uso de bibliotecas ------------------------------------------------------
library(openxlsx) #carrega a biblioteca "openxlsx"
library(ggplot2)
library(edfReader)


# Definição do diretório de trabalho

getwd() # get current working directory
setwd("C:/Users/samsung/Documents/Universidade/PSB/M2/Programas/")


# Abrindo arquivo a partir do Excel (formato .xlsx)

help(read.xlsx)

df1 <- read.xlsx("G2.xlsx", sheet = 1, skipEmptyRows = FALSE)

p <- qplot(df1$`[Time]`, df1$`[G1.Y]`, geom=c("point", "line"))
p <- p + labs(x = "tempo (s)", y="amplitude")
print(p)


# Lendo o arquivo texto na forma de uma tabela
df2 <- read.table("G2.txt",header = TRUE, sep =  "\t", skip = 1)

p2 <- qplot(df2$X.Time., df2$X.G1.X., geom=c("point", "line"))
p2 <- p2 + labs(x = "tempo (s)", y="amplitude")
print(p2)


# Lendo o arquivo csv (com separador em ;)
df3 <- read.csv2("G2.csv",header = TRUE, sep =  ";", skip = 1)

p3 <- qplot(df3$X.Time., df3$X.G1.X., geom=c("point", "line"))
p3 <- p3 + labs(x = "tempo (s)", y="amplitude")
print(p3)


# Lendo o arquivo EDF
CHdr <- readEdfHeader("G2.edf") # leitura do cabeçalho
Signals <- readEdfSignals(CHdr) # leitura dos sinais

tt = seq(from=0, to=3049, by=1) * 1/Signals$`Ch: 0-:G1.X`$sRate

length(tt)

df4 <- data.frame(time= tt, amp=Signals$`Ch: 1-:G1.Y`$signal)

p4 <- qplot(df4$time, df4$amp, geom=c("point", "line"))
p4 <- p4 + labs(x = "tempo (s)", y="amplitude")
print(p4)




