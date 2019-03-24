# Instação de pacotes -----------------------------------------------------

# Antes de instalar os packages execute o RStudio como Administrador no Windows

# install.packages("openxlsx")
# install.packages("ggplot2")
# install.packages("edfReader")

# Uso de bibliotecas ------------------------------------------------------
#carrega a biblioteca "openxlsx"
library(openxlsx)
library(ggplot2)
library(edfReader)
library(dygraphs)

# Definição do diretório de trabalho

getwd() # get current working directory
setwd("C:/Users/samsung/Documents/Universidade/PET EngBiomédica/Workshop PET/Programas/")

#Lendo arquivos do tipo xlsx, txt, csv e EDF -------------------------------
# Abrindo arquivo a partir do Excel (formato .xlsx)
df1 <- read.xlsx("G2.xlsx", sheet = 1, skipEmptyRows = FALSE)

# Lendo o arquivo texto na forma de uma tabela (formato .txt)
df2 <- read.table("G2.txt",header = TRUE, sep =  "\t", skip = 1)

# Lendo o arquivo csv (com separador em ;)
df3 <- read.csv2("G2.csv",header = TRUE, sep =  ";", skip = 1)

# Lendo o arquivo EDF
CHdr <- readEdfHeader("G2.edf") # leitura do cabeçalho
Signals <- readEdfSignals(CHdr) # leitura dos sinais

tt <- seq(from=0, to=length(Signals[[1]][[19]])-1, by=1)

df4 <- data.frame(time= tt, G1.X=Signals[[1]][[19]])

#Algumas formas de gerar gráficos estáticos -------------------------------
#gerando gráfico no formato rápido
qplot(df1$`[Time]`, df1$`[G1.X]`, data = df1) 
qplot(df1$`[Time]`, df1$`[G1.X]`, geom=c("point", "line"))


# Outras formas de gerar gráficos (mais versáteis)
ggplot(data=df2, aes(x=df2$X.Time., y=df2$X.G1.X.)) + geom_line()
ggplot(data=df2, aes(x=df2$X.Time., y=df2$X.G1.X.)) + geom_line() + geom_point()

# Adicionando nome aos eixos do gráfico
p3 <- qplot(df3$X.Time., df3$X.G1.X., geom=c("point", "line"))
p3 <- p3 + labs(x = "tempo (s)", y="amplitude")
print(p3)


#Gráficos dinâmicos 
dygraph(df4) %>% dyRangeSelector()

