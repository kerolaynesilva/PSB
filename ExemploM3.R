# Processamento de Sinais Biomédicos
# Prof. Adriano de Oliveira Andrade, PhD
# adriano@ufu.br
# 

# Instação de pacotes -----------------------------------------------------

# Antes de instalar os packages execute o RStudio como Administrador no Windows

# install.packages("openxlsx")
# install.packages("ggplot2")
# install.packages("edfReader")
# install.packages("plotly")


# Uso de bibliotecas ------------------------------------------------------
library(openxlsx) #carrega a biblioteca "openxlsx"
library(ggplot2)
library(edfReader)
library(plotly)

# Definição do diretório de trabalho

getwd() # get current working directory
setwd("D:/Adriano/OneDrive/Adriano/Teaching/PSB/ListaDeExercicios/M3/Programas")

# Definição da frequência de amostragem em Hz
fs1 <- 50

# Definição intervalo entre as amostras em segundos

dt1 <- 1/fs1

# Definição do vetor de tempo (em segundos)
# Para help digite: help("seq")
t1 <- seq(0, 10, by=dt1) # Aprenda mais em https://www.datamentor.io/r-programming/vector/

# Definição de um sinal senoidal
f1 <- 3 # frequência de oscilação da senoide
y1 <- sin(2*pi*f1*t1)

# Criando um data frame (é muito importante trabalhar com data frames no R!)

df1 <- data.frame(t1, y1) # ler http://adv-r.had.co.nz/Data-structures.html

# Alterando a frequência de amostragem para 25 Hz
fs2 <- 25 # frequência de amostragem em Hz
dt2 <- 1/fs2 # resolução temporal em segundos
t2 <- seq(0, 10, by=dt2) # tempo
f2 <- 3 # frequência de oscilação da senoide
y2 <- sin(2*pi*f2*t2)
df2 <- data.frame(t2, y2) # Criando um data frame

# Alterando a frequência de amostragem para 5 Hz
fs3 <- 5 # frequência de amostragem em Hz
dt3 <- 1/fs3 # resolução temporal em segundos
t3 <- seq(0, 10, by=dt3) # tempo
f3 <- 3 # frequência de oscilação da senoide
y3 <- sin(2*pi*f3*t3)
df3 <- data.frame(t3, y3) # Criando um data frame

g <- ggplot(data=df1, aes(x=df1$t1, y=df1$y1)) + geom_point() + geom_line() + 
  geom_point(data = df2, aes(x=df2$t2, y=df2$y2), color = "red", shape = 25, size=3) + 
  geom_line(data = df2, aes(x=df2$t2, y=df2$y2), color='red', size=2, alpha=0.4) + 
  geom_point(data = df3, aes(x=df3$t3, y=df3$y3), color = "green", shape = 25, size=3) + 
  geom_line(data = df3, aes(x=df3$t3, y=df3$y3), color='green', size=2, alpha=0.4)
g <- g + labs(x = "tempo (s)", y="amplitude")
g <- g + theme_bw() #leia https://ggplot2.tidyverse.org/reference/ggtheme.html
print(g)

gg1 <- ggplotly(g)

print(gg1)


#Para saber mais sobre os tipos de pontos
#http://www.sthda.com/english/wiki/ggplot2-point-shapes


# Interpolação ------------------------------------------------------------

#https://stat.ethz.ch/R-manual/R-devel/library/stats/html/splinefun.html
df4 <- data.frame(spline(x=df3$t3, y = df3$y3, xout = df1$t1))

g <- ggplot(data=df3, aes(x=df3$t3, y=df3$y3)) + geom_point() + geom_line() +
  geom_point(data = df4, aes(x=df4$x, y=df4$y), color = "red", shape = 25, size=3) +
  geom_line(data = df4, aes(x=df4$x, y=df4$y), color='green', size=2, alpha=0.4)
g <- g + labs(x = "tempo (s)", y="amplitude")
g <- g + theme_bw() #leia https://ggplot2.tidyverse.org/reference/ggtheme.html
print(g)

gg1 <- ggplotly(g)

print(gg1)