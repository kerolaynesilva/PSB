# Processamento de Sinais Biomédicos
# Prof. Adriano de Oliveira Andrade, PhD
# adriano@ufu.br
# 

# Instação de pacotes -----------------------------------------------------

# Antes de instalar os packages execute o RStudio como Administrador no Windows

 # install.packages("openxlsx")
 # #install.packages("ggplot2")
 # install.packages("edfReader")
 # install.packages("plotly") #https://plot.ly/r/
 # install.packages("gridExtra")
 # install.packages("dygraphs")
 # install.packages("xts")
 # install.packages("tidyquant")
 # install.packages("spgs")
 # install.packages("moments")
 # install.packages("tidyquant")
 #  install.packages("pracma")
 # install.packages("htmltools")

# Uso de bibliotecas ------------------------------------------------------
library(openxlsx) #carrega a biblioteca "openxlsx"
library(edfReader)
library(dygraphs)
library(pracma) #package para usar a função findpeaks
library(htmltools)


# Definição do diretório de trabalho

 getwd() # get current working directory
 setwd("C:/Users/samsung/Documents/Universidade/PSB/M5")
 
 df1 <- read.xlsx("MMG_G2.xlsx", sheet = 1, skipEmptyRows = FALSE)
 
 
 dt <- df1$Tempo[2] - df1$Tempo[1] # cálculo da resolução temporal em segundos
 
 dygraph(data.frame(time=df1$'[Time]', y1 = df1$'[EMG1]', y2=df1$'[PULSE.A]'), group="EMG") %>%
   dyRangeSelector()

 th <- max(df1$'[PULSE.A]', na.rm = TRUE) # limiar
 
 # help(findpeaks) --> para saber mais sobre os argumentos da função
 pp <- findpeaks(df1$'[EMG1]', minpeakheight=3.22) #encontrando picos (esta função regorna uma matriz)
 
 indxpeaks <- pp[ , 2] #retorna a segunda coluna da matriz (que contém os índices em que os picos aconteceram)
 
 
 ## Saiba mais em http://dygraphs.com/tests/independent-series.html
 df1$peaks <- NA #inicializando um vetor com dados faltantes (NA = not a number)
 df1$peaks[indxpeaks] <- df1$'[EMG1]'[indxpeaks]
 

 d1 <- dygraph(df1, group="E1") %>% dySeries("peaks",stemPlot=TRUE) %>% dyAxis("y", valueRange = c(-4, 4)) %>% 
   dyLegend(width = 400)%>%  dyLimit(th, "Max", strokePattern = "solid", color = "blue")%>% dyRangeSelector()
 
 d2 <- dygraph(df1[c("[Time]", "peaks")], main="Eventos discretos", group="E1")%>%
   #dyOptions(connectSeparatedPoints=TRUE)%>%
   dySeries("peaks",stemPlot=TRUE)%>%
   dyAxis("y", valueRange = c(-4, 4)) %>% 
   dyLegend(width = 400)%>%  dyLimit(th, "Max", strokePattern = "solid", color = "blue") %>% dyRangeSelector()
 
 dy_graph <- list(d1,d2)
 # render the dygraphs objects using htmltools
 htmltools::browsable(htmltools::tagList(dy_graph)) 

 print(d2) 
 
 
 dtempo <- data.frame(time=df1$`[Time]`) 
 dtempo$peaks <- df1$peaks
 
 
 dt <- data.frame(subset(dtempo, is.na(peaks) == FALSE))
 
 dfr <- data.frame(time=seq(from=1, to=10, by=1))
 dfr$delta_t <-NA
 
for(i in 1:10){
  dfr$delta_t[i]<- dt$time[i+1] - dt$time[i]
}
 
 print(mean(dfr$delta_t)) #media dos intervalos de tempos
 print(sd(dfr$delta_t))   #desvio padrão dos intervalos de tempos

 
 
 
 
 