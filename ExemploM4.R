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

# Uso de bibliotecas ------------------------------------------------------
library(openxlsx) #carrega a biblioteca "openxlsx"
library(edfReader)
library(plotly) #https://plot.ly/r/
library(gridExtra)
library(dygraphs)
library(xts)
library(tidyquant)
library(spgs)
library(moments)

# Definição do diretório de trabalho

 getwd() # get current working directory
 setwd("C:/Users/samsung/Documents/Universidade/PSB/M4")
 
# Lendo o arquivo texto na forma de uma tabela
df1 <- read.table("a1.txt",header = FALSE, sep =  " ", skip = 6)

# alteração do nome das variáveis do dataframe
names(df1) <- c("AccX", "AccY", "MuscAnterior","MuscPosterior")

#criando o vetor de tempo

fs <- 500 #Hz

# Definição intervalo entre as amostras em segundos
dt <- 1/fs

# Definição do vetor de tempo (em segundos)
# Para help digite: help("seq")
t <- seq(from=0, to = dt*(length(df1$AccX)-1), by=dt) # Aprenda mais em https://www.datamentor.io/r-programming/vector/

# incluindo o vetor de tempo no dataframe
df1 <- cbind(time=t,df1) 

dygraph(df1) %>% dyRangeSelector()

dygraph(df1[c("time","MuscAnterior")]) %>% dyRangeSelector()

dygraph(df1[c("time","MuscPosterior")]) %>% dyRangeSelector()

dygraph(data.frame(time=1:length(t), MuscPosterior=df1$MuscPosterior)) %>% dyRangeSelector()


A = matrix(

        c(1, 2420, 2765,
          2, 1520, 1963,
          3, 3284, 3654,
          4, 4083, 4425,
          5, 4883, 5249,
          6, 5779, 6123,
          7, 6507, 6910,
          8, 7294, 7680,
          9, 8099, 8445,
          10, 8891, 9308,
          11, 9670, 10089,
          12, 10528, 10906,
          13, 11296, 11671,
          14, 12088, 12470,
          15, 12849, 13233,
          16, 13623, 14011,
          17, 14397, 14808,
          18, 15177, 15556,
          19, 15904, 16332,
          20, 16768, 17028,
          21, 17467, 17867,
          22, 18220, 18608,
          23, 19012, 19395,
          24, 19818, 20206,
          25, 20590, 20996,
          26, 21369, 21704,
          27, 22064, 22512,
          28, 22812, 23189,
          29, 23641, 23969,
          30, 24303, 24704),
        
        nrow=30,
        ncol=3, 
        byrow = TRUE)


colnames(A) = c("Burst", "Inicio", "Fim")


dfr <- data.frame(time= A[,1])
dfr$media <- NA
dfr$mediana <- NA
dfr$quartil_25 <- NA
dfr$quartil_50 <- NA
dfr$quartil_75 <- NA
dfr$percentil_32 <- NA
dfr$percentil_57 <- NA
dfr$percentil_98 <- NA
dfr$amplitude <- NA
dfr$amplitude_interquartil <- NA
dfr$boxplot_1quartil <- NA
dfr$boxplot_3quartil <- NA
dfr$boxplot_mediana <- NA
dfr$boxplot_lim_sup <- NA
dfr$boxplot_lim_inf <- NA
dfr$variancia <- NA
dfr$desvio_padrao <- NA
dfr$coeficiente_de_assimetria <- NA
dfr$curtose <- NA
 

for(i in 1:nrow(A)) {
  # i-th element of `u1` squared into `i`-th position of `usq`
  
  dfr$media[i] <- mean(df1$MuscPosterior[c(A[i,2]:A[i,3])])
  dfr$mediana[i] <- median(df1$MuscPosterior[c(A[i,2]:A[i,3])])
  dfr$quartil_25[i] <- quantile(df1$MuscPosterior[c(A[i,2]:A[i,3])], c(.25))
  dfr$quartil_50[i] <- quantile(df1$MuscPosterior[c(A[i,2]:A[i,3])], c(.50))
  dfr$quartil_75[i] <- quantile(df1$MuscPosterior[c(A[i,2]:A[i,3])], c(.75))
  dfr$percentil_32[i] <- quantile(df1$MuscPosterior[c(A[i,2]:A[i,3])], c(.32))
  dfr$percentil_57[i] <- quantile(df1$MuscPosterior[c(A[i,2]:A[i,3])], c(.57))
  dfr$percentil_98[i] <- quantile(df1$MuscPosterior[c(A[i,2]:A[i,3])], c(.98))
  # dfr$boxplot[i] <- boxplot(df1$MuscPosterior[c(A[i,2]:A[i,3])], horizontal=TRUE) 
  dfr$boxplot_1quartil[i] <- quantile(df1$MuscPosterior[c(A[i,2]:A[i,3])], c(.25)) 
  dfr$boxplot_3quartil[i] <- quantile(df1$MuscPosterior[c(A[i,2]:A[i,3])], c(.75))
  dfr$boxplot_mediana[i] <- median(df1$MuscPosterior[c(A[i,2]:A[i,3])]) 
  
  dfr$boxplot_lim_sup[i] <- median(df1$MuscPosterior[c(A[i,2]:A[i,3])]) + quantile(df1$MuscPosterior[c(A[i,2]:A[i,3])], c(.75)) 
  dfr$boxplot_lim_inf[i] <- median(df1$MuscPosterior[c(A[i,2]:A[i,3])]) - quantile(df1$MuscPosterior[c(A[i,2]:A[i,3])], c(.25))
  
  dfr$amplitude[i] <- max(df1$MuscPosterior[c(A[i,2]:A[i,3])])- min(df1$MuscPosterior[c(A[i,2]:A[i,3])])
  dfr$amplitude_interquartil[i] <- IQR(df1$MuscPosterior[c(A[i,2]:A[i,3])])
  dfr$variancia[i] <- var(df1$MuscPosterior[c(A[i,2]:A[i,3])])
  dfr$desvio_padrao[i] <- sd(df1$MuscPosterior[c(A[i,2]:A[i,3])])
  dfr$coeficiente_de_assimetria[i] <- skewness(df1$MuscPosterior[c(A[i,2]:A[i,3])])
  dfr$curtose[i] <- kurtosis(df1$MuscPosterior[c(A[i,2]:A[i,3])])
}

write.xlsx(dfr, "a1_estatistica.xlsx", colNames = TRUE)

# A = matrix( 
#      c(2, 4, 3, 1, 5, 7), # the data elements 
#      nrow=2,              # number of rows 
#      ncol=3,              # number of columns 
#      byrow = TRUE)        # fill matrix by rows 
# 
# 

dygraph(df1, main = "Sinais coletados", group = "GroupName") %>% dyRangeSelector()
dygraph(df1[c("time", "AccX")], main = "AccX", group = "GroupName") %>% dyRangeSelector()
dygraph(df1[c("time", "AccY")], main = "AccY", group = "GroupName")%>% dyRangeSelector()
dygraph(df1[c("time", "MuscAnterior")], main = "MuscAnterior", group = "GroupName")%>% dyRangeSelector()
dygraph(df1[c("time", "MuscPosterior")], main = "MuscPosterior", group = "GroupName")%>% dyRangeSelector()

# Realizando o cálculo de parâmetros estatísticos para o sinal MuscPosterior

y <- df1$MuscPosterior 

Nwnd <- 100 #tamanho da janela (em número de amostras)

#gerando os índices iniciais e finais de cada janela
indx1 <- seq(from=1, to=length(y), by=Nwnd)

N <- length(indx1) #número de elementos do vetor indx1


dfenv <- data.frame(time=rep(NA,times=N-1))
dfenv$rms <- NA

for(i in 1:(N-1)) {
  #criando um vetor tempo centralizado em cada janela
  dfenv$time[i] <- (indx1[i] + (indx1[i+1]-indx1[i])/2) * dt #tempo em segundos 
  #calculando o valor RMS para cada janela (poderíamos calcular qualquer outro parâmetro)
  dfenv$rms[i]<- rms(y[indx1[i]:indx1[i+1]])
}
dygraph(dfenv) %>% dyRangeSelector()

