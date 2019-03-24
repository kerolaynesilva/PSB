library(pracma) 
library(tuneR)
library(dygraphs)
library(htmltools)
library(moments)
library(openxlsx)
library(ggplot2)


#Diretórios dos comandos:

strPath1 <- "C:/Users/samsung/Documents/Universidade/PET EngBiomédica/Workshop PET/Programas/DADOS/COMANDOS/G2/MEMBRO1/Um/" 

#Nomes dos arquivos gravados janelados no programa Audacity:

fileNames <- list("1.wav", "2.wav", "3.wav", "4.wav", "5.wav")


# unindo diretório aos nomes dos arquivos:

fileNames1 <- as.list(paste(strPath1,fileNames, sep = "")) 

#Função que plota sinais .wav
plotWAV <- function(fname){
  
  originalSound <- readWave(fname)
  #play(originalSound) # opens Windows Media Player to play the sound. Beware that the window does not automatically close when the audio file is finished playing. You need to close the window before you can continue in R.
  
  fs = originalSound@samp.rate # sampling frequency in Hz
  dt = 1/fs #resolution time in seconds
  
  yl <- originalSound@left
  yr <- originalSound@right
  
  tf <- (length(yl)-1)*dt # tempo final em segundos
  t <- seq(from=0, to=tf, by=dt)
  
  df <- data.frame(time=t, yl, yr)
  
  # a função basename extrai o nome do arquivo apenas
  # a função dirname extrai o caminho completo
  pp <- dygraph(df, main= basename(fname))
  
  return(pp)
}

#Aplica uma função a uma lista para plotar os gráficos:
dy_graph_1 <- list(lapply(fileNames1, plotWAV))  # end list
htmltools::browsable(htmltools::tagList(dy_graph_1))

#Gráficos para análise de periodicidade do sinal de aúdio-----------------------------------------------
autocorrelacao <- function(fname){
  
  originalSound <- readWave(fname)
  #play(originalSound) # opens Windows Media Player to play the sound. Beware that the window does not automatically close when the audio file is finished playing. You need to close the window before you can continue in R.
  
  fs = originalSound@samp.rate # sampling frequency in Hz
  dt = 1/fs #resolution time in seconds
  
  yl <- originalSound@left
  yr <- originalSound@right
  
  tf <- (length(yl)-1)*dt # tempo final em segundos
  t <- seq(from=0, to=tf, by=dt)
  
  df <- data.frame(time=t, yl, yr)
  
  aa <- acf(yl,lag.max = 1000, plot = FALSE)
  
  return(acf(yr,lag.max = 1000, plot = TRUE))
}

#Áudio número 1
autocorrelacao(fileNames1[[1]])


#Gráficos para análise da estacionaridade do sinal de aúdio-----------------------------------------------

estacionario <- function(fname){
  
  originalSound <- readWave(fname)
  #play(originalSound) # opens Windows Media Player to play the sound. Beware that the window does not automatically close when the audio file is finished playing. You need to close the window before you can continue in R.
  
  fs = originalSound@samp.rate # sampling frequency in Hz
  dt = 1/fs #resolution time in seconds
  
  yl <- originalSound@left
  yr <- originalSound@right
  
  tf <- (length(yl)-1)*dt # tempo final em segundos
  t <- seq(from=0, to=tf, by=dt)
  
  df <- data.frame(time=t, yl, yr)
  
  return(acf(yr, plot = TRUE))
}

#Áudio número 1
estacionario(fileNames1[[1]])
