library(pracma) 
library(tuneR)
library(dygraphs)
library(htmltools)
library(moments)
library(openxlsx)
library(ggplot2)


#Diret�rios dos comandos:

strPath1 <- "C:/Users/samsung/Documents/Universidade/PET EngBiom�dica/Workshop PET/Programas/DADOS/COMANDOS/G2/MEMBRO1/Um/" 

#Nomes dos arquivos gravados janelados no programa Audacity:

fileNames <- list("1.wav", "2.wav", "3.wav", "4.wav", "5.wav")


# unindo diret�rio aos nomes dos arquivos:

fileNames1 <- as.list(paste(strPath1,fileNames, sep = "")) 

#Fun��o que plota sinais .wav
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
  
  # a fun��o basename extrai o nome do arquivo apenas
  # a fun��o dirname extrai o caminho completo
  pp <- dygraph(df, main= basename(fname))
  
  return(pp)
}

#Aplica uma fun��o a uma lista para plotar os gr�ficos:
dy_graph_1 <- list(lapply(fileNames1, plotWAV))  # end list
htmltools::browsable(htmltools::tagList(dy_graph_1))

#Gr�ficos para an�lise de periodicidade do sinal de a�dio-----------------------------------------------
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

#�udio n�mero 1
autocorrelacao(fileNames1[[1]])


#Gr�ficos para an�lise da estacionaridade do sinal de a�dio-----------------------------------------------

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

#�udio n�mero 1
estacionario(fileNames1[[1]])
