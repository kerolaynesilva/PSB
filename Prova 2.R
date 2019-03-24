
#Questão 3:

library(pracma) 
library(tuneR)
library(dygraphs)
library(htmltools)
library(moments)
library(openxlsx)
library(ggplot2)


#Diretórios dos comandos:

strPath1 <- "C:/Users/samsung/Documents/Universidade/PSB/Prova2/DADOS/COMANDOS/G2/MEMBRO1/Um/" 
strPath2 <- "C:/Users/samsung/Documents/Universidade/PSB/Prova2/DADOS/COMANDOS/G2/MEMBRO1/Dois/"
strPath3 <- "C:/Users/samsung/Documents/Universidade/PSB/Prova2/DADOS/COMANDOS/G2/MEMBRO1/Três/"
strPath4 <- "C:/Users/samsung/Documents/Universidade/PSB/Prova2/DADOS/COMANDOS/G2/MEMBRO1/Quatro/"
strPath5 <- "C:/Users/samsung/Documents/Universidade/PSB/Prova2/DADOS/COMANDOS/G2/MEMBRO1/Cinco/"
strPath6 <- "C:/Users/samsung/Documents/Universidade/PSB/Prova2/DADOS/COMANDOS/G2/MEMBRO1/Seis/"
strPath7 <- "C:/Users/samsung/Documents/Universidade/PSB/Prova2/DADOS/COMANDOS/G2/MEMBRO1/Sete/"
strPath8 <- "C:/Users/samsung/Documents/Universidade/PSB/Prova2/DADOS/COMANDOS/G2/MEMBRO1/Oito/"
strPath9 <- "C:/Users/samsung/Documents/Universidade/PSB/Prova2/DADOS/COMANDOS/G2/MEMBRO1/Nove/"
strPath10 <- "C:/Users/samsung/Documents/Universidade/PSB/Prova2/DADOS/COMANDOS/G2/MEMBRO1/Dez/"


#Nomes dos arquivos gravados janelados no programa Audacity:

fileNames <- list("1.wav", "2.wav", "3.wav", "4.wav", "5.wav")


# unindo diretório aos nomes dos arquivos:

fileNames1 <- as.list(paste(strPath1,fileNames, sep = "")) 
fileNames2 <- as.list(paste(strPath2,fileNames, sep = ""))
fileNames3 <- as.list(paste(strPath3,fileNames, sep = ""))
fileNames4 <- as.list(paste(strPath4,fileNames, sep = ""))
fileNames5 <- as.list(paste(strPath5,fileNames, sep = ""))
fileNames6 <- as.list(paste(strPath6,fileNames, sep = ""))
fileNames7 <- as.list(paste(strPath7,fileNames, sep = ""))
fileNames8 <- as.list(paste(strPath8,fileNames, sep = ""))
fileNames9 <- as.list(paste(strPath9,fileNames, sep = ""))
fileNames10 <- as.list(paste(strPath10,fileNames, sep = ""))


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

dy_graph_2 <- list(lapply(fileNames2, plotWAV))  # end list
htmltools::browsable(htmltools::tagList(dy_graph_2))

dy_graph_3 <- list(lapply(fileNames3, plotWAV))  # end list
htmltools::browsable(htmltools::tagList(dy_graph_3))

dy_graph_4 <- list(lapply(fileNames4, plotWAV))  # end list
htmltools::browsable(htmltools::tagList(dy_graph_4))

dy_graph_5 <- list(lapply(fileNames5, plotWAV))  # end list
htmltools::browsable(htmltools::tagList(dy_graph_5))

dy_graph_6 <- list(lapply(fileNames6, plotWAV))  # end list
htmltools::browsable(htmltools::tagList(dy_graph_6))

dy_graph_7 <- list(lapply(fileNames7, plotWAV))  # end list
htmltools::browsable(htmltools::tagList(dy_graph_7))

dy_graph_8 <- list(lapply(fileNames8, plotWAV))  # end list
htmltools::browsable(htmltools::tagList(dy_graph_8))

dy_graph_9 <- list(lapply(fileNames9, plotWAV))  # end list
htmltools::browsable(htmltools::tagList(dy_graph_9))

dy_graph_10 <- list(lapply(fileNames10, plotWAV))  # end list
htmltools::browsable(htmltools::tagList(dy_graph_10))

#Tabela para análise de processo ergódico-----------------------------------------------
#Cálculo da média, variância, coeficiente de assimetria e curtose de cada sinal

d_frame <- function(fname){
  
  originalSound <- readWave(fname)
  #play(originalSound) # opens Windows Media Player to play the sound. Beware that the window does not automatically close when the audio file is finished playing. You need to close the window before you can continue in R.
  
  fs = originalSound@samp.rate # sampling frequency in Hz
  dt = 1/fs #resolution time in seconds
  
  yl <- originalSound@left
  yr <- originalSound@right
  
  tf <- (length(yl)-1)*dt # tempo final em segundos
  t <- seq(from=0, to=tf, by=dt)
  
  df <- data.frame(time=t, yl, yr)
  
  return(df)
}


tabela <- function(fname){
  
  dfr <- data.frame(time= seq(from=1, to = 5, by=1))
  dfr$media <- NA
  dfr$variancia <- NA
  dfr$coeficiente_de_assimetria <- NA
  dfr$curtose <- NA
  
for(i in 1:5) {
  # i-th element of `u1` squared into `i`-th position of `usq`
  
  dfr$media[i] <- mean(d_frame(fname[[i]])$yr)
  dfr$variancia[i] <- var(d_frame(fname[[i]])$yr)
  dfr$coeficiente_de_assimetria[i] <- skewness(d_frame(fname[[i]])$yr)
  dfr$curtose[i] <- kurtosis(d_frame(fname[[i]])$yr)
  
}

dfr_variancia <- data.frame(time= seq(from=6, to = 6, by=1))
dfr_variancia$media[1] <- var(dfr$media[1:5])
dfr_variancia$variancia[1] <- var(dfr$variancia[1:5])
dfr_variancia$coeficiente_de_assimetria[1] <- var(dfr$coeficiente_de_assimetria[1:5])
dfr_variancia$curtose[1] <- var(dfr$curtose[1:5])

linha <- data.frame(dfr_variancia)
dfr <- rbind(dfr, linha)

return(dfr)
}

write.xlsx(tabela(fileNames1), "Q3_Um.xlsx", colNames = TRUE)
write.xlsx(tabela(fileNames2), "Q3_Dois.xlsx", colNames = TRUE)
write.xlsx(tabela(fileNames3), "Q3_Três.xlsx", colNames = TRUE)
write.xlsx(tabela(fileNames4), "Q3_Quatro.xlsx", colNames = TRUE)
write.xlsx(tabela(fileNames5), "Q3_Cinco.xlsx", colNames = TRUE)
write.xlsx(tabela(fileNames6), "Q3_Seis.xlsx", colNames = TRUE)
write.xlsx(tabela(fileNames7), "Q3_Sete.xlsx", colNames = TRUE)
write.xlsx(tabela(fileNames8), "Q3_Oito.xlsx", colNames = TRUE)
write.xlsx(tabela(fileNames9), "Q3_Nove.xlsx", colNames = TRUE)
write.xlsx(tabela(fileNames10), "Q3_Dez.xlsx", colNames = TRUE)

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

#Áudio número 2
autocorrelacao(fileNames2[[1]])

#Áudio número 3
autocorrelacao(fileNames3[[1]])

#Áudio número 4
autocorrelacao(fileNames4[[1]])

#Áudio número 5
autocorrelacao(fileNames5[[1]])

#Áudio número 6
autocorrelacao(fileNames6[[1]])

#Áudio número 7
autocorrelacao(fileNames7[[1]])

#Áudio número 8
autocorrelacao(fileNames8[[1]])

#Áudio número 9
autocorrelacao(fileNames9[[1]])

#Áudio número 10
autocorrelacao(fileNames10[[1]])


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

#Áudio número 2
estacionario(fileNames2[[1]])

#Áudio número 3
estacionario(fileNames3[[1]])

#Áudio número 4
estacionario(fileNames4[[1]])

#Áudio número 5
estacionario(fileNames5[[1]])

#Áudio número 6
estacionario(fileNames6[[1]])

#Áudio número 7
estacionario(fileNames7[[1]])

#Áudio número 8
estacionario(fileNames8[[1]])

#Áudio número 9
estacionario(fileNames9[[1]])

#Áudio número 10
estacionario(fileNames10[[1]])



#---------------------------------------------------------------------------------------------------------------------

#Questão 4:

# Função que realiza o janelamento com sobreposição
# seg.length = tamanho da janela (em amostras)
# overlap = número de amostras para sobreposição 
# y = série temporal a ser segmentada
# A função retorna os índices de cada segmento, na forma de uma lista, cujo número total de elementos coincide com o número de segmentos

splitWithOverlap <- function(y, seg.length, overlap) {
  vec <- 1:length(y)
  starts = seq(1, length(vec), by=seg.length-overlap)
  ends   = starts + seg.length - 1
  ends[ends > length(vec)] = length(vec) #garantindo que o índice final não ultrapasse o tamanho do vetor
  
  # Dica: lapply é uma função bastante interessante no R, que simplifica a iteração (?lapply)
  ly <- lapply(1:length(starts), 
               function(i) 
                 if( (ends[i]-starts[i]+1) ==  seg.length){
                   y <- vec[starts[i]:ends[i]]
                 }
  )
  ly[!sapply(ly, is.null)] #removendo elementos NULL da lista (retorna elementos não nulos)
  
  
}

#Detectar alterações na amplitude do sinal - valor da média
#Detectar variabilidade dos sinais - valor do desvio padrão
padrao.tendencia <- function(fname){

  originalSound <- readWave(fname)
  #play(originalSound) # opens Windows Media Player to play the sound. Beware that the window does not automatically close when the audio file is finished playing. You need to close the window before you can continue in R.
  
  fs = originalSound@samp.rate # sampling frequency in Hz
  dt = 1/fs #resolution time in seconds
  
  yl <- originalSound@left
  yr <- originalSound@right
  
  tf <- (length(yl)-1)*dt # tempo final em segundos
  t <- seq(from=0, to=tf, by=dt)
  
  df <- data.frame(time=t, yl, yr)
  
  Nwnd <- 100 # Janela retangular de 100 ms
  N <- (Nwnd/1000) * fs # tamanho da janela retangular em número de amostras (obs: t = n * dt)
  
  wngSeg <- splitWithOverlap (yr, seg.length = N, overlap = ceiling(0.2*N)) 
  
  
  yrect1 <- rep(0, length(t)) #iniciando um vetor com zeros (geração do pulso retangular)
  yrect1[wngSeg[[1]]] <- 1
  
  yrect2 <- rep(0, length(t)) 
  yrect2[wngSeg[[2]]] <- 1
  
  yrect3 <- rep(0, length(t)) 
  yrect3[wngSeg[[3]]] <- 1
  
  yrect4 <- rep(0, length(t)) 
  yrect4[wngSeg[[4]]] <- 1
  
  df1 <- data.frame(time = t, yr, yrect1 + 10000, yrect2 - 10000, yrect3 + 10000, yrect4 - 10000)
 
  yrect <- yrect1 + yrect2 + yrect3 + yrect4
  
  y <- yr * yrect
  
  dygraph(data.frame(time = df1$time, y))
  
#Cálculo da média e valor RMS do padrão de tendência:
    
    tc <- rep(NA, length(wngSeg)) # vetor que armazena o tempo médio de cada segmento
    v.media <- rep(NA, length(wngSeg)) # vetor que armazena a média
    v.sd <- rep(NA, length(wngSeg)) # vetor que armazena o desvio padrão
    
    for (i in 1:length(wngSeg)){
      
      tc[i] <- mean(wngSeg[[i]]) * dt
      v.media[i] <- mean( y[wngSeg[[i]]] )
      v.sd[i] <- sd( y[wngSeg[[i]]] )
      
    }
    
    dfr <- data.frame(time=tc, v.media, v.sd)
    
    # dfr2 <- data.frame(time= seq(from=1, to = 1, by=1))
    # dfr2$media <- NA
    # dfr2$desvioPadrao <- NA
    # dfr2$media[1] <- mean(dfr$v.media[1:length(dfr$time)])
    # dfr2$desvioPadrao[1] <- mean(dfr$v.sd[1:length(dfr$time)])
    
    dfr2 <- rep(NA, 2)
    dfr2[1] <- mean(dfr$v.media[1:length(dfr$time)])
    dfr2[2] <- mean(dfr$v.sd[1:length(dfr$time)])
    

  return(dfr2)
}

Juntar_tendencias <- function(v1, v2, v3, v4, v5){
  
  #Aplicando função padrao.tendencia para depois juntar os vetores em apenas uma matriz para depois criar
  #uma planilha em excel
  d1 <- padrao.tendencia(v1)
  d2 <- padrao.tendencia(v2)
  d3 <- padrao.tendencia(v3)
  d4 <- padrao.tendencia(v4)
  d5 <- padrao.tendencia(v5)

  A = matrix(c(NA), nrow=5, ncol=3, byrow = TRUE)
  
  colnames(A) = c("Repetições", "Média", "Desvio Padrão")
  row.names(A) = c("1", "2", "3", "4", "5")
  
  A[1:5,1] <- c("1", "2", "3", "4", "5")
  A[1,2] <- d1[1]
  A[1,3] <- d1[2]
  
  A[2,2] <- d2[1]
  A[2,3] <- d2[2]
  
  A[3,2] <- d3[1]
  A[3,3] <- d3[2]
  
  A[4,2] <- d4[1]
  A[4,3] <- d4[2]
  
  A[5,2] <- d5[1]
  A[5,3] <- d5[2]
  
  return(A)
  
}



write.xlsx(Juntar_tendencias(fileNames1[[1]], fileNames1[[2]],
                             fileNames1[[3]], fileNames1[[4]],
                             fileNames1[[5]]), "tendencia_Um.xlsx", colNames = TRUE)

write.xlsx(Juntar_tendencias(fileNames2[[1]], fileNames2[[2]],
                             fileNames2[[3]], fileNames2[[4]],
                             fileNames2[[5]]), "tendencia_Dois.xlsx", colNames = TRUE)

write.xlsx(Juntar_tendencias(fileNames3[[1]], fileNames3[[2]],
                             fileNames3[[3]], fileNames3[[4]],
                             fileNames3[[5]]), "tendencia_Três.xlsx", colNames = TRUE)

write.xlsx(Juntar_tendencias(fileNames4[[1]], fileNames4[[2]],
                             fileNames4[[3]], fileNames4[[4]],
                             fileNames4[[5]]), "tendencia_Quatro.xlsx", colNames = TRUE)

write.xlsx(Juntar_tendencias(fileNames5[[1]], fileNames5[[2]],
                             fileNames5[[3]], fileNames5[[4]],
                             fileNames5[[5]]), "tendencia_Cinco.xlsx", colNames = TRUE)

write.xlsx(Juntar_tendencias(fileNames6[[1]], fileNames6[[2]],
                             fileNames6[[3]], fileNames6[[4]],
                             fileNames6[[5]]), "tendencia_Seis.xlsx", colNames = TRUE)

write.xlsx(Juntar_tendencias(fileNames7[[1]], fileNames7[[2]],
                             fileNames7[[3]], fileNames7[[4]],
                             fileNames7[[5]]), "tendencia_Sete.xlsx", colNames = TRUE)

write.xlsx(Juntar_tendencias(fileNames8[[1]], fileNames8[[2]],
                             fileNames8[[3]], fileNames8[[4]],
                             fileNames8[[5]]), "tendencia_Oito.xlsx", colNames = TRUE)

write.xlsx(Juntar_tendencias(fileNames9[[1]], fileNames9[[2]],
                             fileNames9[[3]], fileNames9[[4]],
                             fileNames9[[5]]), "tendencia_Nove.xlsx", colNames = TRUE)

write.xlsx(Juntar_tendencias(fileNames10[[1]], fileNames10[[2]],
                             fileNames10[[3]], fileNames10[[4]],
                             fileNames10[[5]]), "tendencia_Dez.xlsx", colNames = TRUE)




#---------------------------------------------------------------------------------------------------------------------

#Questão 5:
#Letra a e b:---------------------------------------------------------------- 
max_picos <- function(fname){

  originalSound <- readWave(fname)
  #play(originalSound) # opens Windows Media Player to play the sound. Beware that the window does not automatically close when the audio file is finished playing. You need to close the window before you can continue in R.
  
  fs = originalSound@samp.rate # sampling frequency in Hz
  dt = 1/fs #resolution time in seconds
  
  yl <- originalSound@left
  yr <- originalSound@right
  
  tf <- (length(yl)-1)*dt # tempo final em segundos
  t <- seq(from=0, to=tf, by=dt)
  
  df <- data.frame(time=t, yl, yr)
  
  dygraph(df)
  
  th <- max(df$yr, na.rm = TRUE)*0.5 # limiar
  
  pp <- findpeaks(df$yr, minpeakheight=th) #encontrando picos (esta função retorna uma matriz)
  
  indxpeaks <- pp[ , 2] #retorna a segunda coluna da matriz (que contém os índices em que os picos aconteceram)
  
  df$peaks <- NA #inicializando um vetor com dados faltantes (NA = not a number)
  
  df$peaks[indxpeaks] <- df$yr[indxpeaks]
  
  max_peaks <- na.exclude(data.frame(time=df$time, df$peaks)) 
  
  if(length(max_peaks$time) > 20){
    
    ordenado <- max_peaks[order(max_peaks$df.peaks, decreasing= TRUE), ]
    
    
    dVinte <- data.frame(time= seq(from=1, to = 20, by=1))
    dVinte$tempo <- NA
    dVinte$peaks <- NA
    
    for(i in 1:20) {
      
      dVinte$tempo[i] <- ordenado$time[i]
      dVinte$peaks[i] <- ordenado$df.peaks[i]
    }

    return(dVinte$peaks)
  }
  
  else{
    return(max_peaks$df.peaks)
  }
  
}

write.xlsx(max_picos(fileNames1[[1]]), "PicosMax_Um.xlsx", colNames = TRUE)
write.xlsx(max_picos(fileNames2[[1]]), "PicosMax_Dois.xlsx", colNames = TRUE)
write.xlsx(max_picos(fileNames3[[1]]), "PicosMax_Três.xlsx", colNames = TRUE)
write.xlsx(max_picos(fileNames4[[1]]), "PicosMax_Quatro.xlsx", colNames = TRUE)
write.xlsx(max_picos(fileNames5[[1]]), "PicosMax_Cinco.xlsx", colNames = TRUE)
write.xlsx(max_picos(fileNames6[[1]]), "PicosMax_Seis.xlsx", colNames = TRUE)
write.xlsx(max_picos(fileNames7[[1]]), "PicosMax_Sete.xlsx", colNames = TRUE)
write.xlsx(max_picos(fileNames8[[1]]), "PicosMax_Oito.xlsx", colNames = TRUE)
write.xlsx(max_picos(fileNames9[[1]]), "PicosMax_Nove.xlsx", colNames = TRUE)
write.xlsx(max_picos(fileNames10[[1]]), "PicosMax_Dez.xlsx", colNames = TRUE)

#Letra c:(questão 5)-----------------------------------------------------
#Encontrando a resposta h[n] para cada comando de áudio:
#y[n] = 1.5 y[n-1] - 0.85 y[n-2] + x[n]

H <- function(a){
  x <- a
  
  h <- rep(0, length(x)) 
  
  h[1] <- 1
  
  dy <- data.frame(time=1:length(x), x)
  
  dh <- data.frame(time=1:20, h)
  
  H <- data.frame(time=1:20)
  H$picos  <- NA
  
  
  for(i in 1:20){
    
    b <- i - 1
    c <- i - 2
    
    if(b <= 0 && c <= 0){
      H$picos[i] <- h[i]
    }
    
    if(c <= 0 && b > 0){
      H$picos[i] <- 1.5 * dy$x[b] - 0.85 * 0 + h[i]
    }
    if(b > 0 && c > 0){
      H$picos[i] <- 1.5 * dy$x[b] - 0.85 * dy$x[c] + h[i]
    }
    
  }
  return(H$picos)
}

#Resposta h[n] para cada comando de áudio no qual n = 20 maiores picos:
#Onde y[n]=h[n] , se x[n]=Impulso[n]:

#COMANDO 1:
h1.1 <- H(max_picos(fileNames1[[1]]))
h1.2 <- H(max_picos(fileNames1[[2]]))
h1.3 <- H(max_picos(fileNames1[[3]]))
h1.4 <- H(max_picos(fileNames1[[4]]))
h1.5 <- H(max_picos(fileNames1[[5]]))


#COMANDO 2:
h2.1 <- H(max_picos(fileNames2[[1]]))
h2.2 <- H(max_picos(fileNames2[[2]]))
h2.3 <- H(max_picos(fileNames2[[3]]))
h2.4 <- H(max_picos(fileNames2[[4]]))
h2.5 <- H(max_picos(fileNames2[[5]]))


#COMANDO 3:
h3.1 <- H(max_picos(fileNames3[[1]]))
h3.2 <- H(max_picos(fileNames3[[2]]))
h3.3 <- H(max_picos(fileNames3[[3]]))
h3.4 <- H(max_picos(fileNames3[[4]]))
h3.5 <- H(max_picos(fileNames3[[5]]))


#COMANDO 4:
h4.1 <- H(max_picos(fileNames4[[1]]))
h4.2 <- H(max_picos(fileNames4[[2]]))
h4.3 <- H(max_picos(fileNames4[[3]]))
h4.4 <- H(max_picos(fileNames4[[4]]))
h4.5 <- H(max_picos(fileNames4[[5]]))


#COMANDO 5:
h5.1 <- H(max_picos(fileNames5[[1]]))
h5.2 <- H(max_picos(fileNames5[[2]]))
h5.3 <- H(max_picos(fileNames5[[3]]))
h5.4 <- H(max_picos(fileNames5[[4]]))
h5.5 <- H(max_picos(fileNames5[[5]]))


#COMANDO 6:
h6.1 <- H(max_picos(fileNames6[[1]]))
h6.2 <- H(max_picos(fileNames6[[2]]))
h6.3 <- H(max_picos(fileNames6[[3]]))
h6.4 <- H(max_picos(fileNames6[[4]]))
h6.5 <- H(max_picos(fileNames6[[5]]))


#COMANDO 7:
h7.1 <- H(max_picos(fileNames7[[1]]))
h7.2 <- H(max_picos(fileNames7[[2]]))
h7.3 <- H(max_picos(fileNames7[[3]]))
h7.4 <- H(max_picos(fileNames7[[4]]))
h7.5 <- H(max_picos(fileNames7[[5]]))


#COMANDO 8:
h8.1 <- H(max_picos(fileNames8[[1]]))
h8.2 <- H(max_picos(fileNames8[[2]]))
h8.3 <- H(max_picos(fileNames8[[3]]))
h8.4 <- H(max_picos(fileNames8[[4]]))
h8.5 <- H(max_picos(fileNames8[[5]]))


#COMANDO 9:
h9.1 <- H(max_picos(fileNames9[[1]]))
h9.2 <- H(max_picos(fileNames9[[2]]))
h9.3 <- H(max_picos(fileNames9[[3]]))
h9.4 <- H(max_picos(fileNames9[[4]]))
h9.5 <- H(max_picos(fileNames9[[5]]))


#COMANDO 10:
h10.1 <- H(max_picos(fileNames10[[1]]))
h10.2 <- H(max_picos(fileNames10[[2]]))
h10.3 <- H(max_picos(fileNames10[[3]]))
h10.4 <- H(max_picos(fileNames10[[4]]))
h10.5 <- H(max_picos(fileNames10[[5]]))


LISTA <- list(h1.1, h1.2, h1.3, h1.4, h1.5,
              h2.1, h2.2, h2.3, h2.4, h2.5,
              h3.1, h3.2, h3.3, h3.4, h3.5,
              h4.1, h4.2, h4.3, h4.4, h4.5,
              h5.1, h5.2, h5.3, h5.4, h5.5,
              h6.1, h6.2, h6.3, h6.4, h6.5,
              h7.1, h7.2, h7.3, h7.4, h7.5,
              h8.1, h8.2, h8.3, h8.4, h8.5,
              h9.1, h9.2, h9.3, h9.4, h9.5,
              h10.1, h10.2, h10.3, h10.4, h10.5)



#Letra d:(questão 5)-------------------------------------------------------------

dist_Euclidiana <- function(a, b){

    #Tamanho vetor pode ser do a ou b
  pot <- rep(NA, length(a)) 
  subt <- rep(NA, length(a)) 
  
  
  for(i in 1:20){
    subt[i] <- a[i] - b[i]
    pot[i] <- subt[i]*subt[i]
  }
  soma <- sum(pot)
  raiz <- sqrt(soma)
  return(soma)
}

A = matrix(c(NA), nrow=50, ncol=50, byrow = TRUE)

h <- c("h1.1", "h1.2", "h1.3", "h1.4", "h1.5",
       "h2.1", "h2.2", "h2.3", "h2.4", "h2.5",
       "h3.1", "h3.2", "h3.3", "h3.4", "h3.5",
       "h4.1", "h4.2", "h4.3", "h4.4", "h4.5",
       "h5.1", "h5.2", "h5.3", "h5.4", "h5.5",
       "h6.1", "h6.2", "h6.3", "h6.4", "h6.5",
       "h7.1", "h7.2", "h7.3", "h7.4", "h7.5",
       "h8.1", "h8.2", "h8.3", "h8.4", "h8.5",
       "h9.1", "h9.2", "h9.3", "h9.4", "h9.5",
       "h10.1", "h10.2", "h10.3", "h10.4", "h10.5")

colnames(A) = h
row.names(A) = h

for(i in 1:nrow(A)) {
  for(j in 1:nrow(A)){
    A[i,j] <- dist_Euclidiana(LISTA[[i]], LISTA[[j]])
  }
}

write.xlsx(A, "DistânciaEuclidiana.xlsx", colNames = TRUE, rowNames = TRUE)


#---------------------------------------------------------------------------------------------------------------------

#Questão 7:

library(tuneR)# biblioteca que contém conjunto de funções para gerar sinais básicos
library(dygraphs)#biblioteca para plotagem de séries temporais (gráfico interativo)
library(signal)
library(ggplot2)
library(REdaS)
library(fftwtools)
library(seewave)

# Definição do diretório de trabalho

getwd() # get current working directory
setwd("C:/Users/samsung/Documents/Universidade/PSB/Prova2/")

# Lendo o arquivo texto na forma de uma tabela
df1 <- read.table("a1.txt",header = TRUE, sep =  " ", skip = 6)

# alteração do nome das variáveis do dataframe
names(df1) <- c("AccX", "AccY", "MuscAnterior","MuscPosterior")

#criando o vetor de tempo

fs <- 500 #Hz

# Definição intervalo entre as amostras em segundos
dt <- 1/fs

t <- seq(from=0, to = dt*(length(df1$AccX)-1), by=dt) 
df1 <- cbind(t,df1)

dygraph(df1[c("t","MuscAnterior")]) %>% dyRangeSelector()

dygraph(df1[c("t","MuscPosterior")]) %>% dyRangeSelector()

dfpost <- data.frame(time=df1$t, MuscPosterior=df1$MuscPosterior)

dfant <- data.frame(time=df1$t, MuscAnterior=df1$MuscAnterior)


#Letra A:-----------------------------------
#Filtro Butterworth---------------------------------------------------------------------------------------------

Fs <- 500 # frequência de amostragem em Hz

#Primeiro filtro: passa alta para atenuar frequências abaixo de 0.2 Hz---------------------------
nh <- 3 # ordem do filtro
Fc_high <- 0.2 # frequência de corte em Hz
Wh <- Fc_high/(Fs/2)
bt_high <- butter(nh, Wh, type = "high") # cálculo dos coeficientes do fitro
freqz(filt = bt_high, Fs = Fs)

# fazendo a filtragem por meio do uso da função filtfilt
y_filt_high <- filtfilt(filt = bt_high, dfpost$MuscPosterior)
y_filt_high2 <- filtfilt(filt = bt_high, dfant$MuscAnterior)

dfplot1 <- data.frame(time = t, dfpost$MuscPosterior, yf = y_filt_high)
dygraph(dfplot1)

dfplot2 <- data.frame(time = t, dfant$MuscAnterior, yf = y_filt_high2)
dygraph(dfplot2)


#Segundo filtro: rejeita faixa para atenuar 60 Hz da rede elétrica---------------------------------
nr <- 4
Fc_l <- 59 # frequência de corte inferior em Hz
Fc_u <- 61 # frequência de corte superior em Hz
Wrej <- c(Fc_l, Fc_u)/(Fs/2)
bt_rej <- butter(nr, Wrej, type = "stop") # cálculo dos coeficientes do fitro
freqz(filt = bt_rej, Fs = Fs)

# fazendo a filtragem por meio do uso da função filtfilt
y_filt_rej <- filtfilt(filt = bt_rej, dfplot1$yf)
y_filt_rej2 <- filtfilt(filt = bt_rej, dfplot2$yf)

dfplot3 <- data.frame(time = t, dfplot1$yf, yf = y_filt_rej)
dygraph(dfplot3)

dfplot4 <- data.frame(time = t, dfplot2$yf, yf = y_filt_rej2)
dygraph(dfplot4)

#Retificando o sinal antes de passar pelo filtro passa baixa
dfplot3 <- abs(dfplot3)
dygraph(dfplot3)
dfplot4 <- abs(dfplot4)
dygraph(dfplot4)


#Terceiro filtro: passa baixa para atenuar frequências abaixo de 2 Hz---------------------------
nl <- 4 # ordem do filtro
Fc_low <- 2 # frequência de corte em Hz
Wl <- Fc_low/(Fs/2)
bt_low <- butter(nl, Wl, type = "low") # cálculo dos coeficientes do fitro
freqz(filt = bt_low, Fs = Fs)

# fazendo a filtragem por meio do uso da função filtfilt
y_filt_low <- filtfilt(filt = bt_low, dfplot3$yf)
y_filt_low2 <- filtfilt(filt = bt_low, dfplot4$yf)

dfplot5 <- data.frame(time = t, dfplot3$yf, yf = y_filt_low)
dygraph(dfplot5)

dfplot6 <- data.frame(time = t, dfplot4$yf, yf = y_filt_low2)
dygraph(dfplot6)

#Plotando a detecção de envoltório
dygraph(data.frame(time=dfplot5$time, envoltorio=dfplot5$yf, df1$MuscPosterior))
dygraph(data.frame(time=dfplot6$time, envoltorio=dfplot6$yf, dfant$MuscAnterior))
dygraph(data.frame(time=dfplot6$time, envoltorio=dfplot6$yf, dfplot2$yf))

#Filtro Chebyshev---------------------------------------------------------------------------------------------

Fs <- 500 # frequência de amostragem em Hz

#Primeiro filtro: passa alta para atenuar frequências abaixo de 0.2 Hz---------------------------
nc <- 3 # ordem do filtro

Fc <- 0.2 # frequência de corte em Hz
W <- Fc/(Fs/2)
Rp <- 0.5

cheb <- cheby1(nc, Rp, W, type = "high") # cálculo dos coeficientes do fitro
freqz(filt = cheb, Fs = Fs)

# fazendo a filtragem por meio do uso da função filtfilt
y_filt_high_cheb <- filtfilt(filt = cheb, dfpost$MuscPosterior)
y_filt_high_cheb2 <- filtfilt(filt = cheb, dfant$MuscAnterior)

dfplot5 <- data.frame(time = t, dfpost$MuscPosterior, yf = y_filt_high_cheb)
dygraph(dfplot5)

dfplot6 <- data.frame(time = t, dfant$MuscAnterior, yf = y_filt_high_cheb2)
dygraph(dfplot6)



#Segundo filtro: rejeita faixa para atenuar 60 Hz da rede elétrica---------------------------------

Ws <- c(Fc_l, Fc_u)/(Fs/2)

cheb_stop <-  cheby1(nc, Rp, Ws, type = "stop") # cálculo dos coeficientes do fitro
freqz(filt = cheb_stop, Fs = Fs)

# fazendo a filtragem por meio do uso da função filtfilt
y_filt_cheb_stop <- filtfilt(filt = cheb_stop, dfplot5$yf)
y_filt_cheb_stop2 <- filtfilt(filt = cheb_stop, dfplot6$yf)

dfplot7 <- data.frame(time = t, dfplot5$yf, yf = y_filt_cheb_stop)
dygraph(dfplot7)

dfplot8 <- data.frame(time = t, dfplot6$yf, yf = y_filt_cheb_stop2)
dygraph(dfplot8)

#Retificando o sinal antes de passar pelo filtro passa baixa
dfplot7 <- abs(dfplot7)
dygraph(dfplot7)
dfplot8 <- abs(dfplot8)
dygraph(dfplot8)


#Terceiro filtro: passa baixa para atenuar frequências abaixo de 2 Hz---------------------------
Fc_low <- 2 # frequência de corte em Hz
Wcl <- Fc_low/(Fs/2)
Rp <- 0.5

cheb_low <- cheby1(nc, Rp, Wcl, type = "low") # cálculo dos coeficientes do fitro
freqz(filt = cheb_low, Fs = Fs)

# fazendo a filtragem por meio do uso da função filtfilt
y_filt_low_cheb <- filtfilt(filt = cheb_low, dfplot7$yf)
y_filt_low_cheb2 <- filtfilt(filt = cheb_low, dfplot8$yf)

dfplot9 <- data.frame(time = t, dfplot7$yf, yf = y_filt_low_cheb)
dygraph(dfplot9)

dfplot10 <- data.frame(time = t, dfplot8$yf, yf = y_filt_low_cheb2)
dygraph(dfplot10)

#Plotando a detecção de envoltório
dygraph(data.frame(time=dfplot9$time, envoltorio=dfplot9$yf, df1$MuscPosterior))
dygraph(data.frame(time=dfplot10$time, envoltorio=dfplot10$yf, dfant$MuscAnterior))


#Letra b:(questão 7)-------------------------------------------------

#Função para detectar bursts
d_burst <- function(df, t, limiar, duracao_burst, duracao_ruido){
  
  #df - amplitude do dataframe
  #t - vetor tempo do dataframe
  #duracao_burst - tempo de um burst -> Depende do dataframe analisado pela função d_burst
  #duracao_ruido - intervalo entre bursts -> Depende do dataframe analisado pela função d_burst
  #limiar - limiar que indica a presença de bursts
  
  
  dburst <- data.frame(time=t)
  dburst$amp <- NA
  
  i = 1
  
  while (i < length(t)) {
    
      if(df[i] >= limiar){
        
        dburst$amp[i:(i+duracao_burst)] <- df[i:(i+duracao_burst)]
        i <- (i+duracao_burst) + duracao_ruido
        
      }
      if(df[i] < limiar){
        
        i = i + 1
      }
  
  }
  
  return(dburst) 
  
}

# #Detecção bursts para sinal do músculo anterior
dburst_Ant <- d_burst(df1$MuscAnterior, df1$t,  2880, 280, 125)
dygraph(dburst_Ant)  %>% dyRangeSelector()

#Detecção bursts para sinal do músculo posterior
dburst_Post <- d_burst(dfplot1$yf, df1$t, 410, 400, 125)
dygraph(dburst_Post)  %>% dyRangeSelector()


#Letra c:(questão 7)-------------------------------------------------

espectro_amplitude <- function(a , b, lim, duracao_burst, duracao_ruido){
  
  #a - amplitude do dataframe
  #b - vetor tempo do dataframe
  #duracao_burst - tempo de um burst
  #duracao_ruido - intervalo entre bursts
  #lim - limiar que indica a presença de bursts

  set.seed(1234)
  
  y <- (2000 + 0.5*b) + rnorm(length(t))
  df <- data.frame(time = b, y)
  # dygraph(df)
  
  # regressão linear
  linearMod <- lm(y ~ t, data=df) 
  # print(linearMod)
  distPred <- predict(linearMod, df)  # predição
  
  # plot(b,a)
  lines(df$time, distPred,type='l',col='blue')
  
  # remoção da tendência linear
  plot(b,a-distPred)
  dtend_linear <- data.frame(time=b, y=a-distPred)
  dl<- d_burst(dtend_linear$y, dtend_linear$time, lim, duracao_burst, duracao_ruido)
  dl <- na.omit(dl)
  
  #Criando lista para armazenar amplitude dos burst
  l1 <- vector("list", 60)
  
  j = 1
  while(j < 24400){
    for(i in 1:61){
        l1[[i]] <- dl$amp[j:(j+400)]
        j <- j + 400
      }
  }
  
  #Criando lista para armazenar o tempo dos burst
  l2 <- vector("list", 61)

  j = 1
  while(j < 24400){
    for(i in 1:61){
      l2[[i]] <- dl$time[j:(j+400)]
      j <- j + 400
    }
  }
  
  #Criando lista para armazenar os espectros de amplitude de cada burst
  ff <- vector("list", 61)
  
  for(i in 1:61){
    ff[[i]] <- fftw(l1[[i]], inverse=0, HermConj=1, n=NULL)
  }
  
  # dygraph(data.frame(time=l2[[1]], espectro=abs(ff[[1]])))
  espectro <- vector("list", 2)
  espectro[[1]] <- l2
  espectro[[2]] <- ff

  return(espectro) #Retorna duas listas
}

Musc_Post <- espectro_amplitude(df1$MuscPosterior, df1$t, 410, 400, 125)
Musc_Ant <- espectro_amplitude(df1$MuscAnterior, df1$t, 2860, 750, 35)

#Letra d: (questão 7)---------------------------------------------------

frequencia_mediana <- function(A){
  
  # Intensidade cumulativa - Somatório da amplitude atual mais a anterior
  # Intensidade do sinal - Somatório de todas amplitudes dividido por dois
  # Frequencia é selecionada na qual a intensidade cumulativa 
  # (ou seja, todos os valores de intensidade para frequências mais baixas e incluindo a intensidade focal) 
  # primeiro excede o valor calculado no passo 1.

  A1 <- A[[1]] #Recebendo lista com vetores tempo
  A2 <- A[[2]] #Recebendo lista com vetores espectro de amplitude
  
  #Criando lista para armazenar as amplitudes cumulativas e intensidade total do sinal 
  intensidade_acumulativa <- vector("list", 61)
  intensidade_sinal <- vector("list", 61)
  
  for(i in 1:length(A2)){
         intensidade_sinal[[i]] <- sum(A2[[i]])/2
  }
  
  for(i in 1:length(A2)){
    for(j in 1:length(A2[[i]])){
      if(j-1 == 0){
        intensidade_acumulativa[[i]][[j]] <- A2[[i]][[j]]
      }
      if(j-1 > 0){
        intensidade_acumulativa[[i]][[j]] <- intensidade_acumulativa[[i]][[j-1]] + A2[[i]][[j]]
      }
    }
  }
  
  posicao <- vector("list", 61)
  
 for(i in 1:length(intensidade_acumulativa)){
    for(j in 1:length(intensidade_acumulativa[[i]])){
      if(abs(intensidade_acumulativa[[i]][[j]]) > abs(intensidade_sinal[[i]][[1]])){
        posicao[[i]][[j]] <- intensidade_acumulativa[[i]][[1]]
        break
      }
    }
 }
  
  #Iniciando um vetor vazio para armazenar a frequencia mediana
  valor <- rep(NA, 61)
  
  for(i in 1:length(posicao)){
    for(j in 1:length(posicao[[i]])){
      if(is.na(posicao[[i]]) == TRUE){
        valor[[i]] <- na.omit(posicao[[i]])
      }
      if(is.na(posicao[[i]]) == FALSE){
        valor[[i]] <- posicao[[i]]
      }
    }
  }
  
  return(valor)
}

pos_post <- frequencia_mediana(Musc_Post)
pos_ant <- frequencia_mediana(Musc_Ant)

#Letra e: (questão 7)---------------------------------------------------
#Plotando gráficos da frequência mediana ao longo do tempo

P_tempo <- Musc_Post[[1]]
P_amp <- Musc_Post[[2]]

tempo <- rep(NA, 61)

for(i in 1:length(pos_post)){
  for(j in 1:length(P_amp[[i]])){
    if(abs(pos_post[i]) == abs(P_amp[[i]][[j]])){
      tempo[i] <- P_tempo[[i]][[j]]
    }

  }
}

Frequencia <- abs(pos_post)

#Músculo posterior
dgraf_post <- data.frame(time=tempo, Frequencia)
linearMod <- lm(Frequencia ~ tempo, data=dgraf_post) #Aplicando regressão linear a resposta das frequências medianas
print(linearMod)
distPred <- predict(linearMod, dgraf_post)

dreg <- data.frame(time=tempo, distPred)

ggplot() + 
  geom_line(data=dreg, aes(x=tempo, y=distPred), color='blue') + 
  geom_point(data=dgraf_post, aes(x=tempo, y=Frequencia), color='black')


