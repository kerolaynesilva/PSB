
#install.packages("tuneR")
library(tuneR)
library(dygraphs)
library(htmltools)
library(moments)
library(openxlsx)

strPath <- "C:/Users/samsung/Documents/Universidade/PSB/M6/" #diretório de trabalho

fileNames <- list("s1.wav", "s2.wav", "s3.wav", "s4.wav", "s5.wav", "n1.wav", "n2.wav", "n3.wav", "n4.wav", "n5.wav") #lista com os nomes dos arquivos gravados


fileNames <- as.list(paste(strPath,fileNames, sep = "")) # unindo diretório aos nomes dos arquivos


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

#aplica uma função a uma lista
dy_graph <- list(lapply(fileNames, plotWAV))  # end list

htmltools::browsable(htmltools::tagList(dy_graph))

#Cálculo da média, variância, coeficiente de assimetria e curtose de cada sinal

dfr <- data.frame(time= seq(from=1, to = 10, by=1))
dfr$media <- NA
dfr$variancia <- NA
dfr$coeficiente_de_assimetria <- NA
dfr$curtose <- NA


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

for(i in 1:10) {
  # i-th element of `u1` squared into `i`-th position of `usq`
  
  dfr$media[i] <- mean(d_frame(fileNames[[i]])$yr)
  dfr$variancia[i] <- var(d_frame(fileNames[[i]])$yr)
  dfr$coeficiente_de_assimetria[i] <- skewness(d_frame(fileNames[[i]])$yr)
  dfr$curtose[i] <- kurtosis(d_frame(fileNames[[i]])$yr)
 
}

dfr_variancia <- data.frame(time= seq(from=11, to = 12, by=1))
dfr_variancia$media[1] <- var(dfr$media[1:5])
dfr_variancia$variancia[1] <- var(dfr$variancia[1:5])
dfr_variancia$coeficiente_de_assimetria[1] <- var(dfr$coeficiente_de_assimetria[1:5])
dfr_variancia$curtose[1] <- var(dfr$curtose[1:5])

dfr_variancia$media[2] <- var(dfr$media[6:10])
dfr_variancia$variancia[2] <- var(dfr$variancia[6:10])
dfr_variancia$coeficiente_de_assimetria[2] <- var(dfr$coeficiente_de_assimetria[6:10])
dfr_variancia$curtose[2] <- var(dfr$curtose[6:10])

linha <- data.frame(dfr_variancia)
dfr <- rbind(dfr, linha)


write.xlsx(dfr, "tabela1.xlsx", colNames = TRUE)


#Questão 3

library(tuneR)
library(dygraphs)
library(htmltools)
library(moments)


strPath <- "C:/Users/samsung/Documents/Universidade/PSB/M6/" #diretório de trabalho

Fname <- list("s1.wav", "s2.wav", "s3.wav", "s4.wav", "s5.wav", "n1.wav", "n2.wav", "n3.wav", "n4.wav", "n5.wav") #nome do arquivo

strFile1 <- paste(strPath,Fname[[1]], sep = "")
strFile2 <- paste(strPath,Fname[[2]], sep = "")
strFile3 <- paste(strPath,Fname[[3]], sep = "")
strFile4 <- paste(strPath,Fname[[4]], sep = "")
strFile5 <- paste(strPath,Fname[[5]], sep = "")
strFile6 <- paste(strPath,Fname[[6]], sep = "")
strFile7 <- paste(strPath,Fname[[7]], sep = "")
strFile8 <- paste(strPath,Fname[[8]], sep = "")
strFile9 <- paste(strPath,Fname[[9]], sep = "")
strFile10 <- paste(strPath,Fname[[10]], sep = "")


ysound1 <- readWave(strFile1)
ysound2 <- readWave(strFile2)
ysound3 <- readWave(strFile3)
ysound4 <- readWave(strFile4)
ysound5 <- readWave(strFile5)
ysound6 <- readWave(strFile6)
ysound7 <- readWave(strFile7)
ysound8 <- readWave(strFile8)
ysound9 <- readWave(strFile9)
ysound10 <- readWave(strFile10)


fs = ysound1@samp.rate # sampling frequency in Hz
dt = 1/fs #resolution time in seconds
y1 <- ysound1@left


tf <- (length(y1)-1)*dt # tempo final em segundos
t <- seq(from=0, to=tf, by=dt)
df1 <- data.frame(time=t, y1)

# a função basename extrai o nome do arquivo apenas
# a função dirname extrai o caminho completo
dygraph(df1)

fs2 = ysound2@samp.rate # sampling frequency in Hz
dt2 = 1/fs2 #resolution time in seconds
y2 <- ysound2@left


tf2 <- (length(y2)-1)*dt2 # tempo final em segundos
t2 <- seq(from=0, to=tf2, by=dt2)
df2 <- data.frame(time=t2, y2)

# a função basename extrai o nome do arquivo apenas
# a função dirname extrai o caminho completo
dygraph(df2)


fs3 = ysound3@samp.rate # sampling frequency in Hz
dt3 = 1/fs3 #resolution time in seconds
y3 <- ysound3@left


tf3 <- (length(y3)-1)*dt3 # tempo final em segundos
t3 <- seq(from=0, to=tf3, by=dt3)
df3 <- data.frame(time=t3, y3)


fs4 = ysound4@samp.rate # sampling frequency in Hz
dt4 = 1/fs4 #resolution time in seconds
y4 <- ysound4@left


tf4 <- (length(y4)-1)*dt4 # tempo final em segundos
t4 <- seq(from=0, to=tf4, by=dt4)
df4 <- data.frame(time=t4, y4)


fs5 = ysound5@samp.rate # sampling frequency in Hz
dt5 = 1/fs5 #resolution time in seconds
y5 <- ysound5@left


tf5 <- (length(y5)-1)*dt5 # tempo final em segundos
t5 <- seq(from=0, to=tf5, by=dt5)
df5 <- data.frame(time=t5, y5)


fs6 = ysound6@samp.rate # sampling frequency in Hz
dt6 = 1/fs6 #resolution time in seconds
y6 <- ysound6@left


tf6 <- (length(y6)-1)*dt6 # tempo final em segundos
t6 <- seq(from=0, to=tf6, by=dt6)
df6 <- data.frame(time=t6, y6)


fs7 = ysound7@samp.rate # sampling frequency in Hz
dt7 = 1/fs7 #resolution time in seconds
y7 <- ysound7@left


tf7 <- (length(y7)-1)*dt7 # tempo final em segundos
t7 <- seq(from=0, to=tf7, by=dt7)
df7 <- data.frame(time=t7, y7)


fs8 = ysound8@samp.rate # sampling frequency in Hz
dt8 = 1/fs8 #resolution time in seconds
y8 <- ysound8@left


tf8 <- (length(y8)-1)*dt8 # tempo final em segundos
t8 <- seq(from=0, to=tf8, by=dt8)
df8 <- data.frame(time=t8, y8)


fs9 = ysound9@samp.rate # sampling frequency in Hz
dt9 = 1/fs9 #resolution time in seconds
y9 <- ysound9@left


tf9 <- (length(y9)-1)*dt9 # tempo final em segundos
t9 <- seq(from=0, to=tf9, by=dt9)
df9 <- data.frame(time=t9, y9)


fs10 = ysound10@samp.rate # sampling frequency in Hz
dt10 = 1/fs10 #resolution time in seconds
y10 <- ysound10@left


tf10 <- (length(y10)-1)*dt10 # tempo final em segundos
t10 <- seq(from=0, to=tf10, by=dt10)
df10 <- data.frame(time=t10, y10)



#comparação entre os sinais 
ccf(y1, y1,lag.max = 10000)
ccf(y1, y2,lag.max = 10000)
ccf(y1, y3,lag.max = 10000)
ccf(y1, y4,lag.max = 10000)
ccf(y1, y5,lag.max = 10000)
ccf(y1, y6,lag.max = 10000)
ccf(y1, y7,lag.max = 10000)
ccf(y1, y8,lag.max = 10000)
ccf(y1, y9,lag.max = 10000)
ccf(y1, y10,lag.max = 10000)

ccf(y2, y1,lag.max = 10000)
ccf(y2, y2,lag.max = 10000)
ccf(y2, y3,lag.max = 10000)
ccf(y2, y4,lag.max = 10000)
ccf(y2, y5,lag.max = 10000)
ccf(y2, y6,lag.max = 10000)
ccf(y2, y7,lag.max = 10000)
ccf(y2, y8,lag.max = 10000)
ccf(y2, y9,lag.max = 10000)
ccf(y2, y10,lag.max = 10000)

ccf(y3, y1,lag.max = 10000)
ccf(y3, y2,lag.max = 10000)
ccf(y3, y3,lag.max = 10000)
ccf(y3, y4,lag.max = 10000)
ccf(y3, y5,lag.max = 10000)
ccf(y3, y6,lag.max = 10000)
ccf(y3, y7,lag.max = 10000)
ccf(y3, y8,lag.max = 10000)
ccf(y3, y9,lag.max = 10000)
ccf(y3, y10,lag.max = 10000)

ccf(y4, y1,lag.max = 10000)
ccf(y4, y2,lag.max = 10000)
ccf(y4, y3,lag.max = 10000)
ccf(y4, y4,lag.max = 10000)
ccf(y4, y5,lag.max = 10000)
ccf(y4, y6,lag.max = 10000)
ccf(y4, y7,lag.max = 10000)
ccf(y4, y8,lag.max = 10000)
ccf(y4, y9,lag.max = 10000)
ccf(y4, y10,lag.max = 10000)

ccf(y5, y1,lag.max = 10000)
ccf(y5, y2,lag.max = 10000)
ccf(y5, y3,lag.max = 10000)
ccf(y5, y4,lag.max = 10000)
ccf(y5, y5,lag.max = 10000)
ccf(y5, y6,lag.max = 10000)
ccf(y5, y7,lag.max = 10000)
ccf(y5, y8,lag.max = 10000)
ccf(y5, y9,lag.max = 10000)
ccf(y5, y10,lag.max = 10000)


ccf(y6, y6,lag.max = 10000)
ccf(y6, y7,lag.max = 10000)
ccf(y6, y8,lag.max = 10000)
ccf(y6, y9,lag.max = 10000)
ccf(y6, y10,lag.max = 10000)


ccf(y7, y6,lag.max = 10000)
ccf(y7, y7,lag.max = 10000)
ccf(y7, y8,lag.max = 10000)
ccf(y7, y9,lag.max = 10000)
ccf(y7, y10,lag.max = 10000)


ccf(y8, y6,lag.max = 10000)
ccf(y8, y7,lag.max = 10000)
ccf(y8, y8,lag.max = 10000)
ccf(y8, y9,lag.max = 10000)
ccf(y8, y10,lag.max = 10000)

ccf(y9, y6,lag.max = 10000)
ccf(y9, y7,lag.max = 10000)
ccf(y9, y8,lag.max = 10000)
ccf(y9, y9,lag.max = 10000)
ccf(y9, y10,lag.max = 10000)

ccf(y10, y6,lag.max = 10000)
ccf(y10, y7,lag.max = 10000)
ccf(y10, y8,lag.max = 10000)
ccf(y10, y9,lag.max = 10000)
ccf(y10, y10,lag.max = 10000)




#Questão 4
library(ggplot2)
library(dygraphs)

# Considere o modelo do potencial de ação da fibra muscular
u <- 0.001
eq = function(t){u*t*(2-u*t)*exp(-u*t)}
N <- 10000
plot(eq(1:N), type='l')

# Atribuindo o sinal do modelo a uma variável

y <- eq(1:N)


# subamostrando o potencial de ação para que o mesmo tenha 100 amostras
yobj <- spline(x=1:N, y, n=100)

df1 <- data.frame(time = 1:length(y), y)
df2 <- data.frame(time = yobj$x, y=yobj$y)


p <- ggplot()
p <- p +   geom_line(data = df1, aes(x=time, y=y),  color= 'red')
p <- p +  geom_point(data = df2, aes(x=time, y=y),  color= 'black')
p <- p + theme_bw()
p

# Simulando um trem de potencial de ação

t <- 1:10000 #Taxa de amostragem 10kHz
y <- runif(length(t), min=0, max=0.05)

#GRUPO E1
E1 <- c(1000, 2000, 5000, 6000, 7000) # tempo discreto em que os potenciais de ação acontecerão

y[E1] <- 1

g1 <- convolve(y, rev(df2$y), type = "open") # digite ?convolve para help

dygraph(data.frame(time= (1:length(g1))*0.001, g1))


#GRUPO E2
E2 <- c(2000, 3000, 4000, 7000, 8000, 9000)

y[E2] <- 1

g2 <- convolve(y, rev(df2$y), type = "open") # digite ?convolve para help

dygraph(data.frame(time= (1:length(g2))*0.001, g2))


#GRUPO E3
E3 <- c(1000, 2000, 3000, 9000, 10000)

y[E3] <- 1

g3 <- convolve(y, rev(df2$y), type = "open") # digite ?convolve para help

dygraph(data.frame(time= (1:length(g3))*0.001, g3))

df3 <- data.frame(time= 1:length(g3), g3)
df4 <- data.frame(time= 1:length(g2), g2)
df5 <- data.frame(time= 1:length(g1), g1)

qplot(df3$time, geom="histogram",  main = "Histograma do Grupo E3", xlab = "Tempo")

qplot(df4$time, geom="histogram",  main = "Histograma do Grupo E2", xlab = "Tempo")

qplot(df5$time, geom="histogram",  main = "Histograma do Grupo E1", xlab = "Tempo")
