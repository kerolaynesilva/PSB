
#Questão 2:

library(dygraphs)

Fs <- 1200 # frequência de amostragem em Hz
t <- seq(from=0, to = 5, by = 1/Fs) # vetor de tempo em segundos


f1 <- 60 # frequência de oscilação
f2 <- 100

y1 <- sin(2*pi* f1 * t) # sinal senoidal oscilando a f1 (em Hz)
y2 <- sin(2*pi* f2 * t)

ss <- y1 + y2


df <- data.frame(time = t, ss)
dygraph(df)%>%
  dyOptions(drawPoints = TRUE, pointSize = 3) %>%
  dyAxis(name='x', label= "Tempo(s)")%>%
  dyAxis(name='y', label= "Amplitude")

 
ss <- 1.8523 * (sin(2*pi* f1 * (t-1)) + sin(2*pi* f2 * (t-1))) - 0.94833 * (sin(2*pi* f1 * (t-2)) + sin(2*pi* f2 * (t-2))) + (sin(2*pi* f1 * t) + sin(2*pi* f2 * t)) - 1.9021 * (sin(2*pi* f1 * (t-1)) + sin(2*pi* f2 * (t-1)))  + (sin(2*pi* f1 * (t-2)) + sin(2*pi* f2 * (t-2)))

df1 <- data.frame(time = t, ss)
dygraph(df1)%>%
  dyOptions(drawPoints = TRUE, pointSize = 3) %>%
  dyAxis(name='x', label= "Tempo(s)")%>%
  dyAxis(name='y', label= "Amplitude")

#-------------------------------------------------------------------------------------------------------------------
#Questão 4:
library(dygraphs)

Fs <- 500 # frequência de amostragem em Hz
t <- seq(from=0, to = 5, by = 1/Fs) # vetor de tempo em segundos


f1 <- 30 # frequência de oscilação
y1 <- sin(2*pi* f1 * t) # sinal senoidal oscilando a f1 (em Hz)

df3 <- data.frame(time = t, y1)
dygraph(df3)%>%
  dyOptions(drawPoints = TRUE, pointSize = 3) %>%
  dyAxis(name='x', label= "Tempo em s")%>%
  dyAxis(name='y', label= "Amplitude")



set.seed(1234)
y2 <- y1 + 0.1* max(y1) * rnorm(length(t)) # corrompendo o sinal senoidal com um ruído gaussiano

df4 <- data.frame(time = t, y2)
dygraph(df4)%>%
  dyOptions(drawPoints = TRUE, pointSize = 3) %>%
  dyAxis(name='x', label= "Tempo em s")%>%
  dyAxis(name='y', label= "Amplitude")

L <- 0.001

y2 <- sin(2*pi* f1 * (t-1)) + 0.1* max(sin(2*pi* f1 * (t-1))) * rnorm(length(t-1)) + (1/L) * (y1 + 0.1* max(y1) * rnorm(length(t))) - sin(2*pi* f1 * (t-L)) + 0.1* max(sin(2*pi* f1 * (t-L))) * rnorm(length(t-L))


df5 <- data.frame(time = t, y2)
dygraph(df5)%>%
  dyOptions(drawPoints = TRUE, pointSize = 3) %>%
  dyAxis(name='x', label= "Tempo em s")%>%
  dyAxis(name='y', label= "Amplitude")

#-------------------------------------------------------------------------------------------------------------------
#Questão 5:
library(signal)
library(ggplot2)
library(REdaS)


b <- c(0.5, 0.5) # coeficientes (b) do fitro 
a <- c(1)# coeficientes (a) do fitro

Fs <- 500 # frequência de amostragem em Hz

fresponse <- freqz(b, a, Fs = Fs) # calculo da resposta em frequência

absH <- abs(fresponse$h) # valor absoluto da resposta em frequência
phi <- atan2(Im(fresponse$h), Re(fresponse$h)) # fase em radianos
phi_deg <-  rad2deg(phi) #fase em graus
absH_DB <- 20 * log10(absH)

dfplot <- data.frame(freq = fresponse$f, h = fresponse$h, phi, absH)

ggplot(data = dfplot, aes(x=freq, y=absH)) + geom_line() + theme_classic()

ggplot(data = dfplot, aes(x=freq, y= phi_deg)) + geom_line() + theme_classic()



#-------------------------------------------------------------------------------------------------------------------
#Questão 6:

# gerando sinais biomédicos
library(tuneR)# biblioteca que contém conjunto de funções para gerar sinais básicos
library(dygraphs)#biblioteca para plotagem de séries temporais (gráfico interativo)
library(signal)
library(ggplot2)
library(REdaS)

Fs <- 1000 # frequência de amostragem em Hz
dt <- 1/Fs # resolução temporal
t <- seq(from=0.0, to=10.0, by=dt) #vetor de tempo
tf <- t[length(t)] # valor do tempo final, que é obtido pelo acesso ao último elemento do vetor

freq1 <- 3 #Três burst
ysquare <- square(freq1, samp.rate = Fs, duration = tf+dt,xunit = "time")@left
dygraph(data.frame(time=t, ysquare)) %>%dyRangeSelector()

ynoise <- noise(kind="white", samp.rate = Fs, duration = tf+dt,xunit = "time")@left
dygraph(data.frame(time=t, ynoise)) %>%dyRangeSelector()

# o ruído é adicionado aos trechos em que a onda quadrada é -1

indx1 <- which(ysquare == -1.0) # encontra as posições do vetor ysquare em que sua amplitude é -1.0
y1 <- rep(NA,length(ynoise)) # iniciando um vetor com NA (not available)
y1[indx1] <- 1.2*ynoise[indx1] # 

dygraph(data.frame(time=t, y1)) %>%dyRangeSelector()

indx2 <- which(ysquare > -1.0) # encontra as posições do vetor ysquare em que sua amplitude é 1.0
y1[indx2] <- ynoise[indx2]*20
dygraph(data.frame(time=t, y1), main="Sinal EMG sintético", xlab = "tempo (s)", ylab = "amplitude (mV)") %>%dyRangeSelector()
dfy <- data.frame(time=t, y1)

#Filtro Butterworth
n <- 3 # ordem do filtro
Fs <- 1000 # frequência de amostragem em Hz
Fc <- 5 # frequência de corte em Hz

#  For digital filters, W must be between 0 and 1 where 1 is the
# Nyquist frequency
W <- Fc/(Fs/2)

bt <- butter (n, W, type = "low") # cálculo dos coeficientes do fitro
freqz(filt = bt, Fs = Fs)


# fazendo a filtragem por meio do uso da função filtfilt
y_filt <- filtfilt(filt = bt, dfy$y1)

# plotando sinal original (y1) e sinal filtrado (yf)
# observe  que a defasagem entre o sinal original e o sinal filtrado praticamente desapareceu
dfplot <- data.frame(time = t, yf = y_filt)
dygraph(dfplot)



#Filtro Chebyshev

#  For digital filters, W must be between 0 and 1 where 1 is the
# Nyquist frequency
W <- Fc/(Fs/2)
Rp <- 0.5

bt1 <- cheby1(n, Rp, W, type = "low") # cálculo dos coeficientes do fitro
freqz(filt = bt1, Fs = Fs)


# fazendo a filtragem por meio do uso da função filtfilt
y_filt1 <- filtfilt(filt = bt, dfy$y1)

# plotando sinal original (y1) e sinal filtrado (yf)
# observe  que a defasagem entre o sinal original e o sinal filtrado praticamente desapareceu
dfplot1 <- data.frame(time = t, yf = y_filt1)
dygraph(dfplot1)

