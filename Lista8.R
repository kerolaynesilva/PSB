
#Quest�o 2:

library(dygraphs)

Fs <- 1200 # frequ�ncia de amostragem em Hz
t <- seq(from=0, to = 5, by = 1/Fs) # vetor de tempo em segundos


f1 <- 60 # frequ�ncia de oscila��o
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
#Quest�o 4:
library(dygraphs)

Fs <- 500 # frequ�ncia de amostragem em Hz
t <- seq(from=0, to = 5, by = 1/Fs) # vetor de tempo em segundos


f1 <- 30 # frequ�ncia de oscila��o
y1 <- sin(2*pi* f1 * t) # sinal senoidal oscilando a f1 (em Hz)

df3 <- data.frame(time = t, y1)
dygraph(df3)%>%
  dyOptions(drawPoints = TRUE, pointSize = 3) %>%
  dyAxis(name='x', label= "Tempo em s")%>%
  dyAxis(name='y', label= "Amplitude")



set.seed(1234)
y2 <- y1 + 0.1* max(y1) * rnorm(length(t)) # corrompendo o sinal senoidal com um ru�do gaussiano

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
#Quest�o 5:
library(signal)
library(ggplot2)
library(REdaS)


b <- c(0.5, 0.5) # coeficientes (b) do fitro 
a <- c(1)# coeficientes (a) do fitro

Fs <- 500 # frequ�ncia de amostragem em Hz

fresponse <- freqz(b, a, Fs = Fs) # calculo da resposta em frequ�ncia

absH <- abs(fresponse$h) # valor absoluto da resposta em frequ�ncia
phi <- atan2(Im(fresponse$h), Re(fresponse$h)) # fase em radianos
phi_deg <-  rad2deg(phi) #fase em graus
absH_DB <- 20 * log10(absH)

dfplot <- data.frame(freq = fresponse$f, h = fresponse$h, phi, absH)

ggplot(data = dfplot, aes(x=freq, y=absH)) + geom_line() + theme_classic()

ggplot(data = dfplot, aes(x=freq, y= phi_deg)) + geom_line() + theme_classic()



#-------------------------------------------------------------------------------------------------------------------
#Quest�o 6:

# gerando sinais biom�dicos
library(tuneR)# biblioteca que cont�m conjunto de fun��es para gerar sinais b�sicos
library(dygraphs)#biblioteca para plotagem de s�ries temporais (gr�fico interativo)
library(signal)
library(ggplot2)
library(REdaS)

Fs <- 1000 # frequ�ncia de amostragem em Hz
dt <- 1/Fs # resolu��o temporal
t <- seq(from=0.0, to=10.0, by=dt) #vetor de tempo
tf <- t[length(t)] # valor do tempo final, que � obtido pelo acesso ao �ltimo elemento do vetor

freq1 <- 3 #Tr�s burst
ysquare <- square(freq1, samp.rate = Fs, duration = tf+dt,xunit = "time")@left
dygraph(data.frame(time=t, ysquare)) %>%dyRangeSelector()

ynoise <- noise(kind="white", samp.rate = Fs, duration = tf+dt,xunit = "time")@left
dygraph(data.frame(time=t, ynoise)) %>%dyRangeSelector()

# o ru�do � adicionado aos trechos em que a onda quadrada � -1

indx1 <- which(ysquare == -1.0) # encontra as posi��es do vetor ysquare em que sua amplitude � -1.0
y1 <- rep(NA,length(ynoise)) # iniciando um vetor com NA (not available)
y1[indx1] <- 1.2*ynoise[indx1] # 

dygraph(data.frame(time=t, y1)) %>%dyRangeSelector()

indx2 <- which(ysquare > -1.0) # encontra as posi��es do vetor ysquare em que sua amplitude � 1.0
y1[indx2] <- ynoise[indx2]*20
dygraph(data.frame(time=t, y1), main="Sinal EMG sint�tico", xlab = "tempo (s)", ylab = "amplitude (mV)") %>%dyRangeSelector()
dfy <- data.frame(time=t, y1)

#Filtro Butterworth
n <- 3 # ordem do filtro
Fs <- 1000 # frequ�ncia de amostragem em Hz
Fc <- 5 # frequ�ncia de corte em Hz

#  For digital filters, W must be between 0 and 1 where 1 is the
# Nyquist frequency
W <- Fc/(Fs/2)

bt <- butter (n, W, type = "low") # c�lculo dos coeficientes do fitro
freqz(filt = bt, Fs = Fs)


# fazendo a filtragem por meio do uso da fun��o filtfilt
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

bt1 <- cheby1(n, Rp, W, type = "low") # c�lculo dos coeficientes do fitro
freqz(filt = bt1, Fs = Fs)


# fazendo a filtragem por meio do uso da fun��o filtfilt
y_filt1 <- filtfilt(filt = bt, dfy$y1)

# plotando sinal original (y1) e sinal filtrado (yf)
# observe  que a defasagem entre o sinal original e o sinal filtrado praticamente desapareceu
dfplot1 <- data.frame(time = t, yf = y_filt1)
dygraph(dfplot1)

