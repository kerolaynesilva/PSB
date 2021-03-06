#Introdu��o aos filtros digitais#
#Filtros tipos FIR leva em considera��o apenas as entradas x[n] (Sistema malha aberta) 
#filtros IIR dependem  das entradas e sa�das do sistema (Sistema malha fechada)
#Para filtrar um sinal aplica fun��o filtfilt e coloca como par�metros o sinal a ser filtrado e o tipo de filtro a ser aplicado

library(dygraphs)

n <- 0:31 # tempo discreto n
omega <- 0.25 * pi # frequ�ncia digital
A <- 2 # amplitude
xn <- A * sin(omega * n) # gera��o da sen�ide

df <- data.frame(time = n, xn)
dygraph(df)%>%
  dyOptions(drawPoints = TRUE, pointSize = 3) %>%
  dyAxis(name='x', label= "Tempo discreto, n")%>%
  dyAxis(name='y', label= "Amplitude")


library(signal)
library(ggplot2)
library(REdaS) #BIBILIOTECA CONVERTE GRAUS EM RADIANOS

#L determina faixa de filtragem do sinal

b <- c(0.5, 0.5) # coeficientes (b) do fitro 
a <- c(1)# coeficientes (a) do fitro

Fs <- 500 # frequ�ncia de amostragem em Hz

fresponse <- freqz(b, a, Fs = Fs) # calculo da resposta em frequ�ncia em Z

absH <- abs(fresponse$h) # valor absoluto da resposta em frequ�ncia
phi <- atan2(Im(fresponse$h), Re(fresponse$h)) # fase em radianos, quando � realizado uma filtragem a fase do sinal � alterado
phi_deg <-  rad2deg(phi) #fase em graus
absH_DB <- 20 * log10(absH)

dfplot <- data.frame(freq = fresponse$f, h = fresponse$h, phi, absH)

ggplot(data = dfplot, aes(x=freq, y=absH)) + geom_line() + theme_classic()
ggplot(data = dfplot, aes(x=freq, y= phi_deg)) + geom_line() + theme_classic()
ggplot(data = dfplot, aes(x=freq, y=absH_DB)) + geom_line() + theme_classic()


# Fun��o freqz plota as a resposta em frequ�ncia de um filtro passa-baixa.
freqz(b, a, Fs = Fs)



#Filtragem por convolu��o
library(dygraphs)

Fs <- 500 # frequ�ncia de amostragem em Hz
t <- seq(from=0, to = 1, by = 1/Fs) # vetor de tempo em segundos


f1 <- 20 # frequ�ncia de oscila��o
y1 <- sin(2*pi* f1 * t) # sinal senoidal oscilando a f1 (em Hz)


set.seed(1234)
y2 <- y1 + 0.3* max(y1) * rnorm(length(t)) # corrompendo o sinal senoidal com um ru�do gaussiano


# projetando o filtro de m�dia m�vel
L <- 5 #N�mero de coeficientes do filtro
bt <- Arma(b = rep(1, L)/L , a = 1) #Representa��o ou modelo computacional (frequ�ncia de pulsos, fun��o rep, com comprimento de filtro diferentes)
freqz(filt = bt, Fs = Fs)

# fazendo a filtragem por meio da opera��o de convolu��o
y_filt <- conv(y2, rep(1, L)/L)

# plotando sinal original (y1), sinal corrompido (y2) e sinal filtrado (yf)
# observe a defasagem entre o sinal original e o sinal filtrado
dfplot <- data.frame(time =t, y1, y2, yf = y_filt[1:length(y1)])
dygraph(dfplot)

# fazendo a filtragem por meio do uso da fun��o filter
y_filt <- filter(filt = bt, y2)

# plotando sinal original (y1), sinal corrompido (y2) e sinal filtrado (yf)
# observe a defasagem entre o sinal original e o sinal filtrado
dfplot <- data.frame(time =t, y1, y2, yf = y_filt)
dygraph(dfplot)

# fazendo a filtragem por meio do uso da fun��o filtfilt
y_filt <- filtfilt(filt = bt, y2)  #Sempre utilizar essa fun��o, pois dentro dela possui o m�todo de convolu��o e refaz esse m�todo diversas vezes, resultando em um sinal melhor filtrado

# plotando sinal original (y1), sinal corrompido (y2) e sinal filtrado (yf)
# observe  que a defasagem entre o sinal original e o sinal filtrado praticamente desapareceu
dfplot <- data.frame(time =t, y1, y2, yf = y_filt)
dygraph(dfplot)


##Filtro Butterworth
n <- 5 # ordem do filtro
Fs <- 500 # frequ�ncia de amostragem em Hz
Fc <- 10 # frequ�ncia de corte em Hz

#  For digital filters, W must be between 0 and 1 where 1 is the
# Nyquist frequency
W <- Fc/(Fs/2)

bt <- butter(n, W, type = "low") # c�lculo dos coeficientes do fitro passa-baixa butterworth
freqz(filt = bt, Fs = Fs)

bt <- butter(n, W, type = "high") # c�lculo dos coeficientes do fitro passa-alta butterworth
freqz(filt = bt, Fs = Fs)


Fc_l <- 10 # frequ�ncia de corte inferior em Hz

Fc_u <- 60 # frequ�ncia de corte superior em Hz

#  For digital filters, W must be between 0 and 1 where 1 is the
# Nyquist frequency
W <- c(Fc_l, Fc_u)/(Fs/2)

bt <- butter(n, W, type = "pass") # c�lculo dos coeficientes do fitro passa-banda
freqz(filt = bt, Fs = Fs)

bt <- butter(n, W, type = "stop") # c�lculo dos coeficientes do fitro rejeita-faixa
freqz(filt = bt, Fs = Fs)


#Filtro Chebyshev
n <- 5 # ordem do filtro
Fs <- 500 # frequ�ncia de amostragem em Hz
Fc <- 100 # frequ�ncia de corte em Hz

#  For digital filters, W must be between 0 and 1 where 1 is the
# Nyquist frequency
W <- Fc/(Fs/2)
Rp <- 0.5  #Diminuir a oscila��o do filtro em dB

bt <- cheby1(n, Rp, W, type = "low") # c�lculo dos coeficientes do fitro passa-baixa
freqz(filt = bt, Fs = Fs)

bt <- cheby1(n, Rp, W, type = "high") # c�lculo dos coeficientes do fitro passa-alta
freqz(filt = bt, Fs = Fs)

Fc_l <- 10 # frequ�ncia de corte inferior em Hz

Fc_u <- 60 # frequ�ncia de corte superior em Hz

#  For digital filters, W must be between 0 and 1 where 1 is the
# Nyquist frequency
W <- c(Fc_l, Fc_u)/(Fs/2)

bt <-  cheby1(n, Rp, W, type = "pass") # c�lculo dos coeficientes do fitro passa-banda
freqz(filt = bt, Fs = Fs)

bt <-  cheby1(n, Rp, W, type = "stop") # c�lculo dos coeficientes do fitro rejeita-faixa
freqz(filt = bt, Fs = Fs)


#Compara��o entre filtros Butterworth e Chebyshev
bf <- butter(5, 0.1)
cf <- cheby1(5, 3, 0.1)
bfr <- freqz(bf)
cfr <- freqz(cf)
plot(bfr$f/pi, 20 * log10(abs(bfr$h)), type = "l", ylim = c(-40, 0),
     xlim = c(0, .5), xlab = "Frequency", ylab = c("dB"))
lines(cfr$f/pi, 20 * log10(abs(cfr$h)), col = "red")


# compare type I and type II Chebyshev filters.
c1fr <- freqz(cheby1(5, .5, 0.5))
c2fr <- freqz(cheby2(5, 20, 0.5))
plot(c1fr$f/pi, abs(c1fr$h), type = "l", ylim = c(0, 1),
     xlab = "Frequency", ylab = c("Magnitude"))
lines(c2fr$f/pi, abs(c2fr$h), col = "red")

