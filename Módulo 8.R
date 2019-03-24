#Introdução aos filtros digitais#
#Filtros tipos FIR leva em consideração apenas as entradas x[n] (Sistema malha aberta) 
#filtros IIR dependem  das entradas e saídas do sistema (Sistema malha fechada)
#Para filtrar um sinal aplica função filtfilt e coloca como parâmetros o sinal a ser filtrado e o tipo de filtro a ser aplicado

library(dygraphs)

n <- 0:31 # tempo discreto n
omega <- 0.25 * pi # frequência digital
A <- 2 # amplitude
xn <- A * sin(omega * n) # geração da senóide

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

Fs <- 500 # frequência de amostragem em Hz

fresponse <- freqz(b, a, Fs = Fs) # calculo da resposta em frequência em Z

absH <- abs(fresponse$h) # valor absoluto da resposta em frequência
phi <- atan2(Im(fresponse$h), Re(fresponse$h)) # fase em radianos, quando é realizado uma filtragem a fase do sinal é alterado
phi_deg <-  rad2deg(phi) #fase em graus
absH_DB <- 20 * log10(absH)

dfplot <- data.frame(freq = fresponse$f, h = fresponse$h, phi, absH)

ggplot(data = dfplot, aes(x=freq, y=absH)) + geom_line() + theme_classic()
ggplot(data = dfplot, aes(x=freq, y= phi_deg)) + geom_line() + theme_classic()
ggplot(data = dfplot, aes(x=freq, y=absH_DB)) + geom_line() + theme_classic()


# Função freqz plota as a resposta em frequência de um filtro passa-baixa.
freqz(b, a, Fs = Fs)



#Filtragem por convolução
library(dygraphs)

Fs <- 500 # frequência de amostragem em Hz
t <- seq(from=0, to = 1, by = 1/Fs) # vetor de tempo em segundos


f1 <- 20 # frequência de oscilação
y1 <- sin(2*pi* f1 * t) # sinal senoidal oscilando a f1 (em Hz)


set.seed(1234)
y2 <- y1 + 0.3* max(y1) * rnorm(length(t)) # corrompendo o sinal senoidal com um ruído gaussiano


# projetando o filtro de média móvel
L <- 5 #Número de coeficientes do filtro
bt <- Arma(b = rep(1, L)/L , a = 1) #Representação ou modelo computacional (frequência de pulsos, função rep, com comprimento de filtro diferentes)
freqz(filt = bt, Fs = Fs)

# fazendo a filtragem por meio da operação de convolução
y_filt <- conv(y2, rep(1, L)/L)

# plotando sinal original (y1), sinal corrompido (y2) e sinal filtrado (yf)
# observe a defasagem entre o sinal original e o sinal filtrado
dfplot <- data.frame(time =t, y1, y2, yf = y_filt[1:length(y1)])
dygraph(dfplot)

# fazendo a filtragem por meio do uso da função filter
y_filt <- filter(filt = bt, y2)

# plotando sinal original (y1), sinal corrompido (y2) e sinal filtrado (yf)
# observe a defasagem entre o sinal original e o sinal filtrado
dfplot <- data.frame(time =t, y1, y2, yf = y_filt)
dygraph(dfplot)

# fazendo a filtragem por meio do uso da função filtfilt
y_filt <- filtfilt(filt = bt, y2)  #Sempre utilizar essa função, pois dentro dela possui o método de convolução e refaz esse método diversas vezes, resultando em um sinal melhor filtrado

# plotando sinal original (y1), sinal corrompido (y2) e sinal filtrado (yf)
# observe  que a defasagem entre o sinal original e o sinal filtrado praticamente desapareceu
dfplot <- data.frame(time =t, y1, y2, yf = y_filt)
dygraph(dfplot)


##Filtro Butterworth
n <- 5 # ordem do filtro
Fs <- 500 # frequência de amostragem em Hz
Fc <- 10 # frequência de corte em Hz

#  For digital filters, W must be between 0 and 1 where 1 is the
# Nyquist frequency
W <- Fc/(Fs/2)

bt <- butter(n, W, type = "low") # cálculo dos coeficientes do fitro passa-baixa butterworth
freqz(filt = bt, Fs = Fs)

bt <- butter(n, W, type = "high") # cálculo dos coeficientes do fitro passa-alta butterworth
freqz(filt = bt, Fs = Fs)


Fc_l <- 10 # frequência de corte inferior em Hz

Fc_u <- 60 # frequência de corte superior em Hz

#  For digital filters, W must be between 0 and 1 where 1 is the
# Nyquist frequency
W <- c(Fc_l, Fc_u)/(Fs/2)

bt <- butter(n, W, type = "pass") # cálculo dos coeficientes do fitro passa-banda
freqz(filt = bt, Fs = Fs)

bt <- butter(n, W, type = "stop") # cálculo dos coeficientes do fitro rejeita-faixa
freqz(filt = bt, Fs = Fs)


#Filtro Chebyshev
n <- 5 # ordem do filtro
Fs <- 500 # frequência de amostragem em Hz
Fc <- 100 # frequência de corte em Hz

#  For digital filters, W must be between 0 and 1 where 1 is the
# Nyquist frequency
W <- Fc/(Fs/2)
Rp <- 0.5  #Diminuir a oscilação do filtro em dB

bt <- cheby1(n, Rp, W, type = "low") # cálculo dos coeficientes do fitro passa-baixa
freqz(filt = bt, Fs = Fs)

bt <- cheby1(n, Rp, W, type = "high") # cálculo dos coeficientes do fitro passa-alta
freqz(filt = bt, Fs = Fs)

Fc_l <- 10 # frequência de corte inferior em Hz

Fc_u <- 60 # frequência de corte superior em Hz

#  For digital filters, W must be between 0 and 1 where 1 is the
# Nyquist frequency
W <- c(Fc_l, Fc_u)/(Fs/2)

bt <-  cheby1(n, Rp, W, type = "pass") # cálculo dos coeficientes do fitro passa-banda
freqz(filt = bt, Fs = Fs)

bt <-  cheby1(n, Rp, W, type = "stop") # cálculo dos coeficientes do fitro rejeita-faixa
freqz(filt = bt, Fs = Fs)


#Comparação entre filtros Butterworth e Chebyshev
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

