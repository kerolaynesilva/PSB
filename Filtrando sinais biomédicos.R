
library(dygraphs)#biblioteca para plotagem de séries temporais (gráfico interativo)
library(signal)
library(ggplot2)
library(REdaS)


# Definição do diretório de trabalho

getwd() # get current working directory
setwd("C:/Users/samsung/Documents/Universidade/PET EngBiomédica/Workshop PET/Programas/")

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
