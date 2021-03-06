
# Gera��o de sinal cont�nuo sint�tico senoidal-------------------------------------------------------

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


# Gera��o de sinais com descontinuidades-----------------------------------------------------------
# gerando sinais biom�dicos
library(tuneR)# biblioteca que cont�m conjunto de fun��es para gerar sinais b�sicos
library(dygraphs)#biblioteca para plotagem de s�ries temporais (gr�fico interativo)

Fs <- 300 # frequ�ncia de amostragem em Hz
dt <- 1/Fs # resolu��o temporal
t <- seq(from=0.0, to=10.0, by=dt) #vetor de tempo
tf <- t[length(t)] # valor do tempo final, que � obtido pelo acesso ao �ltimo elemento do vetor

freq1 <- 10
ysquare <- square(freq1, samp.rate = Fs, duration = tf+dt,xunit = "time")@left
dygraph(data.frame(time=t, ysquare)) %>%dyRangeSelector()

ynoise <- noise(kind="white", samp.rate = Fs, duration = tf+dt,xunit = "time")@left
dygraph(data.frame(time=t, ynoise)) %>%dyRangeSelector()

# o ru�do � adicionado aos trechos em que a onda quadrada � -1

indx1 <- which(ysquare == -1.0) # encontra as posi��es do vetor ysquare em que sua amplitude � -1.0
y1 <- rep(NA,length(ynoise)) # iniciando um vetor com NA (not available)
y1[indx1] <- ynoise[indx1] # 

dygraph(data.frame(time=t, y1)) %>%dyRangeSelector()

indx2 <- which(ysquare > -1.0) # encontra as posi��es do vetor ysquare em que sua amplitude � 1.0
y1[indx2] <- ynoise[indx2]*20
dygraph(data.frame(time=t, y1), main="Sinal EMG sint�tico", xlab = "tempo (s)", ylab = "amplitude (mV)") %>%dyRangeSelector()


# Gerando descontinuidades (simulada pela presen�a de valor extremo)----------------------------------------------

d <- 1000 # valor da amplitude da descontinuidade

Ndes <- 10 # n�mero de descontinuidades a serem geradas

set.seed(42); # ajustando a "semente" do gerador de n�meros aleat�rios para que seja poss�vel gerar n�meros aleat�rios iguais (se isto n�o for feito os n�mero aleat�ricos que voc� gerar� ser� diferente dos apresentandos no exemplo - a raz�o de se colocar este comando � que voc� consiga reproduzir os resultados deste exemplo)

indxDesc <- sample.int(length(y1),Ndes) # gerador de n�mero aleat�rios - para ver o significado dos argumentos digite: help(sample.int)

y2 <- y1 # iniciando um vetor com NA (not available)
y2[indxDesc] <- d # atribuindo o valor de descontinuidade
dygraph(data.frame(time=t, y2)) %>%dyRangeSelector()

# Estat�sticas b�sicas do sinal sem a descontinuidade
summary(y1)

# Estat�sticas b�sicas do sinal com a descontinuidade
summary(y2)

library(plotly) # biblioteca gr�fica para plotagem de gr�ficos em geral (que permite intera��o com o gr�fico)

# Detec��o de descontinuidade - Uma descontinuidade pode ser vista como valores extremos

p <- plot_ly(y = y1, type = "box", name = "Y1 - Sem descontinuidade") %>%
  add_trace(y = y2, name="Y2 - Com descontinuidade")
p # mostrando gr�fico




