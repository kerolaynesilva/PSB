
# Geração de sinal contínuo sintético senoidal-------------------------------------------------------

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


# Geração de sinais com descontinuidades-----------------------------------------------------------
# gerando sinais biomédicos
library(tuneR)# biblioteca que contém conjunto de funções para gerar sinais básicos
library(dygraphs)#biblioteca para plotagem de séries temporais (gráfico interativo)

Fs <- 300 # frequência de amostragem em Hz
dt <- 1/Fs # resolução temporal
t <- seq(from=0.0, to=10.0, by=dt) #vetor de tempo
tf <- t[length(t)] # valor do tempo final, que é obtido pelo acesso ao último elemento do vetor

freq1 <- 10
ysquare <- square(freq1, samp.rate = Fs, duration = tf+dt,xunit = "time")@left
dygraph(data.frame(time=t, ysquare)) %>%dyRangeSelector()

ynoise <- noise(kind="white", samp.rate = Fs, duration = tf+dt,xunit = "time")@left
dygraph(data.frame(time=t, ynoise)) %>%dyRangeSelector()

# o ruído é adicionado aos trechos em que a onda quadrada é -1

indx1 <- which(ysquare == -1.0) # encontra as posições do vetor ysquare em que sua amplitude é -1.0
y1 <- rep(NA,length(ynoise)) # iniciando um vetor com NA (not available)
y1[indx1] <- ynoise[indx1] # 

dygraph(data.frame(time=t, y1)) %>%dyRangeSelector()

indx2 <- which(ysquare > -1.0) # encontra as posições do vetor ysquare em que sua amplitude é 1.0
y1[indx2] <- ynoise[indx2]*20
dygraph(data.frame(time=t, y1), main="Sinal EMG sintético", xlab = "tempo (s)", ylab = "amplitude (mV)") %>%dyRangeSelector()


# Gerando descontinuidades (simulada pela presença de valor extremo)----------------------------------------------

d <- 1000 # valor da amplitude da descontinuidade

Ndes <- 10 # número de descontinuidades a serem geradas

set.seed(42); # ajustando a "semente" do gerador de números aleatórios para que seja possível gerar números aleatórios iguais (se isto não for feito os número aleatóricos que você gerará será diferente dos apresentandos no exemplo - a razão de se colocar este comando é que você consiga reproduzir os resultados deste exemplo)

indxDesc <- sample.int(length(y1),Ndes) # gerador de número aleatórios - para ver o significado dos argumentos digite: help(sample.int)

y2 <- y1 # iniciando um vetor com NA (not available)
y2[indxDesc] <- d # atribuindo o valor de descontinuidade
dygraph(data.frame(time=t, y2)) %>%dyRangeSelector()

# Estatísticas básicas do sinal sem a descontinuidade
summary(y1)

# Estatísticas básicas do sinal com a descontinuidade
summary(y2)

library(plotly) # biblioteca gráfica para plotagem de gráficos em geral (que permite interação com o gráfico)

# Detecção de descontinuidade - Uma descontinuidade pode ser vista como valores extremos

p <- plot_ly(y = y1, type = "box", name = "Y1 - Sem descontinuidade") %>%
  add_trace(y = y2, name="Y2 - Com descontinuidade")
p # mostrando gráfico




