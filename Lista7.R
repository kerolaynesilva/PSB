library(ggplot2)
library(dygraphs)
library(tuneR)

fs <- 1000 # frequência de amostragem em Hz
dt <- 1/fs # resolução temporal em segundos

tf <- 10 # duração da série temporal

t <- seq(from=0, to = tf, by = dt) # vetor de tempo em segundos

set.seed(10)

ynoise <- rnorm(length(t)) # sinal exemplo, amostrado a partir de uma distribuição normal
dygraph(data.frame(time=t, ynoise)) %>%dyRangeSelector()

Nwnd <- 100 # tamanho da janela retangular em ms
N <- (Nwnd/1000) * fs # tamanho da janela retangular em número de amostras (obs: t = n * dt)

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
  
  # Dica: sapply é uma função bastante interessante no R, que simplifica a iteração (?sapply)
  ly[!sapply(ly, is.null)] #removendo elementos NULL da lista (retorna elementos não nulos)
  
  # Para apreender mais sobre lapply e sapply faça o tutorial      (https://www.datacamp.com/community/tutorials/r-tutorial-apply-family)
  
  #https://www.rforexcelusers.com/lapply-sapply-mapply-apply-function/
  
}


wngSeg <- splitWithOverlap (ynoise, seg.length = 10*N, overlap = ceiling(0.3*N)) #sobreposição de 30%


yrect1 <- rep(0, length(t)) #iniciando um vetor com zeros (geração do pulso retangular)
yrect1[wngSeg[[1]]] <- 1

yrect2 <- rep(0, length(t)) #iniciando um vetor com zeros (geração do pulso retangular)
yrect2[wngSeg[[3]]] <- 1
  
yrect3 <- rep(0, length(t)) #iniciando um vetor com zeros (geração do pulso retangular)
yrect3[wngSeg[[5]]] <- 1

yrect4 <- rep(0, length(t)) #iniciando um vetor com zeros (geração do pulso retangular)
yrect4[wngSeg[[7]]] <- 1

yrect5 <- rep(0, length(t)) #iniciando um vetor com zeros (geração do pulso retangular)
yrect5[wngSeg[[9]]] <- 1

yrect <- yrect1 + yrect2 + yrect3 + yrect4 + yrect5

df <- data.frame(time = t, ynoise, yrect1, yrect2, yrect3, yrect4, yrect5)
dygraph(df)


y <- ynoise * yrect # realizando o janelamento de sinais


# plotando o sinal original e os pulsos retangulares
df <- data.frame(time = t, ynoise, yrect)
dg1 <- dygraph(df, group = "G1")

# plotando o produto entre o sinal original e os pulsos retangulares
df2 <- data.frame(time = t, y)
dg2 <- dygraph(df2, group = "G1")

# render the dygraphs objects using htmltools
htmltools::browsable(htmltools::tagList(list(dg1, dg2)))

tc <- rep(NA, length(wngSeg)) # vetor que armazena o tempo médio de cada segmento
v.rms <- rep(NA, length(wngSeg)) # vetor que armazena o valor rms
v.media <- rep(NA, length(wngSeg)) # vetor que armazena a média

for (i in 1:length(wngSeg)){
  
  tc[i] <- mean(wngSeg[[i]]) * dt
  v.media[i] <- mean( y[wngSeg[[i]]] )
  v.rms[i] <-  sqrt ( sum ( y[wngSeg[[i]]] *  y[wngSeg[[i]]] ) / length(wngSeg[[i]]) )
}

qplot(t, y, geom=c("line"))
qplot(tc, v.media, geom=c("line"))
qplot(tc, v.rms,geom=c("line"))

cc <- ccf(v.rms, y,lag.max = 10)

g <- ynoise * yrect1 # realizando o janelamento de sinais
dfg <- data.frame(time = t, g)
dygraph(dfg)

# subamostrando o potencial de ação para que o mesmo tenha 100 amostras
yobj <- spline(x=1:10001, g, n=100)

df1 <- data.frame(time = 1:length(g), g)
df2 <- data.frame(time = yobj$x, y=yobj$y)


p <- ggplot()
p <- p +   geom_line(data = df1, aes(x=time, y=g),  color= 'red')
p <- p +  geom_point(data = df2, aes(x=time, y=y),  color= 'black')
p <- p + theme_bw()
p


g <- rep(0,length(t))
g[1:length(df2$y)] <- df2$y

plot(df2$y)

qplot(x = 1:length(g), y= g)

cc <- ccf(y, g,lag.max = 10000)

df3 <- data.frame(time=cc$lag, acf = cc$acf)

cc$lag[which.max(cc$acf)] #índice do valor máximo da acf

t.inicial <- data.frame(time= df3$time)
t.inicial$t <- NA

t.final <- data.frame(time= df3$time)
t.final$t <- NA

i = 1

while (i < length(df3$time)) {
if (df3$acf[i] > 0.015){
  
  t.inicial$t[i] <- df3$time[i]
  
  i = i + 1000
  
  t.final$t[i] <- df3$time[i]
}
  i = i + 1

}



dygraph(df3) %>% dyLimit(0.02, color = "red") 
  

p <- dygraph(df3)
for (i in 10000:length(t.inicial$time)[1]){
  p <- p %>% dyEvent(date = t.inicial$t[i], label='Início', labelLoc='bottom') 
}
for (i in 10000:length(t.final$time)[1]){
  p <- p %>% dyEvent(date = t.final$t[i], label='Fim', labelLoc='bottom') 
}
p


cc$lag[which(cc$acf > 0.015),,]










