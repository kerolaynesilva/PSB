
###JANELAMENTO DE SINAIS ###

library(dygraphs)
library(htmltools)

fs <- 1000 # frequ�ncia de amostragem em Hz
dt <- 1/fs # resolu��o temporal em segundos

tf <- 10 # dura��o da s�rie temporal

t <- seq(from=0, to = tf, by = dt) # vetor de tempo em segundos

set.seed(1234)

y <- rnorm(length(t)) # sinal exemplo, amostrado a partir de uma distribui��o normal


Nwnd <- 100 # tamanho da janela retangular em ms
N <- (Nwnd/1000) * fs # tamanho da janela retangular em n�mero de amostras (obs: t = n * dt)


yrect <- rep(0, length(t)) #iniciando um vetor com zeros (gera��o do pulso retangular) - fun��o rep repeti o n�mero 0 length(t) vezes

p1 <- c(50, 300, 700, 1500, 3000, 8000, 9000) # tempo discreto inicial das janelas
p2 <- p1 + N-1 # tempo discreto final das janelas


for (i in 1:length(p1)){
  yrect[p1[i]:p2[i]] <- 1
}

ywnd <- y * yrect # realizando o janelamento de sinais

# plotando o sinal original e os pulsos retangulares
df <- data.frame(time = t, y, yrect)
dg1 <- dygraph(df, group = "G1")

# plotando o produto entre o sinal original e os pulsos retangulares
df2 <- data.frame(time = t, ywnd)
dg2 <- dygraph(df2, group = "G1") #Comando group associa v�rios dygraphs para conseguir sincronizar gr�ficos

# render the dygraphs objects using htmltools
htmltools::browsable(htmltools::tagList(list(dg1, dg2)))



# Fun��o que realiza o janelamento com sobreposi��o - Sobrepor � bom para evitar ru�dos e interfer�ncias indesej�veis no sinal coletado
# seg.length = tamanho da janela (em amostras)
# overlap = n�mero de amostras para sobreposi��o 
# y = s�rie temporal a ser segmentada
# A fun��o retorna os �ndices de cada segmento, na forma de uma lista, cujo n�mero total de elementos coincide com o n�mero de segmentos

splitWithOverlap <- function(y, seg.length, overlap) {
  vec <- 1:length(y)
  starts = seq(1, length(vec), by=seg.length-overlap)
  ends   = starts + seg.length - 1
  ends[ends > length(vec)] = length(vec) #garantindo que o �ndice final n�o ultrapasse o tamanho do vetor
  
  # Dica: lapply � uma fun��o bastante interessante no R, que simplifica a itera��o (?lapply)
  #Lista com �nicio e final de cada janela
  ly <- lapply(1:length(starts), 
               function(i) 
                 if( (ends[i]-starts[i]+1) ==  seg.length){
                   y <- vec[starts[i]:ends[i]]
                 }
  )
  
  # Dica: sapply � uma fun��o bastante interessante no R, que simplifica a itera��o (?sapply)
  ly[!sapply(ly, is.null)] #removendo elementos NULL da lista (retorna elementos n�o nulos)
  
  # Para apreender mais sobre lapply e sapply fa�a o tutorial      (https://www.datacamp.com/community/tutorials/r-tutorial-apply-family)
  
  #https://www.rforexcelusers.com/lapply-sapply-mapply-apply-function/
  
}


wngSeg <- splitWithOverlap (y, seg.length = N, overlap = ceiling(N/2)) 


yrect1 <- rep(0, length(t)) #iniciando um vetor com zeros (gera��o do pulso retangular)
yrect1[wngSeg[[1]]] <- 1

yrect2 <- rep(0, length(t)) #iniciando um vetor com zeros (gera��o do pulso retangular)
yrect2[wngSeg[[2]]] <- 1

yrect3 <- rep(0, length(t)) #iniciando um vetor com zeros (gera��o do pulso retangular)
yrect3[wngSeg[[3]]] <- 1

yrect4 <- rep(0, length(t)) #iniciando um vetor com zeros (gera��o do pulso retangular)
yrect4[wngSeg[[4]]] <- 1

df <- data.frame(time = t, y, yrect1 + 5, yrect2 - 5, yrect3 + 5, yrect4 - 5)
dygraph(df)
 
library(ggplot2)

t <- seq(from=0, to = tf, by = dt) # vetor de tempo em segundos

set.seed(1234)

y <- (2 + 0.5*t) + rnorm(length(t))

tc <- rep(NA, length(wngSeg)) # vetor que armazena o tempo m�dio de cada segmento
v.media <- rep(NA, length(wngSeg)) # vetor que armazena a m�dia
v.rms <- rep(NA, length(wngSeg)) # vetor que armazena o valor rms
v.sd <- rep(NA, length(wngSeg)) # vetor que armazena o desvio padr�o
v.var <- rep(NA, length(wngSeg)) # vetor que armazena a vari�ncia


for (i in 1:length(wngSeg)){
  
  tc[i] <- mean(wngSeg[[i]]) * dt
  v.media[i] <- mean( y[wngSeg[[i]]] )
  v.rms[i] <-  sqrt ( sum ( y[wngSeg[[i]]] *  y[wngSeg[[i]]] ) / length(wngSeg[[i]]) )
  v.sd[i] <- sd( y[wngSeg[[i]]] )
  v.var[i] <- var( y[wngSeg[[i]]] )
  
}

qplot(t, y, geom=c("line"))
qplot(tc, v.media)
qplot(tc, v.rms)
qplot(tc, v.sd)
qplot(tc, v.var)


##Detec��o e remo��o de tend�ncias lineares e n�o lineares
df <- data.frame(time = t, y)

# regress�o linear
linearMod <- lm(y ~ t, data=df) 

# y = Intercept + (?? ??? t)
print(linearMod)

distPred <- predict(linearMod, df)  # predi��o

plot(df$time,df$y)
lines(df$time, distPred,type='l',col='blue')

# remo��o da tend�ncia linear
plot(df$time,df$y-distPred)

yy <- df$y-distPred + sin(2*pi*t * 0.1)
qplot(t, yy, geom=c("line"))

df <- data.frame(time=t, yy)


fit.lm <- lm(yy~poly(time,5), data = df) #polin�mio da ordem 5 se ajusta a tend�ncia do sinal analisado

# access the fitted series (for plotting)
fit <- fitted(fit.lm)

plot(df$time,df$y, type='l') # tipe significa o tipo de plotagem do gr�fico, nesse caso � linha
lines(df$time,fit,col='red')

# remo��o da tend�ncia n�o linear
plot(df$time,df$y-fit)


t <- seq(from=0, to = tf, by = dt) # vetor de tempo em segundos

set.seed(1234)

y <- df$y-distPred + sin(2*pi*t * 0.1)

tc <- rep(NA, length(wngSeg)) # vetor que armazena o tempo m�dio de cada segmento
v.media <- rep(NA, length(wngSeg)) # vetor que armazena a m�dia
v.rms <- rep(NA, length(wngSeg)) # vetor que armazena o valor rms
v.sd <- rep(NA, length(wngSeg)) # vetor que armazena o desvio padr�o
v.var <- rep(NA, length(wngSeg)) # vetor que armazena a vari�ncia


for (i in 1:length(wngSeg)){
  
  tc[i] <- mean(wngSeg[[i]]) * dt
  v.media[i] <- mean( y[wngSeg[[i]]] )
  v.rms[i] <-  sqrt ( sum ( y[wngSeg[[i]]] *  y[wngSeg[[i]]] ) / length(wngSeg[[i]]) )
  v.sd[i] <- sd( y[wngSeg[[i]]] )
  v.var[i] <- var( y[wngSeg[[i]]] )
  
}

qplot(t, y, geom=c("line"))

qplot(tc, v.rms)



##M�dia coerente
# Considere o modelo do potencial de a��o da fibra muscular
u <- 0.001
eq = function(t){u*t*(2-u*t)*exp(-u*t)}
plot(eq(1:10000), type='l')
# Atribuindo o sinal do modelo a uma vari�vel

y <- eq(1:10000)


# Fun��o que gera r�plicas do sinal corrompido

rep.row<-function(x,n){
  
  A <- matrix(NA,nrow=n, ncol = length(x))
  
  for (i in 1:n){
    
    A[i,] <- x+ rnorm(length(x))
    
  }
  
  return(A)
  
}

# Um �nico exemplo de sinal corrompido
rr <- rep.row(y, 1)
plot(colMeans(rr), type='l')
lines(y, col='red', type = 'l')

# M�dia coerente de 10 amostras
rr <- rep.row(y, 10)
plot(colMeans(rr), type='l')
lines(y, col='red', type = 'l')

# M�dia coerente de 100 amostras
rr <- rep.row(y, 100)
plot(colMeans(rr), type='l')
lines(y, col='red', type = 'l')

# M�dia coerente de 1000 amostras
rr <- rep.row(y, 1000)
plot(colMeans(rr), type='l')
lines(y, col='red', type = 'l')

# M�dia coerente de 10000 amostras
rr <- rep.row(y, 10000)
plot(colMeans(rr), type='l')
lines(y, col='red', type = 'l')


##Convolu��o de tempo discreto##
library(ggplot2)

# Considere o modelo do potencial de a��o da fibra muscular
u <- 0.001
eq = function(t){u*t*(2-u*t)*exp(-u*t)}
N <- 10000
plot(eq(1:N), type='l')


# Atribuindo o sinal do modelo a uma vari�vel

y <- eq(1:N)


# subamostrando o potencial de a��o para que o mesmo tenha 100 amostras
yobj <- spline(x=1:N, y, n=100)

df1 <- data.frame(time = 1:length(y), y)
df2 <- data.frame(time = yobj$x, y=yobj$y)


p <- ggplot()
p <- p +   geom_line(data = df1, aes(x=time, y=y),  color= 'red')
p <- p +  geom_point(data = df2, aes(x=time, y=y),  color= 'black')
p <- p + theme_bw()
p

# Simulando um trem de potencial de a��o

t <- 1:10000
y <- runif(length(t), min=0, max=0.05)

tndx <- c(700, 1000, 4000) # tempo discreto em que os potenciais de a��o acontecer�o
y[tndx] <- 1


g <- convolve(y, rev(df2$y), type = "open") # digite ?convolve para help

dygraph(data.frame(time= 1:length(g), g))

y <- rep(0,length(t))
y[1:length(df2$y)] <- df2$y

plot(df2$y)

qplot(x = 1:length(y), y= y)

cc <- ccf(g, y,lag.max = 10000) #Correla��o cruzada entre sinal e y para detectar eventos durante numa aquisi��o de sinal

df <- data.frame(time=cc$lag, acf = cc$acf)

cc$lag[which.max(cc$acf)] #�ndice do valor m�ximo da acf

dygraph(df) %>%
  dyLimit(0.43, color = "red")%>%
  dyEvent(cc$lag[which.max(cc$acf)] , label="max", labelLoc = "bottom") %>% dyRangeSelector()

cc$lag[which(cc$acf > 0.43),,]









