#https://www.r-bloggers.com/flexdashboard-easy-interactive-dashboards-for-r/

library(openxlsx)
library(dygraphs)
library(gridExtra)
library(grid)
library(htmltools)
library(ggplot2)
source("TREMSENToolbox.r")


                  ## VISUALIZAÇÃO DE DADOS ##

# Função para plotar dados
plotMultiPanelData <- function(df1)
{
  colnames(df1)[1] <- "time"
  
browsable(
  tagList(list(
    tags$div(
      style = 'width:33%;display:block;float:left;',
      dygraph(data.frame(time=df1$time, G1x=df1$X.G1.X., G2x=df1$X.G2.X.), group = "ensync", height = 200, width = "100%") %>%
        dyLegend(show="always")%>%dyOptions(colors = c("rgb(255,0,0)", "rgb(155,0,0)"), colorSaturation=c(0.5, 0.1)),
      dygraph(data.frame(time=df1$time, G1y=df1$X.G1.Y., G2y=df1$X.G2.Y.), group = "ensync", height = 200, width = "100%") %>%
        dyLegend(show="always")%>%dyOptions(colors = c("rgb(0,255,0)", "rgb(0,155,0)")),
      dygraph(data.frame(time=df1$time, G1z=df1$X.G1.Z., G2z=df1$X.G2.Z.), group = "ensync", height = 200, width = "100%") %>%
        dyLegend(show="always")%>%dyOptions(colors = c("rgb(0,0,255)", "rgb(0,0,155)"))%>%dyRangeSelector()
    ),
    tags$div(
      style = 'width:33%;display:block;float:left;',
      dygraph(data.frame(time=df1$time, A1x=df1$X.A1.X., A2x=df1$X.A2.X.), group = "ensync", height = 200, width = "100%") %>%
        dyLegend(show="always")%>%dyOptions(colors = c("rgb(255,0,0)", "rgb(155,0,0)")),
      dygraph(data.frame(time=df1$time, A1y=df1$X.A1.Y., A2y=df1$X.A2.Y.), group = "ensync", height = 200, width = "100%") %>%
        dyLegend(show="always")%>%dyOptions(colors =  c("rgb(0,255,0)", "rgb(0,155,0)")),
      dygraph(data.frame(time=df1$time, A1z=df1$X.A1.Z., A2z=df1$X.A2.Z.), group = "ensync", height = 200, width = "100%") %>%
        dyLegend(show="always")%>%dyOptions(colors = c("rgb(0,0,255)", "rgb(0,0,155)"))%>%dyRangeSelector()
    ),
    tags$div(
      style = 'width:33%;display:block;float:left;',
      dygraph(data.frame(time=df1$time, M1x=df1$X.M1.X., M2x=df1$X.M2.X.), group = "ensync", height = 200, width = "100%") %>%
        dyLegend(show="always")%>%dyOptions(colors = c("rgb(255,0,0)", "rgb(155,0,0)")),
      dygraph(data.frame(time=df1$time, M1y=df1$X.M1.Y., M2y=df1$X.M2.Y.), group = "ensync", height = 200, width = "100%") %>%
        dyLegend(show="always")%>%dyOptions(colors =  c("rgb(0,255,0)", "rgb(0,155,0)")),
      dygraph(data.frame(time=df1$time, M1z=df1$X.M1.Z., M2z=df1$X.M2.Z.), group = "ensync", height = 200, width = "100%") %>%
        dyLegend(show="always")%>%dyOptions(colors = c("rgb(0,0,255)", "rgb(0,0,155)"))%>%dyRangeSelector()
    )
  )
  ))
}


strpath <- "C:/Users/samsung/Documents/Universidade/PSB/Prova1/"
Filename <- paste(strpath, "G2 P-S.txt", sep = "")

#carregando arquivo
df <- LoadTREMSENFile(Filename)
plotMultiPanelData(df)

#removendo tendências lineares e não lineares dos dados
df3 <- nonLineardetrendTremsenData(df)
plotMultiPanelData(df3)


######################################   PROTOCOLO 2 ###############################################################

######### PARA G1.Y    ##########



                    ## PROCESSAMENTO DE DADOS ##

##  Função para janelar o dataframe df3  ##

janelamento <- function(a,b){

y <- a 

nwd <- b

indx1 <- seq(from=1, to=length(y), by=nwd) 

N <- length(indx1) 

td <- rep(NA,times=N-1) 

djanela <- data.frame(time=rep(NA,times=N-1)) 
djanela$RMS <- NA  

for(i in 1:(N-1)) {
  #criando um vetor tempo centralizado em cada janela
  djanela$time[i] <- (indx1[i] + (indx1[i+1]-indx1[i])/2) * 0.02 #resolução temporal(dt) = 0.02s
                                                                 
  #calculando o valor RMS para cada janela 
  djanela$RMS[i]<- rms(y[indx1[i]:indx1[i+1]])
}
return(djanela)
}

djanela <- janelamento(df3$X.G1.Y.,40)




##  Função para plotar janelamento  ##

plotar_janela <- function(c,d){
  
  d1 <- dygraph(c, main = d) %>% dyRangeSelector() %>%
    dyShading(from = "0", to = "4.0", color = "#e9e9e9") %>%
    dyShading(from = "9.0", to = "12.0", color = "#e9e9e9") %>%
    dyShading(from = "17", to = "20", color = "#e9e9e9") %>%
    dyShading(from = "25", to = "29", color = "#e9e9e9") %>%
    dyShading(from = "34", to = "38", color = "#e9e9e9") %>%
    dyShading(from = "43", to = "48", color = "#e9e9e9") %>%
    dyShading(from = "52", to = "57", color = "#e9e9e9") %>%
    dyShading(from = "62", to = "67", color = "#e9e9e9") %>%
    dyShading(from = "70", to = "75", color = "#e9e9e9") %>%
    dyShading(from = "80", to = "84", color = "#e9e9e9") %>%
    dyShading(from = "89", to = "94", color = "#e9e9e9") %>%
    dyShading(from = "98", to = "102", color = "#e9e9e9") %>%
        #Ruído
    dyShading(from = "4.0", to = "9.0", color = "#FFE6E6") %>%
    dyShading(from = "12.0", to = "17.0", color = "#FFE6E6") %>%
    dyShading(from = "20.0", to = "25.0", color = "#FFE6E6") %>%
    dyShading(from = "29.0", to = "34.0", color = "#FFE6E6") %>%
    dyShading(from = "38.0", to = "43.0", color = "#FFE6E6") %>%
    dyShading(from = "48.0", to = "52.0", color = "#FFE6E6") %>%
    dyShading(from = "57.0", to = "62.0", color = "#FFE6E6") %>%
    dyShading(from = "67.0", to = "70.0", color = "#FFE6E6") %>%
    dyShading(from = "75.0", to = "80.0", color = "#FFE6E6") %>%
    dyShading(from = "84.0", to = "89.0", color = "#FFE6E6") %>%
    dyShading(from = "94.0", to = "98.0", color = "#FFE6E6") 
 
  d2 <- dygraph(df3[c("X.Time.", d)], main = d)%>% dyRangeSelector()
  ds <- list(d1,d2)
  
  
  return(ds)
}

htmltools::browsable(htmltools::tagList(plotar_janela(djanela,"X.G1.Y.")))




## Função para parâmetros estatísticos do sinal G1.Y  ##

param_est_sinal <- function(dj){

A = matrix(
  
  c(1, 0, 4.0,
    2, 9.0, 12.0,
    3, 17.0, 21.0,
    4, 27.0, 29.0,
    5, 34.0, 39.0,
    6, 43.0, 47.0,
    7, 52.0, 57.0,
    8, 62.0, 66.0,
    9, 70.0, 75.0,
  10, 79.0, 84.0),
  nrow=10,
  ncol=3,
  byrow = TRUE)


colnames(A) = c("Sinal.G1.Y", "Inicio", "Fim")


dfr <- data.frame(time= A[,1])
dfr$RMS <- NA
dfr$variancia <- NA
dfr$DI <- NA
dfr$CV <- NA
dfr$prim_quartil <- NA
dfr$seg_quartil <- NA
dfr$terc_quartil <- NA

for(i in 1:nrow(A)) {
  dfr$RMS[i]<- rms(dj$RMS[c(A[i,2]:A[i,3])]) #Valor RMS
  dfr$variancia[i] <- var(dj$RMS[c(A[i,2]:A[i,3])]) #Variancia
  
  dfr$prim_quartil[i] <- quantile(dj$RMS[c(A[i,2]:A[i,3])], c(.25)) #primeiro quartil
  dfr$seg_quartil[i] <- quantile(dj$RMS[c(A[i,2]:A[i,3])], c(.50)) #segundo quartil
  dfr$terc_quartil[i] <- quantile(dj$RMS[c(A[i,2]:A[i,3])], c(.75)) #terceiro quartil
  
  dfr$DI[i] <- IQR(dj$RMS[c(A[i,2]:A[i,3])]) #distancia interquartil
  dfr$CV[i] <- 100*(sd(dj$RMS[c(A[i,2]:A[i,3])])/mean(dj$RMS[c(A[i,2]:A[i,3])]))
}
return(dfr)
}

dp2g1_y <- param_est_sinal(djanela)




## Função para parâmetros estatísticos do ruido G1.Y  ##

param_est_ruido <- function(djr){

B = matrix(
  
  c(1, 4.0, 9.0,
    2, 12.0, 17.0,
    3, 21.0, 27.0,
    4, 29.0, 34.0,
    5, 39.0, 43.0,
    6, 47.0, 52.0,
    7, 57.0, 62.0,
    8, 66.0, 70.0,
    9, 75.0, 79.0,
    10, 84.0, 89.0),
  nrow=10,
  ncol=3,
  byrow = TRUE)


colnames(B) = c("Ruido.G1.Y", "Inicio", "Fim")


dfr1 <- data.frame(time= B[,1])
dfr1$RMS <- NA
dfr1$variancia <- NA
dfr1$DI <- NA
dfr1$CV <- NA
dfr1$prim_quartil <- NA
dfr1$seg_quartil <- NA
dfr1$terc_quartil <- NA

for(i in 1:nrow(B)) {
  dfr1$RMS[i]<- rms(djr$RMS[c(B[i,2]:B[i,3])]) #Valor RMS
  dfr1$variancia[i] <- var(djr$RMS[c(B[i,2]:B[i,3])]) #Variancia
  
  dfr1$prim_quartil[i] <- quantile(djr$RMS[c(B[i,2]:B[i,3])], c(.25)) #primeiro quartil
  dfr1$seg_quartil[i] <- quantile(djr$RMS[c(B[i,2]:B[i,3])], c(.50)) #segundo quartil
  dfr1$terc_quartil[i] <- quantile(djr$RMS[c(B[i,2]:B[i,3])], c(.75)) #terceiro quartil
  
  dfr1$DI[i] <- IQR(djr$RMS[c(B[i,2]:B[i,3])]) #distancia interquartil
  dfr1$CV[i] <- 100*(sd(djr$RMS[c(B[i,2]:B[i,3])])/mean(djr$RMS[c(B[i,2]:B[i,3])]))
}
return(dfr1)
}

dp2g1R_y <- param_est_ruido(djanela)



## Função par relação sinal/ ruído ##

SNR <- function(k,l){
  
  x <- mean(k$RMS) #média do sinal rms
  
  y <- mean(l$RMS) #média do ruído rms
  
  z <- 20*log10(x/y)
  
  return(z)
}

sinal_ruido_p2g1y <- SNR(dp2g1_y,dp2g1R_y)



## Função para tabela final do protocolo 2: sinal e ruido de G1.Y  ##

tabela_final <- function(d,d1){
  
# Dados estatísticos do sinal
#SINAL
dfp2 <- data.frame(time= 1)
dfp2$LF_rms <- median(d$RMS) - quantile(d$RMS, c(.25))
dfp2$M_rms <- median(d$RMS)
dfp2$LI_rms <- median(d$RMS) + quantile(d$RMS, c(.75))
dfp2$LF_var <- median(d$variancia) - quantile(d$RMS, c(.25))
dfp2$M_var <- median(d$variancia)
dfp2$LI_var <- median(d$variancia) + quantile(d$variancia, c(.75))
dfp2$LF_DI <- median(d$DI) - quantile(d$DI, c(.25))
dfp2$M_DI <- median(d$DI)
dfp2$LI_DI <- median(d$DI) + quantile(d$DI, c(.75))
dfp2$LF_CV <- median(d$CV) - quantile(d$CV, c(.25))
dfp2$M_CV <- median(d$CV)
dfp2$LI_CV <- median(d$CV) + quantile(d$CV, c(.75))

#RUIDO
dfp2$LF_rms_Ruido <- median(d1$RMS) - quantile(d1$RMS, c(.25))
dfp2$M_rms_Ruido <- median(d1$RMS)
dfp2$LI_rms_Ruido <- median(d1$RMS) + quantile(d1$RMS, c(.75))
dfp2$LF_var_Ruido <- median(d1$variancia) - quantile(d1$RMS, c(.25))
dfp2$M_var_Ruido <- median(d1$variancia)
dfp2$LI_var_Ruido <- median(d1$variancia) + quantile(d1$variancia, c(.75))
dfp2$LF_DI_Ruido <- median(d1$DI) - quantile(d1$DI, c(.25))
dfp2$M_DI_Ruido <- median(d1$DI)
dfp2$LI_DI_Ruido <- median(d1$DI) + quantile(d1$DI, c(.75))
dfp2$LF_CV_Ruido <- median(d1$CV) - quantile(d1$CV, c(.25))
dfp2$M_CV_Ruido <- median(d1$CV)
dfp2$LI_CV_Ruido <- median(d1$CV) + quantile(d1$CV, c(.75))

return(dfp2)
}

dfp2_g1y <- tabela_final(dp2g1_y,dp2g1R_y)

write.xlsx(dfp2_g1y, "tabelafinal_p2g1y.xlsx", colNames = TRUE)




######### PARA G1.X    ##########

djp2_G1.X <- janelamento(df3$X.G1.X.,40)

htmltools::browsable(htmltools::tagList(plotar_janela(djp2_G1.X,"X.G1.X.")))

dp2g1_X <- param_est_sinal(djp2_G1.X)

dp2g1R_x <- param_est_ruido(djp2_G1.X)

dfp2_g1x <- tabela_final(dp2g1_X,dp2g1R_x)

write.xlsx(dfp2_g1x, "tabelafinal_g1x.xlsx", colNames = TRUE)

## Relação sinal/ruído 

sinal_ruido_p2g1x <- SNR(dp2g1_X,dp2g1R_x)




######### PARA G1.Z    ##########

djp2_G1.Z <- janelamento(df3$X.G1.Z.,40)

htmltools::browsable(htmltools::tagList(plotar_janela(djp2_G1.Z,"X.G1.Z.")))

dp2g1_Z <- param_est_sinal(djp2_G1.Z)

dp2g1R_Z <- param_est_ruido(djp2_G1.Z)

dfp2_g1z <- tabela_final(dp2g1_X,dp2g1R_x)

write.xlsx(dfp2_g1z, "tabelafinal_g1z.xlsx", colNames = TRUE)

## Relação sinal/ruído 

sinal_ruido_p2g1z <- SNR(dp2g1_Z,dp2g1R_Z)





######### PARA A1.X    ##########

djp2_A1.X <- janelamento(df3$X.A1.X.,40)

htmltools::browsable(htmltools::tagList(plotar_janela(djp2_A1.X,"X.A1.X.")))

dp2a1_X <- param_est_sinal(djp2_A1.X)

dp2a1R_x <- param_est_ruido(djp2_A1.X)

dfp2_a1x <- tabela_final(dp2a1_X,dp2a1R_x)

write.xlsx(dfp2_a1x, "tabelafinal_a1x.xlsx", colNames = TRUE)

## Relação sinal/ruído 

sinal_ruido_p2a1x <- SNR(dp2a1_X,dp2a1R_x)


######### PARA A1.Y    ##########

djp2_A1.Y <- janelamento(df3$X.A1.Y.,40)

htmltools::browsable(htmltools::tagList(plotar_janela(djp2_A1.Y,"X.A1.Y.")))

dp2a1_Y <- param_est_sinal(djp2_A1.Y)

dp2a1R_Y <- param_est_ruido(djp2_A1.Y)

dfp2_a1y <- tabela_final(dp2a1_Y,dp2a1R_Y)

write.xlsx(dfp2_A1y, "tabelafinal_a1y.xlsx", colNames = TRUE)

## Relação sinal/ruído 

sinal_ruido_p2a1y <- SNR(dp2a1_Y,dp2a1R_Y)


######### PARA A1.Z    ##########

djp2_A1.Z <- janelamento(df3$X.A1.Z.,40)

htmltools::browsable(htmltools::tagList(plotar_janela(djp2_A1.Z,"X.A1.Z.")))

dp2a1_Z <- param_est_sinal(djp2_A1.Z)

dp2a1R_Z <- param_est_ruido(djp2_A1.Z)

dfp2_a1z <- tabela_final(dp2a1_X,dp2a1R_x)

write.xlsx(dfp2_a1z, "tabelafinal_a1z.xlsx", colNames = TRUE)

## Relação sinal/ruído 

sinal_ruido_p2a1z <- SNR(dp2a1_Z,dp2a1R_Z)





######### PARA M1.X    ##########

djp2_M1.X <- janelamento(df3$X.M1.X.,40)

htmltools::browsable(htmltools::tagList(plotar_janela(djp2_M1.X,"X.M1.X.")))

dp2m1_X <- param_est_sinal(djp2_M1.X)

dp2m1R_x <- param_est_ruido(djp2_M1.X)

dfp2_m1x <- tabela_final(dp2m1_X,dp2m1R_x)

write.xlsx(dfp2_m1x, "tabelafinal_m1x.xlsx", colNames = TRUE)

## Relação sinal/ruído 

sinal_ruido_p2m1x <- SNR(dp2m1_X,dp2m1R_x)


######### PARA M1.Y    ##########

djp2_M1.Y <- janelamento(df3$X.M1.Y.,40)

htmltools::browsable(htmltools::tagList(plotar_janela(djp2_M1.Y,"X.M1.Y.")))

dp2m1_Y <- param_est_sinal(djp2_M1.Y)

dp2m1R_Y <- param_est_ruido(djp2_M1.Y)

dfp2_m1y <- tabela_final(dp2m1_Y,dp2m1R_Y)

write.xlsx(dfp2_m1y, "tabelafinal_m1y.xlsx", colNames = TRUE)

## Relação sinal/ruído 

sinal_ruido_p2m1y <- SNR(dp2m1_Y,dp2m1R_Y)


######### PARA M1.Z    ##########

djp2_M1.Z <- janelamento(df3$X.M1.Z.,40)

htmltools::browsable(htmltools::tagList(plotar_janela(djp2_M1.Z,"X.M1.Z.")))

dp2m1_Z <- param_est_sinal(djp2_M1.Z)

dp2m1R_Z <- param_est_ruido(djp2_M1.Z)

dfp2_m1z <- tabela_final(dp2m1_X,dp2m1R_x)

write.xlsx(dfp2_m1z, "tabelafinal_m1z.xlsx", colNames = TRUE)

## Relação sinal/ruído 

sinal_ruido_p2m1z <- SNR(dp2m1_Z,dp2m1R_Z)



## Plotar gráfico para analisar a reprodutibilidade ##

qplot(dp2g1_X$time, dp2g1_X$variancia, geom=c("point", "line"))  #Gráfico da variância do sinal P2_G1.X

qplot(dp2g1R_x$time, dp2g1R_x$variancia, geom=c("point", "line"))  # Gráfico da variância do ruído P2_G1.X

qplot(dp2g1_y$time, dp2g1_y$variancia, geom=c("point", "line"))  #Gráfico da variância do sinal P2_G1.Y

qplot(dp2g1_y$time, dp2g1_y$variancia, geom=c("point", "line"))  # Gráfico da variância do ruído P2_G1.Y

qplot(dp2g1_Z$time, dp2g1_Z$variancia, geom=c("point", "line"))  #Gráfico da variância do sinal P2_G1.Z

qplot(dp2g1R_Z$time, dp2g1R_Z$variancia, geom=c("point", "line"))  # Gráfico da variância do ruído P2_G1.Z


qplot(dp2a1_X$time, dp2a1_X$variancia, geom=c("point", "line"))  #Gráfico da variância do sinal P2_A1.X

qplot(dp2a1R_x$time, dp2a1R_x$variancia, geom=c("point", "line"))  # Gráfico da variância do ruído P2_A1.X

qplot(dp2g1_Y$time, dp2a1_Y$variancia, geom=c("point", "line"))  #Gráfico da variância do sinal P2_A1.Y

qplot(dp2a1R_Y$time, dp2a1R_Y$variancia, geom=c("point", "line"))  # Gráfico da variância do ruído P2_A1.Y

qplot(dp2a1_Z$time, dp2a1_Z$variancia, geom=c("point", "line"))  #Gráfico da variância do sinal P2_A1.Z

qplot(dp2a1R_Z$time, dp2a1R_Z$variancia, geom=c("point", "line"))  # Gráfico da variância do ruído P2_A1.Z


qplot(dp2m1_X$time, dp2m1_X$variancia, geom=c("point", "line"))  #Gráfico da variância do sinal P2_M1.X

qplot(dp2m1R_x$time, dp2m1R_x$variancia, geom=c("point", "line"))  # Gráfico da variância do ruído P2_M1.X

qplot(dp2m1_Y$time, dp2m1_Y$variancia, geom=c("point", "line"))  #Gráfico da variância do sinal P2_M1.Y

qplot(dp2m1R_Y$time, dp2m1R_Y$variancia, geom=c("point", "line"))  # Gráfico da variância do ruído P2_M1.Y

qplot(dp2m1_Z$time, dp2m1_Z$variancia, geom=c("point", "line"))  #Gráfico da variância do sinal P2_M1.Z

qplot(dp2m1R_Z$time, dp2m1R_Z$variancia, geom=c("point", "line"))  # Gráfico da variância do ruído P2_M1.Z


