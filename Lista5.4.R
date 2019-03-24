
# install.packages("openxlsx")
# install.packages("ggplot2")
# install.packages("edfReader")
# install.packages("plotly") #https://plot.ly/r/
# install.packages("gridExtra")
# install.packages("dygraphs")
# install.packages("xts")
# install.packages("tidyquant")
# install.packages("spgs")

# Uso de bibliotecas ------------------------------------------------------
library(openxlsx) #carrega a biblioteca "openxlsx"
library(ggplot2)
library(edfReader)
library(plotly) #https://plot.ly/r/
library(gridExtra)
library(dygraphs)
library(xts)
library(tidyquant)
library(spgs)

fs <- 500
dt <- 1/fs
t <- 10
N <- t/dt

tt <- seq(from=0, to=t, by=dt)
ruido <- rnorm(N+1)

df1 <- data.frame(time=tt, ruido)
dygraph(df1) %>% dyRangeSelector()

y <- sin(2*pi*tt*5)
dygraph(data.frame(time=tt,y))

turningpoint.test(ruido) 
turningpoint.test(y) 


y2 <- 20*sin(2*pi*tt*13) + ruido
dygraph(data.frame(time=tt,y2)) %>% dyRangeSelector()
turningpoint.test(y2) 

