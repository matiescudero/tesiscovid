library(readr)
library(ggplot2)
library(plyr)
library(hrbrthemes)
library(viridis)
library(spdep)
library(maptools)
library(tidyverse)

#Se lee el archivo csv
final = read_csv("final.csv")

#Se abre el shp y se le une la información del csv
shp = readShapePoly("DATOS/SHP/COMUNAS_PAPER/COMUNAS_PAPER.shp")
shape = shp
shape@data = join(shape@data, final, by = "COD_COM")
W = poly2nb(shape)
W_list = nb2listw(W)

#Modelo no espacial
modelo1 <- lm(tasa_max ~ sem_cuar + p_hacina_c + IVS , data = shape@data)

#Modelo Espacial
sar_modelo1 = lagsarlm(formula =tasa_max ~ sem_cuar + p_hacina_c + IVS , data = shape@data, listw = W_list)

###MODELO SAR 1
###TASA ESTIMADA
##ßX
#Beta
B = unlist(as.numeric(sar_modelo1$coefficients)) #Cambiar según el modelo que se busque

#Matriz con valores de X, cambiarla según los escenarios
X = matrix(c(c(rep(1,38)), final$sem_cuar, final$p_hacina_c, final$IVS), ncol = 4)

#multiplicación entre matriz de X y vector de betas
XB = X %*% B  #vector con que contiene los y estimados sin agregar la componente espacial


##(I - pW)
I = diag(38) #Matriz identidad
rho = as.numeric(sar_modelo1$rho) #Cambiar según modelo
W_mat = nb2mat(W)
IPW = I - rho*W_mat
IPW_inv = solve(IPW)
IPWinv_XB =   IPW_inv %*% XB

##Cálculo tasa estimada según modelo
final$tasa_est = IPWinv_XB
final$error = abs((final$tasa_max - final$tasa_est)/final$tasa_est * 100)

#Columnas para semana de entrada
final$sem13 = 13
final$sem20 = 20

#Columnas para variación de salidas
final$mov80 = 80
final$mov40 = 40
final$mov20 = 20

####ESCENARIOS####
##SEMANA 13
Xsem13 = matrix(c(c(rep(1,38)), final$sem13, final$p_hacina_c, final$IVS), ncol = 4)

#multiplicación entre matriz de X y vector de betas
XBsem13 = Xsem13 %*% B  #vector con que contiene los y estimados sin agregar la componente espacial
tasa_sem13 =   IPW_inv %*% XBsem13
final$tasa_sem13 = tasa_sem13

##SEMANA 20
Xsem20 = matrix(c(c(rep(1,38)), final$sem20, final$p_hacina_c, final$IVS), ncol = 4)

#multiplicación entre matriz de X y vector de betas
XBsem20 = Xsem20 %*% B  #vector con que contiene los y estimados sin agregar la componente espacial
tasa_sem20 =   IPW_inv %*% XBsem20
final$tasa_sem20 = tasa_sem20

#Variación porcentual tasa
final$var_tasa_sem13 = ((final$tasa_sem13 - final$tasa_max)/final$tasa_max)*100
final$var_tasa_sem20 = ((final$tasa_sem20 - final$tasa_max)/final$tasa_max)*100

##MOV 80
Xmov80 = matrix(c(c(rep(1,38)), final$sem_cuar, final$p_hacina_c, final$mov80), ncol = 4)
#multiplicación entre matriz de X y vector de betas
XBmov80 = Xmov80 %*% B  #vector con que contiene los y estimados sin agregar la componente espacial
tasa_mov80 =   IPW_inv %*% XBmov80
final$tasa_mov80 = tasa_mov80

##MOV 40
Xmov40 = matrix(c(c(rep(1,38)), final$sem_cuar, final$p_hacina_c, final$mov40), ncol = 4)
#multiplicación entre matriz de X y vector de betas
XBmov40 = Xmov40 %*% B  #vector con que contiene los y estimados sin agregar la componente espacial
tasa_mov40 =   IPW_inv %*% XBmov40
final$tasa_mov40 = tasa_mov40

##MOV 20
Xmov20 = matrix(c(c(rep(1,38)), final$sem_cuar, final$p_hacina_c, final$mov20), ncol = 4)
#multiplicación entre matriz de X y vector de betas
XBmov20 = Xmov20 %*% B  #vector con que contiene los y estimados sin agregar la componente espacial
tasa_mov20 =   IPW_inv %*% XBmov20
final$tasa_mov20 = tasa_mov20

#Variación porcentual tasa
final$var_tasa_mov80 = ((final$tasa_mov80 - final$tasa_max)/final$tasa_max)*100
final$var_tasa_mov40 = ((final$tasa_mov40 - final$tasa_max)/final$tasa_max)*100
final$var_tasa_mov20 = ((final$tasa_mov20 - final$tasa_max)/final$tasa_max)*100

#Se exporta la tabla a csv
write.csv(final, 'DATOS/CSV/escenarios_moran.csv', row.names = FALSE)



####Gráficos####

final <- read_csv("DATOS/CSV/escenarios_moran.csv")

final$error_n = abs(final$tasa_max - final$tasa_max)
#Histograma error
hist(final$error, 
     col = "#69b3a2",
     main = "Histograma de Errores Porcentuales del Modelo",
     xlab = "Error Porcentual (%)",
     ylab = "Frecuencia")

abline(v = c(median(final$error), mean(final$error)),                     
       col = c("red","blue"),
       lty = c(3,3),
       lwd = c(3,3))

text(x = 30,  
     y = 13,
     paste("Mediana =", round(median(final$error), 4)),
     col = "red",
     cex = 1)

text(x = 30,  
     y = 11,
     paste("Promedio =", round(mean(final$error), 3)),
     col = "blue",
     cex = 1)


##grafico sem13

hist_tasasem13 <- ggplot(final, aes(x=x) ) +
  geom_histogram( aes(x = tasa_max, y = ..density..), fill="#69b3a2", bins = 10 ) +
  geom_label( aes(x=700, y=0.004, label="Tasa Máxima Real"), color="#69b3a2") +
  geom_label( aes(x=700, y=0.003, label="Promedio: 468,37"), color="#69b3a2") +
  geom_histogram( aes(x = tasa_sem13, y = -..density..), fill= "#404080", bins = 10) +
  geom_label( aes(x=700, y=-0.003, label="Tasa Máxima Estimada\n Semana 13"), color="#404080") +
  geom_label( aes(x=700, y=-0.0045, label="Promedio: 427,29"), color="#404080") +
  theme_ipsum() +
  xlab("Contagiados cada 100.000 habitantes") +
  ylab("Densidad")

hist_tasasem13

##grafico sem20

hist_tasasem20 <- ggplot(final, aes(x=x) ) +
  geom_histogram( aes(x = tasa_max, y = ..density..), fill="#69b3a2", bins = 10 ) +
  geom_label( aes(x=700, y=0.004, label="Tasa Máxima Real"), color="#69b3a2") +
  geom_label( aes(x=700, y=0.003, label="Promedio: 468,37"), color="#69b3a2") +
  geom_histogram( aes(x = tasa_sem20, y = -..density..), fill= "#404080", bins = 10) +
  geom_label( aes(x=700, y=-0.003, label="Tasa Máxima Estimada\n Semana 20"), color="#404080") +
  geom_label( aes(x=700, y=-0.0045, label="Promedio: 493,2"), color="#404080") +
  theme_ipsum() +
  xlab("Contagiados cada 100.000 habitantes") +
  ylab("Densidad")

hist_tasasem20

##grafico mov80

hist_tasamov80 <- ggplot(final, aes(x=x) ) +
  geom_histogram( aes(x = tasa_max, y = ..density..), fill="#69b3a2", bins = 10 ) +
  geom_label( aes(x=700, y=0.004, label="Tasa Máxima Real"), color="#69b3a2") +
  geom_label( aes(x=700, y=0.0025, label="Promedio: 468,37"), color="#69b3a2") +
  geom_histogram( aes(x = tasa_mov80, y = -..density..), fill= "#404080", bins = 20) +
  geom_label( aes(x=300, y=-0.003, label="Tasa Máxima Estimada\n IVS = 80%"), color="#404080") +
  geom_label( aes(x=300, y=-0.005, label="Promedio: 629,84"), color="#404080") +
  theme_ipsum() +
  xlab("Contagiados cada 100.000 habitantes") +
  ylab("Densidad")

hist_tasamov80

##grafico mov40

hist_tasamov40 <- ggplot(final, aes(x=x) ) +
  geom_histogram( aes(x = tasa_max, y = ..density..), fill="#69b3a2", bins = 8 ) +
  geom_label( aes(x=700, y=0.005, label="Tasa Máxima Real"), color="#69b3a2") +
  geom_label( aes(x=700, y=0.0035, label="Promedio: 468,37"), color="#69b3a2") +
  geom_histogram( aes(x = tasa_mov40, y = -..density..), fill= "#404080", bins = 20) +
  geom_label( aes(x=700, y=-0.003, label="Tasa Máxima Estimada\n IVS = 40%"), color="#404080") +
  geom_label( aes(x=700, y=-0.006, label="Promedio: 259,93"), color="#404080") +
  theme_ipsum() +
  xlab("Contagiados cada 100.000 habitantes") +
  ylab("Densidad")

hist_tasamov40


#GRAFICOS PARA DFS CON MENOR ERROR
df_error = final[final$error >= 26.7,]
df_bueno = final[final$error <= 14.5,]

ggplot(df_bueno, aes(x=sem_cuar, y = var_tasa_sem13)) + 
  geom_point(
    color="blue",
    fill="#69b3a2",
    shape= 21,
    alpha=0.6,
    size=5,
    stroke = 1
  ) + 
  xlab("Semana de Ingreso a Cuarentena") +
  ylab("Variación Tasa Máxima Estimada")
