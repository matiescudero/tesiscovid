library(readr)
library(ggplot2)
library(plyr)
library(hrbrthemes)
library(viridis)
library(spdep)

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
final$error = (final$tasa_max - final$tasa_est)/final$tasa_est * 100

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

