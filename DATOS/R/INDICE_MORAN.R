library(maptools)
library(readr)
library(plyr)
library(spdep)
<<<<<<< HEAD
shp=readShapePoly("DATOS/SHP/COMUNAS_PAPER/COMUNAS_PAPER.shp")
shape=shp
shape@data = join(shape@data,final, by = "COD_COM")
=======

final = read_csv("final.csv")

shp = readShapePoly("DATOS/SHP/COMUNAS_PAPER/COMUNAS_PAPER.shp")
shape = shp
shape@data = join(shape@data, final, by = "COD_COM")
>>>>>>> 93c2f8e5fdbb6bf0fa6a20245b24aeb926da134e
W = poly2nb(shape)
W_list = nb2listw(W)

# regresion a utilizar


modelo1 <- lm(log(tasa_max) ~ sem_cuar + p_hacina_c + IVS , data = shape@data)
modelo2 <- lm(tasa_max ~ sem_cuar + densidad + IVS , data = shape@data)
modelo3 = lm(tasa_max ~ log(sem_cuar) + p_hacina_c + IVS , data = shape@data)

### indice de moran 
indice_moran_r1 <- lm.morantest(modelo1, W_list)
indice_moran_r2<- lm.morantest(modelo2, W_list)
indice_moran_r3 <- lm.morantest(modelo3, W_list)

#Regresiones SAR
sar_modelo1 = lagsarlm(formula =log(tasa_max) ~ sem_cuar + p_hacina_c + IVS , data = shape@data, listw = W_list)
sar_modelo2 = lagsarlm(formula = tasa_max ~ sem_cuar + densidad + IVS , data = shape@data, listw = W_list)
sar_modelo3 = lagsarlm(formula = tasa_max ~ log(sem_cuar) + p_hacina_c + IVS , data = shape@data, listw = W_list)

sar_modelo4 = lagsarlm(formula = log(tasa_max) ~ sem_cuar + densidad + IVS , data = shape@data, listw = W_list)



<<<<<<< HEAD
=======
####PRUEBA CON MATRICES####

###MODELO SAR 1
###TASA ESTIMADA
##ßX
#Beta
B = unlist(as.numeric(sar_modelo1$coefficients)) #Cambiar según el modelo que se busque

#Matriz con valores de X, cambiarla según los escenarios
X = matrix(c(c(rep(1,38)), final$sem_cuar, final$p_hacina, final$IVS), ncol = 4)

#multiplicación entre matriz de X y vector de betas
XB = X %*% B  #vector con que contiene los y estimados sin agregar la componente espacial


##(I - pW)
I = diag(38)

rho = as.numeric(sar_modelo1$rho) #Cambiar según modelo

W_mat = nb2mat(W)

IPW = I - rho*W_mat

IPW_inv = solve(IPW)

IPWinv_XB =   IPW_inv %*% exp(XB)

##ESCENARIOS

final$tasa_est = IPWinv_XB

>>>>>>> 6c4fd37ea88e7c453407ee0935280baab62d4350
