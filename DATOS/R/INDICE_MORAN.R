library(maptools)
library(readr)
library(plyr)
library(spdep)

final = read_csv("final.csv")

shp = readShapePoly("DATOS/SHP/COMUNAS_PAPER/COMUNAS_PAPER.shp")
shape = shp
shape@data = join(shape@data, final, by = "COD_COM")
W = poly2nb(shape)
W_list = nb2listw(W)

# regresion a utilizar
modelo1 <- lm(log(tasa_max) ~ sem_cuar + p_hacina_c + IVS , data = shape@data)
modelo2 <- lm(tasa_max ~ sem_cuar + densidad + IVS , data = shape@data)
modelo3 = lm(tasa_max ~ log(sem_cuar) + p_hacina_c + IVS , data = shape@data)

### indice de moran 
indice_moran_r1 <- lm.morantest(modelo1, W_list)
indice_moran_r3 <- lm.morantest(modelo2, W_list)
indice_moran_r4 <- lm.morantest(modelo3, W_list)

#Regresiones SAR
sar_modelo1 = lagsarlm(formula =log(tasa_max) ~ sem_cuar + p_hacina_c + IVS , data = shape@data, listw = W_list)
sar_modelo2 = lagsarlm(formula = tasa_max ~ sem_cuar + densidad + IVS , data = shape@data, listw = W_list)
sar_modelo3 = lagsarlm(formula = tasa_max ~ log(sem_cuar) + p_hacina_c + IVS , data = shape@data, listw = W_list)
sar_modelo4 = lagsarlm(formula = log(tasa_max) ~ sem_cuar + densidad + IVS , data = shape@data, listw = W_list)


