library(maptools)
library(spatialreg)
library(plyr)
library(spdep)
shp=readShapePoly("DATOS/SHP/COMUNAS_PAPER/COMUNAS_PAPER.shp")
shape=shp
shape@data = join(shape@data,final, by = "COD_COM")
W = poly2nb(shape)
W_list = nb2listw(W)
# regresion a utilizar
modelo1<- lm(tasa_max ~ sem_cuar + p_hacina_c + IVS , data = final)
modelo2<- lm(tasa_max ~ sem_cuar + densidad + IVS , data = final)
modelo3 = lm(tasa_max ~ log(sem_cuar) + p_hacina_c + IVS , data = final)
modelo4<- lm(log(tasa_max) ~ sem_cuar + p_hacina_c + IVS , data = final)
### indice de moran 
indice_moran_r1<-lm.morantest(modelo1, W_list)
indice_moran_r2<-lm.morantest(modelo2, W_list)
indice_moran_r3<-lm.morantest(modelo3, W_list)
indice_moran_r4<-lm.morantest(modelo4, W_list)
