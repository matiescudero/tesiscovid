library(maptools)
library(spatialreg)
library(plyr)
library(spdep)
shp=readShapePoly("DATOS/SHP/COMUNAS_PAPER/COMUNAS_PAPER.shp")
shape=shp
shape@data = join(shape@data,df_sem20, by = "COD_COM")
W = poly2nb(shape)
W_list = nb2listw(W)
# regresion a utilizar
r1<- lm(tasa_max ~ sem_cuar + p_hacina_c + IVS , data = final)

r3<- lm(tasa_max ~ sem_cuar + densidad + IVS , data = final)

r4 = lm(tasa_max ~ log(sem_cuar) + p_hacina_c + IVS , data = final)

### indice de moran 
indice_moran_r1<-lm.morantest(r1, W_list)

indice_moran_r3<-lm.morantest(r3, W_list)
indice_moran_r4<-lm.morantest(r4, W_list)

