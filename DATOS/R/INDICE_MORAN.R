library(maptools)
library(spatialreg)
library(plyr)
library(spdep)
shp=readShapePoly("COMUNAS_PAPER.shp")
shape=shp
shape@data = join(shape@data,df_sem20, by = "COD_COM")
W = poly2nb(shape)
W_list = nb2listw(W)
# regresion a utilizar
r1<- lm(log(tasa_max_contagios) ~ SEMANA_Entro + DENSIDAD + var_salidas , data = df_sem20)
r2<- lm(log(tasa_max_contagios) ~ SEMANA_Entro + P_HACINAMIENTO_C + var_salidas , data = df_sem20)
### indice de moran 
indice_moran_r1<-lm.morantest(r1, W_list)
indice_moran_r2<-lm.morantest(r2, W_list)
