library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(hrbrthemes)


#se lee el csv
df_final = read_csv("final.csv")

#Se corre la regresión definitiva
modelo = lm(tasa_max ~ sem_cuar + p_hacina_c + IVS , data = df_final)
summary(modelo)

#tasa_max estimada
df_final$tasa_est = -220.781 + 7.130*df_final$sem_cuar + 36.882*df_final$p_hacina_c + 8.368*df_final$IVS

#Se calcula el error asociado a cada registro
df_final$error = abs((df_final$tasa_max - df_final$tasa_est)/df_final$tasa_est) * 100

#Columnas para semana de entrada
df_final$sem13 = 13
df_final$sem20 = 20

#Columnas para variación de salidas
df_final$mov80 = 80
df_final$mov40 = 40
df_final$mov20 = 20


##SEMANA DE ENTRADA
#tasa_max si todas las comunas hubiesen ingresado a cuarentena en la semana 13
df_final$tasa_sem13 = -220.781 + 7.130*df_final$sem13 + 36.882*df_final$p_hacina_c + 8.368*df_final$IVS

#tasa_max si todas las comunas hubiesen ingresado a cuarentena en la semana 13
df_final$tasa_sem20 = -220.781 + 7.130*df_final$sem20 + 36.882*df_final$p_hacina_c + 8.368*df_final$IVS

##MOVILIDAD
df_final$tasa_mov80 = -220.781 + 7.130*df_final$sem_cuar + 36.882*df_final$p_hacina_c + 8.368*df_final$mov80
df_final$tasa_mov40 = -220.781 + 7.130*df_final$sem_cuar + 36.882*df_final$p_hacina_c + 8.368*df_final$mov40
df_final$tasa_mov20 = -220.781 + 7.130*df_final$sem_cuar + 36.882*df_final$p_hacina_c + 8.368*df_final$mov20




####GRAFICOS####
##grafico sem13
hist(df_final$tasa_max, breaks=10, xlim=c(0,900), col=rgb(1,0,0,0.5), xlab="height", 
     ylab="nbr of plants", main="distribution of height of 2 durum wheat varieties" )

# Second with add=T to plot on top
hist(df_final$tasa_mov40, breaks=6, xlim=c(0,900), col=rgb(0,0,1,0.5), add=T)
