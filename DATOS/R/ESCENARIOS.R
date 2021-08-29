#se lee el csv
library(readr)
df_final = read_csv("final.csv")

#Se corre la regresión definitiva
modelo = lm(tasa_max ~ sem_cuar + p_hacina_c + IVS , data = df_final)
summary(modelo)

#tasa_max estimada
df_final$tasa_est = -220.781 + 7.130*df_final$sem_cuar + 36.882*df_final$p_hacina_c + 8.368*df_final$IVS

#Columnas para semana de entrada
df_final$sem13 = 13

#tasa_max si todas las comunas hubiesen ingresado a cuarentena en la semana 13
