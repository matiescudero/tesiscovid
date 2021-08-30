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

hist_tasasem13 <- ggplot(df_final, aes(x=x) ) +
  geom_histogram( aes(x = tasa_max, y = ..density..), fill="#69b3a2", bins = 10 ) +
  geom_label( aes(x=700, y=0.004, label="Máxima Tasa Real"), color="#69b3a2") +
  geom_label( aes(x=700, y=0.003, label="Promedio: 468,37"), color="#69b3a2") +
  geom_histogram( aes(x = tasa_sem13, y = -..density..), fill= "#404080", bins = 10) +
  geom_label( aes(x=700, y=-0.003, label="Máxima Tasa Semana 13"), color="#404080") +
  geom_label( aes(x=700, y=-0.004, label="Promedio: 435,73"), color="#404080") +
  theme_ipsum() +
  xlab("Contagiados cada 100.000 habitantes") +
  ylab("Densidad")

hist_tasasem13



