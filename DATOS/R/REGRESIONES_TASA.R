library(readr)
library(ggplot2)
library(corrplot)
library(MASS)

#Se lee el archivo csv
covid_df <- read_csv("C:/Users/Usuario/Desktop/UNIVERSIDAD/Investigacion/Datos/Csv/covid_table1008.csv")

##DFS alternativos
#df con semanas de inicio igual o menor a 24
df_sem24 = covid_df[covid_df$SEMANA_Entro <= 24,]
df_sem20 = covid_df[covid_df$SEMANA_Entro <= 20,] #menor a 20

####An?lisis exploratorio####
###Scatter Plot
##Tasa contagios v/s Semana entro
ggplot(covid_df, aes(x = tasa_max_contagios, y = SEMANA_Entro)) +
  geom_point(color = "blue",
             alpha = 0.5,
             size = 2)

#para df editado
ggplot(df_sem20, aes(x = tasa_max_contagios, y = SEMANA_Entro)) +
  geom_point(color = "blue",
             alpha = 0.5,
             size = 2)

##CORRPLOT
corr_sem20 = cor(df_sem20[,4:21])
corrplot(corr_sem20,method = "pie", type = "upper", tl.cex = 0.7, number.cex = 0.6)

####Regresiones####
##tasa m?xima
r_tasamax <- lm(tasa_max_contagios ~ SEMANA_Entro + P_HACINAMIENTO_C + var_salidas + P_POB, data = df_sem20)
summary(r_tasamax)

r2 = lm(tasa_max_contagios ~ var_salidas + P_HACINAMIENTO_C, data = df_sem20)
summary(r2)

r3 = lm(tasa_max_contagios ~ var_salidas, data = df_sem20)
summary(r3)

r4 = lm(tasa_max_contagios ~ var_salidas + P_HACINAMIENTO_C, data = df_sem20)
summary(r4)

r5 = lm(tasa_max_contagios ~ var_salidas + DENSIDAD + SEMANA_Entro, data = df_sem20)
summary(r5)

r6 = lm(sqrt(tasa_max_contagios) ~ var_salidas + DENSIDAD + SEMANA_Entro, data = df_sem20)
summary(r6)

r7 = lm(log(tasa_max_contagios) ~ var_salidas + P_POB + SEMANA_Entro +DENSIDAD , data = df_sem20)
summary(r7)

r8 = lm(tasa_max_contagios ~ var_salidas + P_POB + SEMANA_Entro +DENSIDAD, data = df_sem20)
summary(r8)

r9 = rlm(tasa_max_contagios ~ var_salidas + DENSIDAD + SEMANA_Entro, data = df_sem20)
summary(r9)

plot(r6)

##sem_max
r_tasa = lm(semana_max ~ var_salidas + DENSIDAD + SEMANA_Entro, data = df_sem20)
summary(r_tasa)




#NUEVAS

r_nueva = lm(log(tasa_max_contagios) ~ var_salidas, data = df_sem20)
summary(r_nueva)

r_nueva2 = lm(log(tasa_max_contagios) ~ var_salidas - 1, data = df_sem20)
summary(r_nueva2)


ggplot(df_sem20, aes(x = tasa_max_contagios, y = var_salidas)) +
  geom_point(color = "blue",
             alpha = 0.5,
             size = 2) +
  geom_smooth(method = 'lm')
