library(readr)
library(ggplot2)
library(corrplot)
library(MASS)

#Se lee el archivo csv
covid_df <- read_csv("DATOS/CSV/covid_table1008.csv")

##DFS alternativos
#df con semanas de inicio igual o menor a 24
df_sem24 = covid_df[covid_df$SEMANA_Entro <= 24,]
df_sem20 = covid_df[covid_df$sem_cuar <= 20,] #menor a 20

#Se exporta el df como csv
write.csv(df_sem20,"DATOS/CSV/df_sem20.csv", row.names=FALSE)


####An?lisis exploratorio####
###Scatter Plot
##Tasa contagios v/s Semana entro
ggplot(covid_df, aes(x = SEMANA_Entro, y = tasa_max_contagios)) +
  geom_point(color = "blue",
             alpha = 0.5,
             size = 2) +
  xlab("Semana de Ingreso a Cuarentena") +
  ylab("Máxima Tasa de Contagios")


ggplot(df_sem20, aes(x = SEMANA_Entro, y = tasa_max_contagios)) +
  geom_point(color = "blue",
             alpha = 0.5,
             size = 2) +
  xlab("Semana de Ingreso a Cuarentena") +
  ylab("Máxima Tasa de Contagios")

#para df editado
ggplot(df_sem20, aes(x = var_salidas, y = tasa_max_contagios)) +
  geom_point(color = "blue",
             alpha = 0.5,
             size = 2) +
  geom_smooth(method = "lm")


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

#######regresiones definitivas 


r1<- lm(tasa_max_contagios~ SEMANA_Entro + P_HACINAMIENTO_C+ var_salidas , data = df_sem20)
summary(r1)
r2<- lm(log(tasa_max_contagios) ~ SEMANA_Entro + P_HACINAMIENTO_C + var_salidas , data = df_sem20)
summary(r2)

r3<- lm(tasa_max_contagios~ SEMANA_Entro + DENSIDAD + var_salidas , data = df_sem20)
summary(r3)
r4<- lm(log(tasa_max_contagios) ~ SEMANA_Entro + DENSIDAD + var_salidas , data = df_sem20)
summary(r4)






#Prueba 

r3 = lm(tasa_max_contagios ~ var_salidas, data = df_sem20)
summary(r3)

plot(r3)

#Histogramas de residuos 

qplot(r1$residuals,
      geom="histogram",
      bins=8,
      xlab="Residuos", 
      ylab="Frecuencia",
      fill=I("skyblue3"), 
      col=I("black"))

qplot(r2$residuals,
      geom="histogram",
      bins=8,
      xlab="Residuos", 
      ylab="Frecuencia",
      fill=I("skyblue3"), 
      col=I("black"))
qplot(r3$residuals,
      geom="histogram",
     
      bins=8,
      xlab="Residuos", 
      ylab="Frecuencia",
      fill=I("skyblue3"), 
      col=I("black"))

qplot(r4$residuals,
      geom="histogram",
      
      bins=8,
      xlab="Residuos", 
      ylab="Frecuencia",
      fill=I("skyblue3"), 
      col=I("black"))


