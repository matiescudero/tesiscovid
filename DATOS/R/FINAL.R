library(reshape)
library(readr)
library(corrplot)
library(MASS)
library(psych)


#Gráficos para todas las comunas
covid_df <- read_csv("DATOS/CSV/covid_table1008.csv")

#Se eliminan las columnas que no se usarán
covid_df = covid_df[,-c(4,5,9:12,14,16,17,19,20)]


covid_df = rename(covid_df, c(tasa_max_contagios="tasa_max", DENSIDAD="densidad",P_POB="p_pob", SEMANA_Entro="sem_cuar",P_HACINAMIENTO="p_hacina",P_HACINAMIENTO_C="p_hacina_c", var_salidas="IVS"))
corr_covid  = cor(covid_df[,4:10])
corrplot(corr_covid,method = "color",
         addgrid.col = 'white',
         addCoef.col = 'white',
         type = "upper", tl.cex = 0.7, number.cex = 0.6)

#scatter plots
pairs.panels(df_sem20[,4:10], pch = 19,  cex = 0.5, hist.col = "light blue", cor = FALSE, cex.labels = 2)

#Se eliminan las de final csv
final= df_sem20
final$semana_max=NULL
final$`SEM INICIO`=NULL
final$P_VIAJES_ORI=NULL
final$VIAJES_ORI=NULL
final$max_contagios=NULL
final$DIF_IME=NULL
final$N_POB=NULL
final$semana_max_n=NULL
final$dif_cuar_peak=NULL
final$EST_POB=NULL
final$sem_max_posi=NULL
final= rename(final,c(tasa_max_contagios="tasa_max", DENSIDAD="densidad",P_POB="p_pob", SEMANA_Entro="sem_cuar",P_HACINAMIENTO="p_hacina",P_HACINAMIENTO_C="p_hacina_c", var_salidas="IVS"))
corr_final = cor(final[,4:10])
corrplot(corr_final,method = "color",
         addgrid.col = 'white',
         addCoef.col = 'white',
         type = "upper", tl.cex = 0.7, number.cex = 0.6)

final = read_csv("final.csv")

#Regresiones lm

modelo1 <- lm(tasa_max ~ sem_cuar + p_hacina_c + IVS , data = shape@data)
modelo2 <- lm(tasa_max ~ sem_cuar + densidad + IVS , data = shape@data)
modelo3 <- lm(tasa_max ~ log(sem_cuar) + p_hacina_c + IVS , data = shape@data)

#Regresiones espaciales

sar_modelo1 = lagsarlm(formula =tasa_max ~ sem_cuar + p_hacina_c + IVS , data = shape@data, listw = W_list)
sar_modelo2 = lagsarlm(formula = tasa_max ~ sem_cuar + densidad + IVS , data = shape@data, listw = W_list)
sar_modelo3 = lagsarlm(formula = tasa_max ~ log(sem_cuar) + p_hacina_c + IVS , data = shape@data, listw = W_list)



#test AIC

AIC(modelo1)
AIC(modelo2)
AIC(modelo3)

AIC(sar_modelo1)
AIC(sar_modelo2)
AIC(sar_modelo3)




#test de normalidad de residuos 
shapiro.test(modelo1$residuals)
shapiro.test(modelo2$residuals)
shapiro.test(modelo3$residuals)

shapiro.test(sar_modelo1$residuals)
shapiro.test(sar_modelo2$residuals)
shapiro.test(sar_modelo3$residuals)


