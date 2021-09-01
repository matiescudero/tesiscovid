library(reshape)
library(readr)
library(corrplot)
library(MASS)

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
corrplot(corr_final,method = "pie", type = "upper", tl.cex = 0.7, number.cex = 0.6)

final = read_csv("final.csv")

#Regresiones 


modelo1<- lm(tasa_max ~ sem_cuar + p_hacina_c + IVS , data = final)
summary(modelo1)
modelo2<- lm(tasa_max ~ sem_cuar + densidad + IVS , data = final)
summary(modelo2)
modelo3 = lm(tasa_max ~ log(sem_cuar) + p_hacina_c + IVS , data = final)
summary(modelo3)
modelo4 = lm(log(tasa_max) ~ sem_cuar + p_hacina_c + IVS , data = final)

modelo5 = lm(log(tasa_max) ~ sem_cuar + densidad + IVS , data = final)

modelo6 = lm(log(tasa_max) ~ log(sem_cuar) + p_hacina_c + IVS , data = final)
#test AIC

AIC(modelo1)
AIC(modelo2)
AIC(modelo3)
AIC(modelo4)
AIC(modelo5)
AIC(modelo6)


#test de normalidad de residuos 
shapiro.test(modelo1$residuals)
shapiro.test(modelo2$residuals)
shapiro.test(modelo3$residuals)
shapiro.test(modelo4$residuals) #hacinamiento
shapiro.test(modelo5$residuals) #densidad
shapiro.test(modelo6$residuals)



