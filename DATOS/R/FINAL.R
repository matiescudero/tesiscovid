library(reshape)
library(corrplot)
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


#Regresiones 

r1<- lm(tasa_max~ sem_cuar + p_hacina_c+ IVS , data = final)
summary(r1)
r3<- lm(tasa_max~ sem_cur + densidad + IVS , data = final)
summary(r3)


#test AIC

AIC(r1)
AIC(r2)
#test de normalidad de residuos 
shapiro.test(r1$residuals)
shapiro.test(r2$residuals)
