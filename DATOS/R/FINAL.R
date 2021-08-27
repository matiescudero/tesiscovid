library(reshape)
library(corrplot)
final= df_sem20
final= rename(final,c(tasa_max_contagios="tasa_max", DENSIDAD="densidad",P_POB="p_pob", SEMANA_Entro="sem_cuar",P_HACINAMIENTO="p_hacina",P_HACINAMIENTO_C="p_hacina_c", var_salidas="IVS"))
corr_final = cor(final[,4:10])
corrplot(corr_final,method = "pie", type = "upper", tl.cex = 0.7, number.cex = 0.6)
