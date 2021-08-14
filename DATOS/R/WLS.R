library(readr)
library(lmtest)

#Se lee el df
covid_df <- read_csv("DATOS/CSV/covid_table1008.csv")

#df con semanas de inicio igual o menor a 20
df_sem20 = covid_df[covid_df$SEMANA_Entro <= 20,]

#Regresiones
r1<- lm(log(tasa_max_contagios) ~ SEMANA_Entro + DENSIDAD + var_salidas , data = df_sem20)
summary(r1)
r2 = lm(tasa_max_contagios ~ var_salidas + P_HACINAMIENTO_C, data = df_sem20)
summary(r2)

#Test de heterocedasticidad
bptest(r1)
bptest(r2)

