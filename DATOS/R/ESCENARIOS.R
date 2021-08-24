### Data de escenario 
escenarios=df_sem20

##regresion con log
escenarios$esc_1_log= df_sem20$SEMANA_Entro*0.02791+ df_sem20$DENSIDAD*0.00001621+ df_sem20$var_salidas*0.01846 + 4.338
##regresion
escenarios$tasa_max_log= log(df_sem20$tasa_max_contagios)
escenarios$esc_1=exp(escenarios$esc_1_log)
escenarios$real_pred=escenarios$tasa_max_contagios-escenarios$esc_1

##variacion de movilidad 80%
escenarios$esc_2= df_sem20$SEMANA_Entro*0.02791+ df_sem20$DENSIDAD*0.00001621+ 80*0.01846 + 4.338
escenarios$esc_2=exp(escenarios$esc_2)

##variacion de movilidad
escenarios$esc_3= df_sem20$SEMANA_Entro*0.02791+ df_sem20$DENSIDAD*0.00001621+ 40*0.01846 + 4.338
escenarios$esc_3=exp(escenarios$esc_3)
