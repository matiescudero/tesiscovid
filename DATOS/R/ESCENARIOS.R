df_sem20$esc_1_log= df_sem20$SEMANA_Entro*0.02791+ df_sem20$DENSIDAD*0.00001621+ df_sem20$var_salidas*0.01846 + 4.338
df_sem20$tasa_max_log= log(df_sem20$tasa_max_contagios)
df_sem20$esc_1=exp(df_sem20$esc_1_log)
df_sem20$real_pred=df_sem20$tasa_max_contagios-df_sem20$esc_1
summary(df_sem20)
