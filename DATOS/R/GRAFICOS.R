library(readr)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(hrbrthemes)


#Se exporta data frame desde GitHub
url_file = "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto15/FechaInicioSintomas.csv"
casos_covid = read_csv(url(url_file))

#Se filtra unicamente para la RM
casos_covid = casos_covid[(casos_covid$Region == 'Metropolitana'),]

#Se dejan únicamente los gráficos para el 2020
casos_covid = casos_covid[, -c(44:97)]

#Se renombran las columnas
lista_semanas = colnames(casos_covid)[6:43] #Nombres de columnas a lista
lista_semanas = gsub("^.{0,4}","SE",lista_semanas) #Cambio "2020" a "sE"
colnames(casos_covid)[6:43] = lista_semanas #actualizacion de columnas

#Se crea un df que se usara para hacer el grafico de casos para la rm
casos_rm = casos_covid

#Se transforma el formato del data frame para poder trabajarlo con librerías de series de tiempo
casos_gather <- casos_covid %>%
  tidyr::gather(key = Date, value = contagiados, -Comuna, -Region, -Poblacion, -"Codigo comuna")

#Se limpian los valores no deseados generados por el cambio de formato del data frame
confirmados_com = casos_gather[!(casos_gather$Date== "Codigo comuna"|casos_gather$Date== "Codigo region"|casos_gather$Date== "Poblacion"),]

#se transforma a entero el número de contagiados
confirmados_com$contagiados = as.integer(confirmados_com$contagiados)

#Se reemplazan los NA
confirmados_com$contagiados[is.na(confirmados_com$contagiados)] = 0

#Se crea campo que muestre cantidad de nuevos contagios cada 100.000 habitantes
confirmados_com$Tasa_contagiados = (confirmados_com$contagiados/confirmados_com$Poblacion)*100000
confirmados_com$Tasa_contagiados = as.integer(confirmados_com$Tasa_contagiados)


#Se genera un df que incluya algunas comunas para ejemplificar
df_ejemplo = confirmados_com[(confirmados_com$Comuna == 'San Ramon') |
                               (confirmados_com$Comuna == 'La Granja') |
                               (confirmados_com$Comuna == 'Recoleta') | 
                               (confirmados_com$Comuna == 'Maipu'), ]

#Columna con información para graficar
df_ejemplo$label_pob = paste("Población:", format(df_ejemplo$Poblacion,
                                                  decimal.mark = ",",
                                                  big.mark = "."))


#Gráficos para casos nuevos
ggplot(data=df_ejemplo, aes(x=Date,y=contagiados, group=1)) +
  geom_line(color="#69b3a2", size = 1) +
  labs(x = "Semana Epidemiológica", y = "Casos Nuevos") +
  facet_wrap(~Comuna, nc=2,
             labeller = as_labeller(c(`Maipu` = "Maipú",
                                      `La Granja` = "La Granja",
                                      `Recoleta` = "Recoleta",
                                      `San Ramon`= "San Ramón"))) +
  theme(axis.text.x=element_text(size=7,angle=60, hjust=1)) +
  geom_text(x = 29, y = 1600, aes(label = label_pob), 
            data = df_ejemplo,
            size = 4,
            color = "gray30",
            family = "sans")

#Gráficos para tasas
ggplot(data=df_ejemplo, aes(x=Date,y=Tasa_contagiados, group=1)) +
  geom_line(color="#69b3a2", size = 1) +
  labs(x = "Semana Epidemiológica", y = "Tasa Casos Nuevos") +
  facet_wrap(~Comuna, nc=2,
             labeller = as_labeller(c(`Maipu` = "Maipú",
                                      `La Granja` = "La Granja",
                                      `Recoleta` = "Recoleta",
                                      `San Ramon`= "San Ramón")))+
  theme(axis.text.x=element_text(size=7,angle=60, hjust=1)) +
  geom_text(x = 29, y = 800, aes(label = label_pob), 
            data = df_ejemplo,
            size = 4,
            color = "gray30",
            family = "sans")

##GRAFICO REGIONAL
#Se eliminan columnas que no sirven
casos_rm = casos_rm[, -c(2:5)]

#Se añade una columna de totales
casos_rm = rbind(casos_rm, data.frame(Region = "Total", t(colSums(casos_rm[, -1]))))

#Se deja únicamente la fila de a region completa
casos_rm = casos_rm[-c(1:53),]

#transformación de formato
casos_gather_rm <- casos_rm %>%
  tidyr::gather(key = Date, value = contagiados, -Region)

#Gráfico para la RM
ggplot(data=casos_gather_rm, aes(x=Date,y=contagiados, group=1)) +
  geom_line(color="#69b3a2", size = 1) +
  labs(x = "Semana Epidemiológica", y = "Casos Nuevos") +
  theme(axis.text.x=element_text(size=7,angle=60, hjust=1))


#Histogramas multiples
df_final <- read_csv("final.csv")

ggplot(gather(df_final[,-c(1,2,3,4)]), aes(value)) + 
  geom_histogram(bins =  8, color = "black", fill = "lightblue") + 
  facet_wrap(~key, scales = 'free_x')  +
  labs(x="", y = "Frecuencia")

