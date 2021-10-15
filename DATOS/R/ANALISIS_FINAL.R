library(readr)

#csv final
final <- read_csv("DATOS/CSV/escenarios_moran.csv")

#nuevas columnas
final$max_contagios = (final$tasa_max * final$Poblacion)/100000
