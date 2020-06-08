### Proyecto final Modelado Estadístico ###
### Fecha inicio:            30/05/2020 ###
### Fecha fin:                5/06/2020 ###

#Usando la ruta absoluta del equipo en el que se programó este Script
#Nombre del archivo del conjunto de datos: Consumo_Gasolina_Autos_Ene_2018.xlsx

library(readxl)
library(utf8)

setwd("../Desarrollo/Diplomado/Modelos Estadisticos/Modulo_II_Modelos_Estadisticos/")

df = read_excel("../Datos/Datos_tratados.xlsx")
#df = read.csv(file = "../Datos/Consumo_Gasolina_Autos_Ene_2018.csv")

dim(df)
names(df)
str(df)

attach(df)

tabla_transmision_fa = table(Trans_resumen);tabla_transmision
tabla_transmision_fr = prop.table(tabla_transmision_fa);tabla_transmision_fr

barplot(tabla_transmision_fa, col=3:4, ylim=c(0,3500))
title("Diagrama de barras - Transmisión \n (Frecuencias absolutas)")

barplot(tabla_transmision_fr, col=3:4, ylim=c(0,0.8))
title("Diagrama de barras - Transmisión \n (Frecuencias relativas)")

