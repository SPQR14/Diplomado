### Proyecto final Modelado Estadístico ###
### Fecha inicio:            30/05/2020 ###
### Fecha fin:                5/06/2020 ###

#Usando la ruta absoluta del equipo en el que se programó este Script
#Nombre del archivo del conjunto de datos: Consumo_Gasolina_Autos_Ene_2018.xlsx

library(readxl)
library(utf8)

setwd("/home/spqr14/Desarrollo/Diplomado/Modelos Estadisticos/Modulo_II_Modelos_Estadisticos/")

df = read_excel("../Datos/Consumo_Gasolina_Autos_Ene_2018.xlsx")

dim(df)
names(df)
str(df)

attach(df)

tabla_marca <- table(Marca);tabla_marca
tabla_submarca <- table(Submarca);tabla_submarca

barplot(tabla_marca)
