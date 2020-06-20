### Proyecto final Modelado Estadístico ###
### Fecha inicio:            30/05/2020 ###
### Fecha fin:                5/06/2020 ###

#Usando la ruta absoluta del equipo en el que se programó este Script
#Nombre del archivo del conjunto de datos: Consumo_Gasolina_Autos_Ene_2018.xlsx

library(readxl)
library(utf8)
library(ggplot2)

setwd("../Desarrollo/Diplomado/Modelos Estadisticos/Modulo_II_Modelos_Estadisticos/")

df = read_excel("../Datos/Datos_tratados.xlsx")
#df = read.csv(file = "../Datos/Consumo_Gasolina_Autos_Ene_2018.csv")

dim(df)
names(df)
str(df)

attach(df)

#Tablas y gráficos por transmisión
tabla_transmision_fa = table(Trans_resumen);tabla_transmision_fa
tabla_transmision_fr = prop.table(tabla_transmision_fa);tabla_transmision_fr

barplot(tabla_transmision_fa, col=3:4, ylim=c(0,3500))
title("Diagrama de barras - Transmisión \n (Frecuencias absolutas)")

barplot(tabla_transmision_fr, col=3:4, ylim=c(0,0.8))
title("Diagrama de barras - Transmisión \n (Frecuencias relativas)")


#Tablas y graficos por combustible
tabla_combustible_fa = table(df$Comb.);tabla_combustible_fa
table_combustible_fr = prop.table(tabla_combustible_fa); table_combustible_fr

barplot(tabla_combustible_fa, col=3:4, ylim=c(0,5500))
title("Diagrama de barras - Combustible \n (Frecuencias absolutas)")

barplot(table_combustible_fr, col=3:4, ylim=c(0,1))
title("Diagrama de barras - Combustible \n (Frecuencias relativas)")

#tablas y gráficos por marca
tabla_marcas_fa = table(df$Marca);tabla_marcas_fa
tabla_marcas_fr = prop.table(tabla_marcas_fa);tabla_marcas_fr
par(las=2)
barplot(tabla_marcas_fr, col=heat.colors(43), xlim=c(0,0.1), horiz = TRUE)
title("Diagrama de barras - Combustible \n (Frecuencias relativas)")

#tablas y gráficos por marca
tabla_cilindros_fa = table(df$Cilindros);tabla_cilindros_fa
tabla_cilindros_fr = prop.table(tabla_cilindros_fa);tabla_cilindros_fr
par(las=1)
barplot(tabla_cilindros_fr, col=heat.colors(43), ylim=c(0,0.6))
title("Diagrama de barras - Número de cilindros \n (Frecuencias relativas)")

