# B. Datos cualitativos ordinales

# Ejercicio: Enviar un documento en .doc o .docx que contenga la respuesta a cada inciso
# En el caso de las graficas pegarlas en dicho documento.

# a) De acuerdo a lo discutido en clase y usando los datos de empleados, realiza la tabla 
# de frecuencias de una variable cualitativa cuya escala de medicion sea ordinal. Interpreta
# los resultados.

# b) En una misma hoja de R, dibuja los diagramas de barras para la variable ordinal. Considera:
# 1) frecuencias absolutas
# 2) frecuencias absolutas acumuladas
# 3) frecuencias relativas
# 4) frecuencias relativas acumuladas

library(BHH2)
library(agricolae)
library(modeest)
library(modes)
library(MASS)
library(UsingR)
library(fdth)   

datos1 <- read.table("D:/Desarrollo/Diplomado/Modelos Estadisticos/Datos/Empleados.txt",header=TRUE,row.names=1)
attach(datos1)

tabla_escolaridad <- table(escolaridad)  # hace una tabla de frecuencias absolutas para esta variable
print(tabla_escolaridad)
sum(tabla_escolaridad)
cumsum(tabla_escolaridad)
names(tabla_escolaridad)

barplot(tabla_escolaridad,names.arg=c("Bachillerato","Licenciatura. s/t", "Licenciatura. c/t", "Pogrado"),col=c("dodgerblue1", "dodgerblue2","dodgerblue3","dodgerblue4"),ylim=c(0,120))
title("Escolaridad\nFrecuencias absolutas")
barplot(cumsum(tabla_escolaridad),names.arg=c("Bachillerato","Licenciatura. s/t", "Licenciatura. c/t", "Pogrado"),col=c("dodgerblue1", "dodgerblue2","dodgerblue3","dodgerblue4"),ylim=c(0,200))
title("Escolaridad\nFrecuencias absolutas acumuladas")


tablar_escolaridad <- prop.table(tabla_escolaridad);tablar_escolaridad
cumsum(tablar_escolaridad)
barplot(tablar_escolaridad,names.arg=c("Bachillerato","Licenciatura. s/t", "Licenciatura. c/t", "Pogrado"),col=c("dodgerblue1", "dodgerblue2","dodgerblue3","dodgerblue4"),ylim=c(0,0.6))
title("Escolaridad\nFrecuencias relativas")

barplot(cumsum(tablar_escolaridad),names.arg=c("Bachillerato","Licenciatura. s/t", "Licenciatura. c/t", "Pogrado"),col=c("dodgerblue1", "dodgerblue2","dodgerblue3","dodgerblue4"),ylim=c(0,1))
title("Escolaridad\nFrecuencias relativas acumuladas")




