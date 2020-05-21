# Diplomado en Tecnicas Estadisticas y Mineria de Datos
# MODULO II: Modelos Estadisticos
# Prof. Elizabeth Martinez
# FES-Acatlan, UNAM
# TUTORIAL 2: ESTADISTICA DESCRIPTIVA EN R

# **********************************************************************
# **********************************************************************
# **********************************************************************

#======== Resumen de comandos en R ==========

#---Se puede definir un espejo como repositorio:
#options(repos="http://cran....")  # https://cran.r-project.org/

# Para checar la version instalada de R:
version
version[['version.string']]
strsplit(version[['version.string']], ' ')[[1]][3]

# Instalar librerias
install.packages("BHH2")

# Cargar librerias (puede hacerse en cualquier momento del script)
library(BHH2)
library(agricolae)
library(modeest)
library(modes)
library(MASS)
library(UsingR)
library(fdth)   

# **********************************************************************
# **********************************************************************
# **********************************************************************

#--- Lectura de datos ---
# Ejemplo (Empleados). Considera una base de datos que contiene informacion de
# 200 empleados de una empresa.

# OJO: para no escribir de nuevo toda la ruta se usa ./
datos1 <- read.table("D:/Desarrollo/Diplomado/Modelos Estadisticos/Datos/Empleados.txt",header=TRUE,row.names=1)
# row.names=1: la columna 1 es el ID y por lo tanto no se debe tomar como una variable.

# Para leer arhivos con tabulador ("\t") como separador se usa:
# install.packages("readr")

# Si los datos estuvieran en el MISMO directorio de trabajo basta con:
#datos1<- read.table("Empleados.txt",header=TRUE,row.names=1)

head(datos1)    # ver los primeros 6 renglones del conjunto de datos
dim(datos1)
names(datos1)
str(datos1)
mode(datos1)


# Exploremos algunas variables

datos1$antiguedad
datos1[,3] # da la columna 3

datos1[3,] # da el renglon 3

datos1$sexo  # da la columna 3

datos1[1:10,3:6]  # para seleccionar una parte de los datos

# pero....
antiguedad  # habiamos llamado datos1$antiguedad

antiguedad*12


# Solucion.....attach(datos1) # con el attach ya puedo accesar a los nombres de las variables

antiguedad

antiguedad*12

# OJO: el problema con attach es que si manejo 2 o mas bases de datos distintas,
# y estas comparten el mismo nombre de las columnas, R se hace bolas y no
# sabra exactamente a que base de datos se refiere.

detach(datos1) # para decirle a R que ya NO usare esa base de datos

ls()   # nos "enlista" todo lo que tengo en la hoja de trabajo

attach(datos1)
#*****************************************************************************************

# 0. Descripcion de las variables

# Las variables que se midieron son
# Antiguedad: antiguedad en años --- Cuantitativa, continua, de razón
# Hora extra: horas extra trabajadas en el año --- Cuantitativa, continua, de razón
# Sexo: 1-mujer, 2-hombre --- Culitativa, discreta, nominal
# Cursos: numero de cursos de capacitacion en un año --- Cuantitativa, discreta, de razón
# Incapacidad: dias de incapacidad --- Cuantitativa, discreta, de razón
# Aptitudes: indice de aptitudes y capacidades --- Cuantitativa, continua, de intervalo
# Escolaridad: 0-bachillerato, 1-licenciatura sin titulo, 2-licenciatura con titulo, 3-posgrado --- Cualitativa, discreta, ordinal
# Salarios: salario anual en dolares --- Cuntitativa, continua, de intervalo
# Edad: edad en años del trabajador --- Cuantitativa, continua, de razón

# Ejercicio: Describe la naturaleza de cada variable. Indica tanto el tipo como la escala de medicion.

#*****************************************************************************************

##########################################################################################
############################ Análisis de variables Cualitativas ##########################
##########################################################################################

# A. Datos cualitativos nominales 

#-Tabla de frecuencias
tabla_sexo <- table(sexo)  # hace una tabla de frecuencias absolutas para esta variable
print(tabla_sexo)
sum(tabla_sexo)
prop.sexo <- round(prop.table(tabla_sexo),digits=4)
print(prop.sexo)
sum(prop.sexo)
cumsum(prop.sexo)

names(tabla_sexo)

names(tabla_sexo)[1]   # solo me da el nombre asociado con 1

names(tabla_sexo)[2]   # solo me da el nombre asociado con 2

sum(tabla_sexo)  # suma los valores

tablar_sexo <- prop.table(tabla_sexo)  # convierte una tabla en una tabla de frecuencias relativas 
print(tablar_sexo)
sum(tablar_sexo)  # suma las entradas de un objeto


# otra forma de obtener las marginales:

frec_relativa.sexo<-c(sum(sexo==1)/length(sexo),sum(sexo==2)/length(sexo))
frec_relativa.sexo

# Diagrama de barras
barplot(tabla_sexo,names.arg=c("Mujer","Hombre"),col=c("pink","dodgerblue4"),ylim=c(0,120))

#barplot(table(sexo)) # más rápido de escribir

abline(h=tabla_sexo,col=c("pink","dodgerblue4"))  # dibuja las lineas horizontales
title("Diagrama de barras - Sexo \n (Frecuencias Absolutas)")

barplot(tablar_sexo,names.arg=c("Mujeres","Hhombres"),col=3:4,ylim=c(0,0.6))
title("Diagrama de barras - Sexo \n (Frecuencias Relativas)")

# ¿Tiene sentido la siguiente gráfica?
#NOOOOO

hist(sexo,breaks=0:2)   # eje vertical es la frecuencia absoluta
hist(sexo,breaks=0:2,probability=TRUE)  # eje vertical es la frecuencia relativa

# Diagrama Circular

par(mfrow=c(1,2))
pie(tabla_sexo,labels=paste(c("Mujeres","Hombres"),tabla_sexo,sep=": "),col=5:6,radius=1)
title("Diagrama de pastel - Sexo \n (Frecuencias absolutas)")   # \n para escribir en otro renglon

pie(tablar_sexo,labels=paste(c("Mujeres","Hombres"),tablar_sexo,sep=": "),col=15:16,radius=1)
title("Diagrama de pastel - Sexo \n (Frecuencias relativas)")

#*****************************************************************************************

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


