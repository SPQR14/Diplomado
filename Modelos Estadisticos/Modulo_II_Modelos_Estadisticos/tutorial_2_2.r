###############################################################################################
########################### V. ANALISIS DE VARIABLES CUANTITATIVAS ############################
###############################################################################################

# A. Datos cuantitativos discretos 

### Tabla de frecuencias
attach(datos1)
tabla_cursos <- table(cursos)  # si me sale error escribo de nuevo "attach(emp)"
print(tabla_cursos)
sum(tabla_cursos)
cumsum(tabla_cursos)
tablar_cursos <- prop.table(tabla_cursos)
print(tablar_cursos)
sum(tablar_cursos)
cumsum(tablar_cursos)
cbind(tabla_cursos,cumsum(tabla_cursos),tablar_cursos,cumsum(tablar_cursos))

# Como la variable es discreta, hagamos un diagrama de barras (cada barra puede estar asociada a 
# un valor distinto). ¿Sera util e interpretable?

par(mfrow= c(1,2))
barplot(tabla_cursos)  # frecuencias absolutas
title("Diagrama de barras (Cursos) \n (Frecuencias absolutas)")

barplot(tablar_cursos,col=c(10:20))   # frecuencias relativas
title("Diagrama de barras (Points) \n (Frecuencias relativas)")

#¿Que pasa si a la "ingenua" graficamos un histograma?

hist(cursos,breaks=-1:9,plot=FALSE)  # las clases son: [-1,0] (0,1],...,(8,9])
# OJO: las clases (breaks) los defini asi para que se pareciera al diagrama
# de barras y poderlo comparar.

hist(cursos,breaks=-1:9,probability=TRUE)

# El perfil es "identico", solo que en el segundo caso las barras estan pegadas. 
# ¿Sera correcto?

# ¿Diagrama de pay?

par(mfrow=c(1,2))
pie(tabla_cursos,labels=paste(c("0","1","2","3","4","5","6","7","8","9"),
                              tabla_cursos,sep=": "),radius=1)
title("Diagrama de pay - Cursos \n (Frecuencias absolutas)")

pie(tablar_cursos,labels=paste(c("0","1","2","3","4","5","6","7","8","9"),
                               tablar_cursos,sep=": "),radius=1)
title("Diagrama de pay - Cursos \n (Frecuencias relativas)")

### Diagrama de puntos (dotplot o strip chart)

dotchart(cursos,labels=row.names(datos1),cex=1,xlab="Numero de cursos",main="Diagrama de puntos")  # Cleveland dot plot
#cex = minificador


# Mejor tratemos con un subconjunto:
datos2<- subset(datos1,cursos>4); datos2
dotchart(datos2$cursos,labels=row.names(datos2),cex=0.8,xlab="Numero de cursos",main="Diagrama de puntos")  # Cleveland dot plot

# Y si mejor seleccionamos al azar?
datos3<- sample(datos1$cursos,size=100,replace = F); datos3
dotchart(datos3,cex=0.8,xlab="Numero de cursos",main="Diagrama de puntos")  # Cleveland dot plot

# Muestreo aleatorio de TODO el conjunto de datos

sample(datos1,10,replace = F)  # no!

datos4<- datos1[sample(nrow(datos1),90,replace = F),]; datos4  
dotchart(datos4$cursos,labels=row.names(datos4),cex=0.8,xlab="Numero de cursos",main="Diagrama de puntos")  # Cleveland dot plot


# Otro tipo de grafica de puntos
#install.packages("BHH2")
library(BHH2)

dotPlot(na.omit(datos4$cursos),main="Diagrama de puntos (muestra aleatoria)",xlab="Numero de puntos",pch=16)

### Diagrama de tallo y hojas

# Recordemos que en este diagrama se divide a cada observacion en dos partes: tallo y hoja.
# Cuando hay punto decimal, este se usa como separador.
# El problema es cuando se tienen muchos digitos a la derecha del separador (hojas). Esto es 
# poco practico, por lo que solo se considera un decimal, ¿como escogemos este unico decimal?
# El argumento "scale" permite modificar la longitud del diagrama y con ello tener este unico
# decimal. Por ejemplo:

stem(datos1$cursos, scale=0.1)  

# Los valores que se leen son: 0.0,0.1,0.5,0.6,0.9

stem(datos1$cursos, scale=1) 

# Los valores que se leen son: 0,1,2,3,4,5,6,9


# COMENTARIO: En el primer caso, el valor exacto se pierde. Sin embargo, esto no es relevante
# para este tipo de grafica/tabla. Solo interesa identificar alrededor de cual valor se concentran
# mas las observaciones y el perfil que muestra esta distribucion.

#-------------------------------------------------------------------------------------

# B. Datos cuantitativos continuos

### Tabla de frecuencias
tabla_c <- table(datos1$aptitudes)
print(tabla_c)
sum(tabla_c)
cumsum(tabla_c)
tablar_c <- prop.table(tabla_c)
print(tablar_c)
sum(tablar_c)
cumsum(tablar_c)

# COMENTARIO: para este tipo de variable es muy posible que cada observacion sea unica. De ahi
# que no sea adecuada esta tabla para resumir la informacion.
# ¿Y si queremos hacer una grafica de barras?

barplot(tabla_c)
title("Diagrama de barras (aptitudes) \n (Frecuencias absolutas)")

barplot(tablar_c)
title("Diagrama de barras (aptitudes) \n (Frecuencias relativas)")

# COMENTARIO: Se observa que no es util para este tipo de variable representarlo asi, por
# eso conviene hacer una tabla de frecuencias usando INTERVALOS DE CLASE.

# En R tenemos dos opciones para construir una tabla de frecuencias para datos agrupados.

# OPCION 1: Usando la funcion "hist"

hist_c <- hist(datos1$aptitudes,breaks=seq(70,160,10),plot=F);hist_c# se debe guardar como objeto para accesar a sus
# elementos

# Experimento: vean que pasa si breaks=c(70,152,10)
hist_c1 <- hist(datos1$aptitudes,breaks=seq(70,152,10),plot=F);hist_c1

# Con breaks se indica el numero de cortes (o intervalos) deseados. Si se omite, R, elige por
# default los que sean adecuados para ese conjunto de observaciones.

# En este ejemplo, le indicamos que comience en 70 y termine en 160. ¿De donde salieron estos valores?

range(datos1$aptitudes)  # muestra el valor minimo y maximo

# Bajamos un poco del minimo y subimos un poco del maximo. 

summary(hist_c)   # muestra los elementos que conforman al objeto histograma

# Tabla de frecuencias
n <- length(hist_c$breaks)
tab_c <- cbind(hist_c$breaks[-n],hist_c$breaks[-1],hist_c$counts,round(hist_c$counts/sum(hist_c$counts),digits=3),
               cumsum(hist_c$counts),round(cumsum(hist_c$counts/sum(hist_c$counts)),digits=3))
dimnames(tab_c)[[2]]<-c("Linf","Lsup","f","fr","F","Fr")
print(tab_c)

# Es una manera "muy artesanal" de construirla, pero no muestra ni parentesis ni corchetes en cada
# intervalo. Mejor usamos la opcion 2.

# OPCION 2: Usando el paquete "fdth"
attach(datos1)
library(fdth)
(tabla_cont<- fdt(datos1$aptitudes,breaks="Scott",start=70,end=160,h=5,right=T))

(tabla_cont<- fdt(datos1$aptitudes,breaks="Sturges",right=T))

(tabla_cont<- fdt(datos1$aptitudes,breaks="Sturges",start=70,end=160,h=2,right=T))

(tabla_cont<- fdt(datos1$aptitudes,breaks="FD",right=T))

# Compara las distintas tablas.

# EXTRA: si se requieren intervalos desiguales para observar ciertos detalles de interes.
par(mfrow=c(1,2))
hist_uneq<- hist(aptitudes,breaks = quantile(aptitudes, 0:10 / 10),col="gray")  # usa los cuantiles 
hist(aptitudes,breaks=c(70,72:90,120,160))   # intervalos en puntos arbitrarios


###############################################################################################
##################################### VI. CURVAS POBLACIONALES #################################
###############################################################################################

### Poligono de frecuencias
library(MASS)
frequency.polygon(aptitudes,nclass = nclass.freq(aptitudes))

library(UsingR)
simple.freqpoly(aptitudes,col=rainbow(9))

library(agricolae)
intervals.freq(hist_c)  # intervalos de clase

### Asimetria y curtosis
# A veces es dificil identificar si la distribucion de frecuencias es asimetrica o simetrica
# mediante un histograma o un diagrama de barras. Para ello existen medidas estadisticas que
# nos permiten saber si el perfil es simetrico o no; ademas si es un perfil "picudo" o "plano".
# Estas medidas son: coeficiente de asimetria y coeficiente de curtosis.

# Calculo manual coeficiente de asimetria
n<- length(aptitudes)   # longitud del vector
mu3<- sum((aptitudes-mean(aptitudes))^3)/(n-1)
s3<- sd(aptitudes)^3
j3<- mu3/s3; j3    

# Calculo manual coeficiente de curtosis
mu4<- sum((aptitudes-mean(aptitudes))^4)/(n-1)
s4<- sd(aptitudes)^4
j4<- mu4/s4; j4    

#Curtosis de 3 = curtorsis de la distribución normal
# Usando R

library(agricolae)
(j3_a<- skewness(aptitudes))  

(j4_a<- kurtosis(aptitudes))   # da el exceso de curtosis  
#Te da sólo el valor que sobrepasa la curtósis de las distribución normal
#Exceso o falta de curtósis

# Si no funciona "agricolae"

library(moments)
(j3_b<- skewness(aptitudes))
(j4_b<- kurtosis(aptitudes))  

### Ojiva: solo valida para variables cuantitativas continuas (porque se construye a partir de
# de la tabla de frecuencias con intervalos de clase).

ogive.freq(hist_c)   # requiere al histograma como objeto y depende de la libreria agricolae

# ¿Como se interpreta esta grafica?

# Usemos la siguiente funcion para ubicar un punto sobre la grafica activa, en este caso, la ojiva
# anterior.

locator()

abline(h=0.5,v=105.0371, col="red",lty=3)  # con esto marcamos el punto seleccionado
# h es la linea horizontal
# v es la linea vertical

## Otra manera de dibujar la ojiva (si agricolae no funciona)
plot(tab_c[,2],tab_c[,6],type="o",xlab="Límite superior tiempos",
     ylab="Frecuencia relativa acumulada",main="Ojiva") # tab_c[,2]=limite superior de la clase
# tab_c[,6]=frecuencia relativa acumulada

### EXTRA: Un adelanto de lo que se vera en el siguiente tema, modelos de probabilidad.

hist(aptitudes,probability=T,col="grey50",ylim=c(0,0.04))  # histograma cuyo eje vertical esta en escala
# de "probabilidades".

lines(density(aptitudes),lwd=2,col="red")   # curva de densidad de probabilidad

# la curva anterior muestra el perfil que describe la variable "aptitudes".
#curva emírica

###############################################################################################
################################# VII. ESTADISTICAS DE RESUMEN ################################
###############################################################################################

### 1. Medidas de tendencia central

## a. Moda: es la unica que puede aplicarse a variables en escala nominal.
# Recordemos que es el valor que tiene la frecuencia absoluta mas alta (puede haber mas de una).
attach(datos1)
install.packages("modeest")
library(modeest)    # "genefilter" es una dependencia que NO esta disponible para R 3.6.0
# Otra paqueteria
install.packages("modes")
library(modes)   # no disponible en R 4.0.0

# Variable nominal
mfv(sexo)     # most frequent value
mode(sexo)    # NO!

modes(sex)          # abreviando "sexo" pero marca error
modes(datos1$sex)     # abreviando "sexo" pero aqui NO marca error
modes(sexo)       # da el valor mas frecuente y ademas cuantas veces

# Variable ordinal
mfv(escolaridad)   

# Variable intervalo/razon
mfv(aptitudes)   # continua
mfv(cursos)      # discreta

# ¿que resultado muestra? ¿tiene sentido para una variable continua?
# Notese que si las observaciones se redondean es posible que encontremos un valor que se 
# repita varias veces. El programa R lo que hace es redondear hacia abajo (floor) y con ello localiza 
# la moda. Esto no es adecuado.

# Para una variable continua debemos construir su tabla de frecuencias y a partir de esta, identificar
# la clase (o clases) con la frecuencia absoluta mas alta. Esta se llamara CLASE MODAL.

# Habiamos visto que

tabla_cont   # tabla de frecuencias para datos agrupados por intervalos de clase

str(tabla_cont)    # para ver como debemos llamar a cada elemento
which.max(tabla_cont$table$f)  # indica el renglon cuya frecuencia absoluta, f, es maxima


tabla_cont$table$`Class limits`[which.max(tabla_cont$table$f)] # indica la CLASE MODAL

# OJO: recuerden que R ejecuta las instrucciones de la mas interna a la mas externa.

### b. Mediana: necesita como minimo que la variable este en escala ordinal.

# Variable ordinal
median(escolaridad)


# Variable intervalo/razon
median(aptitudes)
median(cursos)

### c. Media: es sensible a la presencia de datos atipicos (outliers) 

# Variable nominal
mean(sexo)  # aunque la puede calcular, tiene sentido el resultado?

# Variable ordinal

mean(escolaridad)       # no tiene interpretacion por lo que no es util

# COMENTARIO: MEJOR NO INTERPRETAMOS LOS RESULTADOS NUMERICOS DE LAS VARIABLES CUALITATIVAS

# Variable intervalo/razon
mean(cursos)
mean(aptitudes)

# EXTRA: Media truncada (trimmed mean)  #util para distribuciones con colas largas

# NOTA: "trim" se refiere a la fraccion de observaciones (0-0.5) que seran reducidas de cada
# extremo antes de obtener el promedio.

# Metodo 1: funcion "mean" de R
trim.m05<- mean(na.omit(aptitudes),trim=0.05) #elimina el 5% por cada extemo
trim.m10<- mean(na.omit(aptitudes),trim=0.10)
trim.m15<- mean(na.omit(aptitudes),trim=0.15)
trim.m50<- mean(na.omit(aptitudes),trim=0.50)
trim.m75<- mean(na.omit(aptitudes),trim=0.75) # ya me pase!
trim<- c(trim.m05,trim.m10,trim.m15,trim.m50,trim.m75)
names(trim)<- c("5%trim","10%trim","15%trim","50%trim","75%trim")
trim

# COMENTARIO: Observese como cambia el promedio segun la cantidad de observaciones. 

### d. Porcentiles
# La funcion "quantile" tiene problemas con datos faltantes. Si este fuera el caso escribir "na.omit"

# Variables nominales: no se puede

# Variables ordinales: aunque esta variable este registrada con numeros

quantile(escolaridad,prob=c(0.10,0.25,0.5,0.75))  

# NOTA: cuando ponemos prob=0.25,0.5,0.75 se refiere a los CUARTILES

# Los resultados no tienen sentido ni interpretacion

# Variables intervalo/razon:

quantile(na.omit(aptitudes),prob=c(0.12,0.18,0.2,0.5,0.9))  
quantile(na.omit(datos1$apt),prob=c(0.12,0.18,0.2,0.5,0.9))  # noten la abreviatura

## EXTRA: SIMETRIA 

# Se ha visto que si comparamos la moda, media y mediana podemos darnos una idea acerca de la
# simetria o asimetria.

# Usamos la variable: aptitudes
# Vimos que la clase modal es el intervalo (99.6563,104.983] (ver tabla_cont)
# Mediana: median(aptitudes)=104.18
# Media: mean(aptitudes)=105.1989

# moda< mediana< media por lo que esta SESGADA A LA DERECHA. En efecto, cuando calculamos
# skewness(aptitudes) resulto ser positivo.
skewness(aptitudes)
### 2. Medidas de dispersion

# Variables intervalo/razon: 

# a. Rango + Rango intercuartilico
range(na.omit(aptitudes))  # rango o amplitud
IQR(na.omit(aptitudes))    # rango o amplitud intercuartilica, requisito para construir gráfica

# b. varianza + desviacion estandar + error medio
var(na.omit(aptitudes))   # varianza
sd(na.omit(aptitudes))   # desviacion estandar (o tipica)
mean(abs(na.omit(aptitudes)-mean(na.omit(aptitudes))))   # error medio (media)
mean(abs(aptitudes-median(aptitudes)))  # error medio (mediana)
mean(abs(aptitudes-mfv(aptitudes)))  # error medio (moda) OJO!!!


#******************** RESUMEN ESTADISTICA DESCRIPTIVA ***********************#


summary(aptitudes)     #Cuartiles + minimo + maximo + media
# NOTA: si la variable es cualitativa(verbal), indica el # de observaciones en cada categoria

###############################################################################################
#################### IX. COMPARACION Y ASOCIACION ENTRE VARIABLES #############################
###############################################################################################

## a) variable ordinal vs variable cuantitativa: COMPARACION ENTRE VARIABLES
# Para comparar el comportamiento de estas variables conviene usar un diagrama de caja-brazos.
# Este diagrama se usa para la variable cuantitativa, la cualitativa solo sirve para separar
# el analisis por casos.

# Cualitativa: sexo
# Cuantitativa: aptitudes

boxplot(aptitudes, horizontal = T, main="Aptitudes")   # no distinguimos aun el sexo del empleado

# Ahora, analicemos esta variable por sexo
boxplot(aptitudes~sexo, horizontal = T ,col="yellow") # ~sexo separa los datos de aptitudes

boxplot(aptitudes~escolaridad, col = "yellow")
boxplot(escolaridad~sexo, col = "yellow")

  # Otra manera
par(mfrow=c(1,1))   # ventana original
boxplot(aptitudes[sexo=="1"],aptitudes[sexo=="2"],
        col=c("yellow","pink"),names=c("mujeres","hombres"))

## b) Variable cuantitativa vs variable cuantitativa: ASOCIACION ENTRE VARIABLES

cor(c(antiguedad,horasextra,cursos,incapacidad,aptitudes,salario,edad),method = "pearson")       # error!!

# Mejor escribimos...
round(cor(datos1[c(1,2,4:6,8,9)]),digits = 2) 

# Graficamente...
plot(datos1[c(1,2,4:6,8:9)])   # matriz de graficas de dispersion

round(cor(datos1), digits = 2)
## c) Variable cuantitativa vs variable cuantitativa: COMPARACION ENTRE VARIABLES

# Comparamos tanto la "edad" como las "aptitudes":

cv_aptitudes<- sd(aptitudes)/mean(aptitudes); cv_aptitudes   # coeficiente de variacion
cv_edad<- sd(edad)/mean(edad); cv_edad     # coeficiente de variacion

# ¿Donde hay mas variabilidad en cuanto a las aptitudes de acuerdo con el sexo?

mujer<- datos1[sexo=="1",]  # separamos por sexo
hombre<- datos1[sexo=="2",]  # separamos por sexo

cv_mujer<- sd(mujer$aptitudes)/mean(mujer$aptitudes); cv_mujer

cv_hombre<- sd(hombre$aptitudes)/mean(hombre$aptitudes); cv_hombre

pdf("Boxplots_empleados.pdf")    # para guardar la imagen (con esta linea se "abre" la instruccion de guardado)  
par(mfrow=c(1,2))
boxplot(mujer$aptitudes,horizontal=T,main="Mujeres",xlab="Aptitudes")
text(x = boxplot.stats(mujer$aptitudes)$stats, labels = boxplot.stats(mujer$aptitudes)$stats, y = 1.3)
boxplot(hombre$aptitudes,horizontal=T,main="Hombres",xlab="Aptitudes")
text(x = boxplot.stats(hombre$aptitudes)$stats, labels = boxplot.stats(hombre$aptitudes)$stats, y = 1.3)
dev.off()  # para guardar la imagen (con esta linea se "cierra" la instruccion de guardado)   

# Resumen de los 5 numeros descriptivos

res_apt<-matrix(fivenum(aptitudes),nrow=1,ncol=5)    # sin separar por sexo fivenum -> resumir 5 números descriptivos
colnames(res_apt)<- c("Min","q1","q2","q3","Max")
rownames(res_apt)<- c("")
res_apt

