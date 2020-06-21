# Diplomado en Tecnicas Estadisticas y Mineria de Datos
# MODULO II: Modelos Estadisticos
# Prof. Elizabeth Martinez
# FES-Acatlan, UNAM
# TUTORIAL 6: ESTIMACION POR INTERVALOS Y PRUEBAS DE HIPOTESIS (PARAMETRICAS)

###############################################################################################
###############################################################################################
############################### PRUEBAS DE HIPOTESIS PARAMETRICAS #############################
###############################################################################################
###############################################################################################

# Ejemplo 1: Los siguientes datos representan los tiempos de armado de 20 
# peluches seleccionados al azar:

tiempos<- c(9.8,10.4,10.6,9.6,9.7,9.9,10.9,11.1,9.6,10.2,10.3,
            9.6,9.9,11.2,10.6,9.8,10.5,10.1,10.5,9.7)

# supongase que el tiempo necesario para armar cada peluche es una variable aleatoria 
# normal con media mu y desviacion estandar sigma= 0.6 min. De acuerdo con esta 
# muestra, ?existe razon para creer, con un nivel de signicancia de 0.05, que el tiempo 
# de armado promedio no es de 10 minutos?

# H0: mu=10   vs Ha: mu!=10 (distinto de 10 min)

library(PASWR)

zcrit1<- qnorm(0.05/2,mean=0,sd=1,lower.tail=T)
zcrit2<- qnorm(0.05/2,mean=0,sd=1,lower.tail=F)

prueba1<- z.test(tiempos,alternative="two.sided",mu=10,sigma.x=0.6,conf.level=0.95)
prueba1$statistic
prueba1$p.value
prueba1$conf.int


# EXTRA: https://hselab.shinyapps.io/critvalues/  


#****************************************************************************************

# Ejemplo 2: Se lleva a cabo un estudio para saber si cierto tipo de peliculas 
# afecta a los niveles de agresividad de una persona. Para ello se miden los 
# niveles de agresividad antes y despues de ver una pelicula de accion en una
# muestra de 8 individuos. ?Las personas se vuelven mas agresivas despues de ver
# la pelicula de accion? Utiliza una significancia del 5%.

antes<- c(7,8.1,4.2,5,14,6.5,7.3,10)   # poblacion 1
despues<- c(8.2,9.1,5.3,6.2,10,6.5,8,11.2)  # poblacion 2

# Las hipotesis a contrastar son: H0:mu1>=mu2   vs  Ha: mu1< mu2
# O bien: H0: mu1-mu2>=0   vs  Ha: mu1-mu2<0

# muestras independientes
t.test(antes,despues,alternative="less",mu0 = 0,paired = F,var.equal = T,conf.level = 0.95)

t.crit= qt(0.05,df=n1+n2-2,lower.tail = T); t.crit


# muestras dependientes
t.test(antes,despues,alternative="less",mu0 = 0,paired = T,conf.level = 0.95)

t.crit2= qt(0.05,df=n-1,lower.tail = T); t.crit2


#****************************************************************************************

## Ejemplo 3:  En una muestra de 105 comercios seleccionados al azar de una zona,
# se registra si han tenido ganancias/perdidas en este mes. Un analista
# economico de la zona establece que la proporcion de comercios en la zona
# con perdidas es inferior a 0.35. ?Sera cierto lo que dice el analista?


datos<- read.table("./Datos/comercios.dat")
attach(datos)

# Las hipotesis a contrastar son: H0:p>=0.35   vs  Ha: p< 0.35

num.exitos<- length(datos$V1[datos$V1=="p?rdida"])


binom.test(x=num.exitos, n=nrow(datos), p=0.35,alternative = "less")  # muestras peque?as
prop.test(x=num.exitos, n=nrow(datos), p=0.35,
          alternative = "less",correct = F)  # muestras grandes, correct =F -> prueba Z
                                                                      

#****************************************************************************************

## Ejemplo 4: Es frecuente que se comparen dos computadoras al ejecutar un conjunto 
# de varios programas y registrar la diferencia en tiempo del CPU necesario para 
# completar el mismo programa. Se ejecutaron seis de estos programas en dos computadoras
# distintas y produjeron la siguiente tabla de tiempos del CPU (minutos):

comp1<- c(1.12,1.73,1.04,1.86,1.47,2.10)
comp2<- c(1.15,1.72,1.10,1.87,1.46,2.15)

dif<- comp1-comp2
mean(dif)
var(dif)

# Prueba de hipotesis para comparacion de medias con varianza desconocida (muestras indep)

# Primero verifiquemos si se puede asumir igualdad de varianzas:

# a) IC al 97%

vartest_3<- var.test(comp1,comp2,ratio=1,alternative="two.sided",conf.level = 0.97)
vartest_3$conf.int



prueba3<- t.test(comp1,comp2,alternative="two.sided",mu=0,paired=F,var.equal=T,conf.level=0.95)
prueba3$statistic
prueba3$p.value
prueba3$conf.int


prueba3.1<- t.test(comp1,comp2,alternative="two.sided",mu=0,paired=T,conf.level=0.95)
prueba3.1$statistic
prueba3.1$p.value
prueba3.1$conf.int


#****************************************************************************************

## Ejemplo 5: En un estudio que se lleva a cabo sobre el desarrollo de la micorriza 
# (una relacion simbiotica entre las raices de los arboles y un hongo en la 
# cual se transfieren minerales del hongo a los arboles y azucares de los 
# arboles a los hongos), se cultivaron en un invernadero 20 robles rojos 
# que fueron expuestos al hongo. Todos los arboles se plantaron en el mismo
# suelo y recibieron la misma cantidad de sol y agua. La mitad de ellos no
# recibio Nitrogeno al momento de plantarlos (control) y la otra mitad recibio 
# cierta dosis de Nitrogeno. Despues de 140 dias se registraron los 
# siguientes pesos de los tallos (en gramos):

con<- c(0.26,0.43,0.47,0.49,0.52,0.75,0.79,0.86,0.62,0.46)
sin<- c(0.32,0.53,0.28,0.37,0.47,0.43,0.36,0.42,0.38,0.43)

# Analicemos estas muestras por separado:

boxplot(con,sin,horizontal = T,main="Peso de los tallos",
        names=c("Con nitrogeno","Sin nitrogeno"))


# Claramente se observan diferencias entre los pesos medios de los tallos
# con/sin nitrogeno.

# De hecho, pareciera que la presencia de Nitrogeno hace que los tallos
# pesen en promedio mas; por lo que la prueba de hipotesis es unilateral.

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# H0: muCON = muSIN   vs    Ha: muCON >  muSIN

# Prueba de hipotesis PARAMETRICA para medias con varianza poblacional desconocida.
# SUPUESTO: normalidad y muestras dependientes

prueba.mico<- t.test(con,sin,alternative="greater",mu=0,paired=T,conf.level = 0.95)
prueba.mico$statistic
prueba.mico$p.value

# Como el valor-p=0.0114 es pequeno, se rechaza H0. Por lo tanto si hay diferencia
# entre el suministrar o no nitrogeno al suelo cuando se plantan los arboles.
# Es decir, el nitrogeno ayuda a que el peso medio de los tallos sea mayor.

# Prueba de hipotesis PARAMETRICA para medias con varianza poblacional desconocida.
# SUPUESTO: normalidad y muestras independientes 
# OJO: los diagramas de caja-brazos muestran una diferencia en variabilidad.
# Lo verificamos mediante una prueba de hipotesis:

vartest.mico<- var.test(con,sin,ratio=1,alternative="two.sided",conf.level = 0.98)
vartest.mico$conf.int

# En efecto, las varianzas NO son estadisticamente iguales.
# Prueba de hipotesis PARAMETRICA para medias con varianza poblacional desconocida.
# SUPUESTO: normalidad,muestras independientes y varianzas distintas

prueba.mico2<- t.test(con,sin,alternative="greater",mu=0,paired=F,var.equal=F, conf.level=0.92)
prueba.mico2$statistic
prueba.mico2$p.value

# El valor.p=0.0114 sugiere que se rechaza H0 y por tanto hay diferencia.

# Si se estima un intervalo de confianza al 95% para la diferencia 
# de medias:

ic.mico<- t.test(con,sin,alternative="two.sided",mu=0,paired=T,conf.level=0.95)
ic.mico$conf.int

ic.mico2<- t.test(con,sin,alternative="two.sided",mu=0,paired=F,conf.level=0.95)
ic.mico2$conf.int



