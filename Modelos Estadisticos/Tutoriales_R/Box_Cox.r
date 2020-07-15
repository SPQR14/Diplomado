
# Diplomado en Tecnicas Estadisticas y Mineria de Datos
# MODULO II: Modelos Estadisticos
# Prof. Elizabeth Martinez
# FES-Acatlan, UNAM
# TUTORIAL 7: TRANSFORMACION BOX-COX

## Recordemos que uno de los supuestos importantes para la estimacion
## de intervalos de confianza y hacer el contraste de hipotesis (parametrico)
## es el de normalidad.

## Veamos este conjunto de datos

datos<- read.table("./Datos/datos_simulados.dat")  # cambiar la ruta
attach(datos)
str(datos)

y<- datos$V1  # renombramos la primera columna por facilidad
hist(y)  # claramente no sigue un perfil "normal"

# Grafica cuantil-cuantil
qqnorm(y,pch=16);qqline(y,col="red",lwd=2)

# Se desvia de la "normalidad". La linea roja corresponde a la "normal" teorica,
# mientras que los datos son los puntos negros. 

## ¿Que pasa si NO hay normalidad en la poblacion a partir de la cual
## se extrae la muestra?

# TRANSFORMACION BOX-COX: lambda=0, log(x)
#           lambda!=0, (x^{lambda}-1)/lambda


install.packages('car'); library(car)
lambda = seq(-2.5,2.5,0.5)  # proponemos un conjunto de valores para lambda

# Aplicamos la funcion "bcPower" del paquete car
tmp=sapply(lambda,function(x) bcPower(y,lambda=x))
par(mfrow=c(2,2))  # matriz de graficas
sapply(1:ncol(tmp), function(i) hist(tmp[,i],main=lambda[i]))

# Muestra los histogramas de los datos "nuevos", o sea transformados para
# cada valor de lambda propuesto, la funcion "bcPower" hace el calculo
# directamente.

# Otra manera es hacer los calculos de manera manual:
par(mfrow=c(1,1))
t1<- log(y); hist(t1)
t2<- y^(-2.5)/(-2.5); hist(t2)
t3<- y^(-1.5)/(-1.5); hist(t3)
t4<- y^(-0.5)/(-0.5); hist(t4)
t5<- y^(0.5)/(0.5); hist(t5)
t6<- y^(1.5)/(1.5); hist(t6)
t7<- y^(2.5)/(2.5); hist(t7)

# ALTERNATIVA PARA TRANSFORMACIONES BOX-COX EN R:

install.packages("forecast");library(forecast)
lambda = BoxCox.lambda(y,method="loglik",lower=-100,upper=100);lambda 
lambda2 = BoxCox.lambda(y,method="guerrero",lower=-100,upper=100);lambda2 

# Hay dos metodos implementados para hacer la transformacion Box-Cox, uno
# utiliza la funcion de verosimilitud y el otro es una variante en la potencia
# de la transformacion ("guerrero"). En ambos lo que se busca es el valor
# optimo de lambda.
# En este ejemplo, usamos el valor de lambda optimo obtenido por el metodo
# de verosimilitud.

trans.y = BoxCox(y, lambda)  # aplica la lambda optima a los datos, transformandolos
hist(trans.y)  # WOW!

# Verificamos normalidad:
# Ho: los datos se distribuyen como normal

lillie.test(trans.y)  # prueba de hipotesis de bondad de ajuste llamada Lilliefors

par(mfrow=c(1,2))
hist(y)  # variable original
hist(trans.y)  # variable transformada

qqnorm(trans.y,pch=16);qqline(trans.y,col="red",lwd=2)

# La distribucion de "y" ya es el de una normal, gracias a la transformacion
# de Box-Cox. 

# Ahora si hacemos la inferencia estadistica que conocemos porque el supuesto
# de normalidad en la poblacion ya fue verificado (y "corregido").


