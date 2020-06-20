#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Ejercicio para entregar en SUAYED:

# Considera el modelo de probabilidad Weibull (ver notas de clase).
# a) Escribe la funcion de (log)verosimilitud para los datos de lluvias 
# usando el modelo Weibull.
# b) Estima los parametros del modelo usando alguno de los optimizadores
# discutidos en clase. Como valores iniciales considera las estimaciones
# de momentos (ver notas de clase).
# c) Grafica el histograma de los datos de lluvia y ajusta el modelo Weibull
# con los parametros estimados en b).
# d) Compara tus ajustes con los obtenidos mediante la funcion "fitdistr".
# e) Utiliza el criterio AIC o BIC para seleccionar entre el modelo "gamma"
# y el modelo "Weibull". ¿Cual es el modelo preferido para explicar el
# comportamiento de las lluvias en Mexico?

# Entregar un reporte en donde escribas el codigo y presentes los resultados
# graficos.

library(MASS)

lluvias= read.table("D:/Desarrollo/Diplomado/Modelos Estadisticos/Datos/lluvias.dat",sep="\t",row.names = 1,header=T)
attach(lluvias)
y = Precipitacion

hist(y,breaks=seq(0,2400,300),main= "Precipitacion anual en Mexico (2017)",
     xlab="Precipitacion (mm)", col = heat.colors(14))

plot(seq(0,30,0.01),dweibull(seq(0,30,0.01),shape=2,scale=2),ty="l",xlab="x",ylab="f(x)")
title("Curva de la función de distribución Weibull")

# a) Escribe la funcion de (log)verosimilitud para los datos de lluvias 
# usando el modelo Weibull.

log_ver_wei <- function(theta) { 
  l= sum(dweibull(y,theta[1],theta[2],log=TRUE))
  return(-l)
}

# b) Estima los parametros del modelo usando alguno de los optimizadores
# discutidos en clase. Como valores iniciales considera las estimaciones
# de momentos (ver notas de clase).
equis_barra = mean(y);equis_barra
ese = var(y);ese
factor1 = ese/equis_barra^2;factor1


plot(seq(0,5,0.5),  factor1 - ( gamma(1+(2/seq(0,5, 0.5)))/gamma(1+(1/seq(0,5,0.5)))^2 ) + 1, ty = "l", main = "Aproximación por
     método gráfco del parámetro alfa = 1.975")
#locator() #con locator() se halla un alfa aprox. 1.975
abline(h=0,v = 1.975, col="red",lty=3)

alfa_goro_momentos = 1.975;alfa_gorro_momentos
beta_gorro_momentos = equis_barra/gamma(1+(1/alfa_gorro_momentos));beta_gorro_momentos

ajuste_2<- optim(c(alfa_goro_momentos,beta_gorro_momentos),log_ver_wei);ajuste_2
alfa_log_ver = ajuste_2$par[1]; alfa_log_ver
beta_log_ver = ajuste_2$par[2]; beta_log_ver
# c) Grafica el histograma de los datos de lluvia y ajusta el modelo Weibull
# con los parametros estimados en b).
hist(y, pch=20, prob=TRUE,main="")
curve(dweibull(x, alfa_log_ver,beta_log_ver),col="blue", lwd=2, add=T)

# d) Compara tus ajustes con los obtenidos mediante la funcion "fitdistr".
ajuste_3 = fitdistr(y,densfun = "weibull");ajuste_3
str(ajuste_3)

# e) Utiliza el criterio AIC o BIC para seleccionar entre el modelo "gamma"
# y el modelo "Weibull". ¿Cual es el modelo preferido para explicar el
# comportamiento de las lluvias en Mexico?
k=2   # numero de parametros a estimar

AIC_gamma= 482.3427#2*k-(2*lluvias_mle1$loglik); AIC_gamma
BIC_gamma= 485.2742#log(n)*k-(2*lluvias_mle1$loglik); BIC_gamma
n = length(y)
AIC_weibull = 2*k-(2*ajuste_3$loglik);AIC_weibull
BIC_weibull = log(n)*k-(2*ajuste_3$loglik);BIC_weibull


