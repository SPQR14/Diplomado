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
lluvias= read.table("D:/Desarrollo/Diplomado/Modelos Estadisticos/Datos/lluvias.dat",sep="\t",row.names = 1,header=T)
attach(lluvias)
y= Precipitacion


hist(y,breaks=seq(0,2400,300),main= "Precipitacion anual en Mexico (2017)",
     xlab="Precipitacion (mm)")

plot(seq(0,30,0.1),dweibull(seq(0,30,0.1),shape=2,scale=2),ty="l",xlab="x",ylab="f(x)")

equis_barra = mean(y);equis_barra
ese = var(y);ese

factor1 = ese/equis_barra^2;factor1

plot(seq(0,5,0.5),  factor1 - ( gamma(1+(2/seq(0,5, 0.5)))/gamma(1+(1/seq(0,5,0.5)))^2 ) + 1, ty = "l")
locator()#con locator se halla un alfa aprox. 1.934, 1.935
abline(h=0,v = 1.935, col="red",lty=3)

alfa_gorro = 1.935
beta_gorro = equis_barra/gamma(1+(1/alfa_gorro));beta_gorro



