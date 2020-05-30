
# Diplomado en Tecnicas Estadisticas y Mineria de Datos
# MODULO II: Modelos Estadisticos
# Prof. Elizabeth Martinez
# FES-Acatlan, UNAM
# TUTORIAL 3: VARIABLES ALEATORIAS UNIDIMENSIONALES

#****************************************************************************************
#********************** PROPIEDADES BASICAS DE VARIABLES DISCRETAS **********************
#****************************************************************************************

#%%%%%%%%%%%%%%%%%% PROBLEMA 1 #%%%%%%%%%%%%%%%%%% 
# X= # semaforos encontrados en una muestra de 10 imagenes

x<- c(0,1,2,3,4)
px<- c(0.41,0.37,0.16,0.05,0.01)
Fx<- cumsum(px); Fx

# a) Grafica de px (f.m.p)

plot(x,px,type="h",ylab="Función de masa de probabilidad, px",col="red")  # tipo de linea: vertical


# b) Grafica de Fx 
plot(x,Fx,type="s",ylab="Función de probabilidad acumulada, Fx",col="blue")  # tipo de linea: escalon

# c) Calculo de probabilidades

# i) P(1< X < 3)

px[3]   # usando f.m.p, OJO: px[3] se refiere a la tercera posicion del arreglo px (y que corresponde a
# x=2)

# P(X<3)-P(X<=1)=P(X<=2)-P(X<=1)

Fx[3]-Fx[2]   # usando f.p.a (funcion de probabilidad acumulada)

# ii) P(X>3|X>2)=P(X>2 y X>3)/P(X>2)

# P(X>2 y X>3)= P(X=4)  # interseccion entre X=3,4 con X=4
numerador<- px[5]   # px[5] es la posicion de x=4

# P(X>2)=P(X=3)+P(X=4)
denominador<- px[4]+px[5]

numerador/denominador   # resultado de la probabilidad condicional

# d) Valor esperado de X

EX<- function(x,fx){
  sum(x*fx)
}

EX(x,px)

#%%%%%%%%%%%%%%%%%% PROBLEMA 2 #%%%%%%%%%%%%%%%%%%

# X= cantidad de la perdida de unidades

# a) funcion masa de probabilidad (f.m.p)

fmp1<- function(x){
  c= 2/49
  if (x==0) {    # probabilidad cuando x=0
    f=0.9
  } else if (x>0) {   # probabilidad cuando x>0
    f=c/x
  }  else {
    f=0       # probabilidad cuando x<0
  }
  f        # guarda el valor de la funcion
}


x<- c(0,1,2,3,4,5,6)
px<- round(c(fmp1(0),fmp1(1),fmp1(2),fmp1(3),fmp1(4),fmp1(5),fmp1(6)),digits=3)   # aplicamos a cada valor de x
(tabla_prob<- cbind(x,px))   # tabla de probabilidad de la variable aleatoria X

sum(px)   # suma de las probabilidades es 1


# b) Desviacion estandar (necesitamos calcular media y varianza)

EX(x,px)   # usamos la funcion del problema anterior, evaluando en los nuevos valores de x y px

# Varianza de X

VX<- function(x,fx){
  EX2= sum(x^{2}*fx)
  var= EX2-(EX(x,fx))^{2}
  var
}

sx<- sqrt(VX(x,px)); sx   # desviacion estandar de X

# c) Moda y mediana: usaremos las graficas de px y Fx

# Moda: Grafica de px

plot(x,px,type="h",ylab="Función de masa de probabilidad, fx",col="red",main="X (cantidad de la perdida de unidades)")  # tipo de linea: vertical

tabla_prob[which.max(tabla_prob[,2])]   # primero selecciona el valor maximo de la segunda columna (px) y
# luego al aplicar tabla_prob[ ] muestra el valor de x para el cual se
# cumple el maximo, es decir, x=0

# Mediana: Grafica de Fx 

Fx<- cumsum(px)
plot(x,Fx,type="s",ylab="Función de probabilidad acumulada, Fx",col="blue",
     main="X (cantidad de la perdida de unidades)",ylim=c(0,1))  # tipo de linea: escalon
abline(h=0.5,col="red")

# d) Haciendo la comparacion de:

media<- EX(x,px)
moda<- 0
mediana<- 0

# Se concluye que la ASIMETRIA ES HACIA LA DERECHA. Ademas la grafica px lo muestra claramente.

#%%% EXTRA: Calculo del coeficiente de asimetria (ahora para variables aleatorias)

gamma1<- function(x,fx){
  EX1<- sum(x^{1}*fx)    # tambien se puede usar EX(x,fx) que es la funcion escrita previamente
  EX2<- sum(x^{2}*fx)
  EX3<- sum(x^{3}*fx)
  s= sqrt(EX2-(EX1^2))   # tambien se puede usar sqrt(VX(x,fx)) que es la funcion escrita previamente
  j1= (1/s^3)*(EX3-3*EX1*EX2+2*(EX1^3))  # coeficiente de asimetria
  j1
}

gamma1(x,px)  # sale j1>0 por lo que, en efecto, la distribucion de X tiene ASIMETRIA HACIA LA DERECHA

#****************************************************************************************
#********************** PROPIEDADES BASICAS DE VARIABLES CONTINUAS **********************
#****************************************************************************************

#%%%%%%%%%%%%%%%%%% PROBLEMA 1 #%%%%%%%%%%%%%%%%%% 

# T=tiempo (en unidades de 100 horas) que se usa una computadora mensualmente 

# a) Dibujar p.d.f. y c.d.f. 


ft1<- function(t) t
ft2<- function(t) 2-t

t1= seq(0,1,by=0.01)
t2= seq(1,2,by=0.01)
plot(t1,ft1(t1),type="l",xlim=c(0,2),xlab="Tiempo",ylab="f(t)")
lines(t2,ft2(t2))  # para poner otra grafica sobre la anterior

# b) Distribucion acumulada

Ft1<- function(t) t^2/2
Ft2<- function(t) (-t^2/2)+(2*t)-1

plot(t1,Ft1(t1),type="l",xlim=c(0,2),ylim=c(0,1))
lines(t2,Ft2(t2))   # para poner otra grafica sobre la anterior

# c) Calculo de probabilidades

# i) P(0.2 <= T <= 1.5)

int1= integrate(ft1,0.2,1)
int2= integrate(ft2,1,1.5)

str(int1)   # para ver los elementos que forman al objeto "int1"
int1$value+int2$value   # $ value es donde se guarda el resultado de la integral

# O bien, 

Ft2(1.5)-Ft1(0.2)

# ii) P(T<2|T<0.5)

integrate(ft1,0,0.5)$value/integrate(ft1,0,0.5)$value  # escribimos directamente el cociente
# ver ejercicios de clase

# O bien,

(Ft1(0.5)-Ft1(0))/Ft1(0.5)


# d) Valor esperado de T

ET1<- function(t)  t*ft1(t)
ET2<- function(t)  t*ft2(t)

ET<- integrate(ET1,0,1)$value+integrate(ET2,1,2)$value  
ET*100  # porque esta en unidades de 100 horas

#%%%%%%%%%%%%%%%%%% PROBLEMA 2 #%%%%%%%%%%%%%%%%%% 

# T=tiempo (en unidades de 100 horas) que se usa una computadora mensualmente 

# a) Ver ejercicios de clase.
# k=2

# verificamos

fx<- function(x) 2*(1-x)

integrate(fx,0,1)  # en efecto, k=2 para que sea fx una funcion de probabilidad valida

x<- seq(0,1,0.1)
plot(x,fx(x),type="l",col="purple",lwd=2)

# b) desviacion estandar

EX<- function(x) x*fx(x)   # noten que esta funcion es EXCLUSIVA para la funcion fx=2(1-x)
# por ello no la declaramos como argumento
ex<- integrate(EX,0,1)$value   # resultado de la integral, es decir, el valor del promedio 

EX2<- function(x) x^2*fx(x)
ex2<- integrate(EX2,0,1)$value

sx<- sqrt(ex2-ex^2); sx   # desviacion estandar

# c) moda, mediana

# usamos la funcion de probabilidad acumulada
Fx<- function(x) 2*x-x^2    

plot(x,Fx(x),type="l")  # la graficamos para localizar a la mediana
abline(h=0.5,col=6)     # mediana esta en 0.5 de la escala vertical. Recordar que es el valor
# para el cual se acumula una probabilidad del 50%
locator()   

# La interseccion nos da: x=0.2970 y y=0.4965

# Asi que la mediana es 0.2970 aproximadamente

# Para la moda nos fijamos en el valor "x" donde la funcion toma el valor mas alto.
# Usamos la grafica

plot(x,fx(x),type="l",col="purple",lwd=2)

# Haciendo la comparacion de:

media<- ex
moda<- 0
mediana<- 0.2970

# Se concluye que la ASIMETRIA ES HACIA LA DERECHA. 

#%%% EXTRA: Calculo del coeficiente de asimetria (ahora para variables aleatorias)

EX3<- function(x) x^3*fx(x)
ex3<- integrate(EX3,0,1)$value


j1= (1/sx^3)*(ex3-3*ex*ex2+2*(ex^3))  # coeficiente de asimetria

# sale j1>0 por lo que, en efecto, la distribucion de X tiene ASIMETRIA HACIA LA DERECHA

# e) Costo esperado

EC<- function(a,b,c){
  a+b*ex+c*ex2
}

EC(10,20,4)





