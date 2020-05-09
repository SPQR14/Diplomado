# Diplomado en Tecnicas Estadisticas y Mineria de Datos
# MODULO II: Modelos Estadisticos
# Prof. Elizabeth Martinez
# FES-Acatlan, UNAM
# TUTORIAL 1: INTRODUCCION A R


# Librerias empleadas en este tutorial:

library(rgl)
library(scatterplot3d)
library(pracma)


# **********************************************************************
# **********************************************************************
# **********************************************************************

###### 1. Basicos de R

# R es un software estadistico GRATUITO. Tiene muchas aplicaciones entre ellas:
# 1. calculadora 
# 2. graficadora
# 3. analisis de datos mediante una gran variedad de herramientas estadisticas.
# 4. poderoso lenguaje de programacion

# Hasta Mayo 2020 existen 15636 paqueterias que incluyen funciones y herramientas diversas
# para resolver problemas en distintas areas.

# https://cran.r-project.org/web/packages/available_packages_by_name.html

# Iniciando una sesion en R
setwd("D:/Desarrollo/Diplomado/Modelos Estadisticos/Modulo_II_Modelos_Estadisticos")   	# fijar el directorio de trabajo

# Otra manera de escribir la ruta
#setwd(file.path("D:", "tu propia ruta de acceso"))  # si son varias carpetas, estas se separan por comas


getwd()   					# reporta el directorio de trabajo 
system("pwd")  				# reporta el directorio de trabajo (desde el sistema operativo)
citation()					# referencia biliografica para publicaciones



# Operaciones basicas EN R

# a) Aritmeticas: +,-,*,/,^, %% (mod), %/% (division entera)
# b) Logaritmos y exponenciales: log, log10, log2, log(x,b) (logaritmo de x a la base b),
#                                log1p(x) (calcula el log(1+x) exactamente para |x|<<1)
#                                exp

# c) Funciones trigonometricas: sin(x),cos(x),tan(x),asin(x),acos(x),atan(x) donde
#                               los angulos estan en radianes; x e y son escalares 
#                               o vectores reales o complejos.
#                               Con el paquete "pracma" se obtienen cot(x),csc(x),
#                               sec(x),acot(x),acsc(x),asec(x)
# d) Funciones hiperbolicas: sinh(x),cosh(x),tanh(x),asinh(x),acosh(x),atanh(x). 
#                            Analogamente con "pracma" se obtienen las otras.
# e) Miscelaneas: sqrt(x),abs(x)
# f) Otras funciones especiales disponibles en los paquetes: "orthopolynom", "pracma",
#                                                            "hypergeo", "specfun"


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1. Creando objetos

b <- 14.21993   # objeto b que consiste del valor 14.21993
x <- b       # el valor "b" es asignado a "x"
x = b         # el valor "b" es asignado a "x"
b -> x       # el valor "b" es asignado a "x"


ls()  			# lista objetos en el directorio de trabajo
rm(b)
rm(x)       # elimina objeto

# Tipos de objetos: 
# Null (vacio): NULL
# Logical (Booleano): TRUE, FALSE, T, F
# Numerical (real): 3,-5,pi, 4e-3
# Complex (complejo): 4+2i, 6i
# Character (cadena de caracteres): 'hola', "P"

c = 3+2i

# 1.1 Revisar tipo de objeto
is.null()
is.logical()
is.numeric()
is.complex()
is.character()

# 1.2 Conversiones de objetos

as.logical()
as.numeric()
as.complex()
as.character()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2. Numeros

w = 2+6;w
5*8
4-(3*9)+2.54
2/0
2/Inf
Inf/Inf

# R maneja 3 tipos de numeros: ordinarios, infinitos, NaN

# R tambien trabaja con numeros complejos

?complex
(1i)^2
(1+2i)-(3+4i)
(1+2i)*(3+4i)
(1+2i)/(3+4i)
Re(3+2i); Im(3+2i)
Mod(3+2i); Arg(3+2i)
Conj(3+2i)

# formula de Euler: exp(i*phi)=cos(phi)+i*sin(phi)   
exp(pi/7*1i)
cos(pi/7)+1i*sin(pi/7)


# 2.1 Constantes construidas en R

# a) pi
# b) LETRAS (el alfabeto romano completo en mayusculas)
# c) letras (el alfabeto romano completo en minusculas)
# d) month.abb (abreviaturas de 3 letras para los meses, en ingles)
# e) month.name (nombres de los meses en ingles)


# 2.2 Redondeando

Arg(3+2i)
# options(digits=3)   # toda la sesion activa utilizara esta opcion
round(1234.56789,digits=3)
signif(1234.56789,digits=3)   # redondea al numero especificado de digitos significativos

# Otras funciones: ceiling(x),floor(x),trunc(x)


# 2.3 Mejorando la presentacion de los numeros

(x=runif(9)) #generador de números pseudoaleatorios de la distribucion uniforme (cantidad de números)
(x=x+123456) #se lo suma a todo
formatC(x,digits=3,format="f")
formatC(x,digits=3,format="E")
(0.7/0.1)-7  # oops!
zapsmall(0.7/0.1)-7  # mejor!

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3. Variables 

x= 5; y=-4

x + y
x - 2*y
x^2 + 1/y

z-2*x    # cuidado con no definir ciertas variables antes!
X + Y    # cuidado con las mayusculas!

x = 2*x  # se "actualiza" el valor de x

sec.=1; min_eq=60*sec.; hr.=60*min_eq    # recomendacion: no dejar espacios en los nombres
dia=24*hr.;yr=365.25*dia; siglo.100= 100*yr

# para eliminar variables: rm(nombre de la variable); para eliminar TODO lo que 
# se tenga: rm(list=ls())

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4. Funciones estandar

sin(x)
cos(0)
sin(pi) #pi es una constante ya conocida para R
tan(pi/2)

exp(1)
log(3)
log(-3)   # cual es la base del logaritmo?
log(0)
log(x-y)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 5. Pidiendo ayuda

?log
Gamma(2)  # Oops! 
help.search("Gamma")   # en caso de no saber el nombre exacto de la funcion
help.start()     

RSiteSearch("mean weighted")  # para soluciones diversas

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 6. Construccion de funciones

#+++++
f = function(x){
  x/(1-x)
} 

f(2)
y = 4
f(y)
f(2*y)

#+++++
g = function(x,y){
  (x+2*y)/3 # (x+2y)/3
}     
g(1,2)
g(2,1)

#+++++
hipotenusa<- function(cateto1, cateto2){
  hip= sqrt(cateto1^2+cateto2^2)
  cat("Cateto 1=", cateto1,",", "Cateto 2=", cateto2,".", "Por pitagorazo, la hipotenusa es:",hip) 
}

hipotenusa(2,7)

#+++++
potencia <- function(x, y) {
  # función que calcula x elevado a y
  res = x^y
  paste(x,"elevado a la potencia de", y, "es", res)
}

potencia(2,-1)
potencia(-2,4)

#+++++
valor.absoluto <- function(x) {
  # valor absoluto de x
  if(x<0){-x}  # si el valor de x es negativo, nos devuelve su opuesto -x
  else 
    x
}

valor.absoluto(-2)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 7. Graficas de funciones

mifun<- function(x){
  h= (x^2)*(5-x)*sin(2*x)^2
  return(h)
}

x<- seq(0,5,by=0.0001)
plot(x,mifun(x),type="l",col="purple",lwd=2,main="Mi función",xlab="x",ylab="f(x)")
locator()


# Ahora una funcion en 3D

mifun3D <- function(x, y){
  sqrt(x^2+y^2)
}

x <- y <- seq(-1, 1, length= 50)
z <- outer(x, y, mifun3D)   # evalua la funcion usando los valores de "x", "y"
persp(x, y, z,
      main="Gráfica función 3D",
      zlab = "z",
      theta = 45, phi = 15,
      col = "yellow",shade=0.5)


library(rgl)
rgl.open() # abre una ventana nueva de rgl
persp3d(x,y,z,col="yellow")


# Otra funcion en 3D
library(scatterplot3d)
Z<- seq(0, 5*pi, length.out=1000)
X<- Z*cos(Z)
Y<- Z*sin(Z)
scatterplot3d(X, Y, Z, col.axis = "blue",col.grid = "lightblue", main = "Helice",type="l",color="red")


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Ejercicio (entregar en clase) 
# 1)Escribe una funcion y nombrala como gustes que calcule: 80y^2*exp(-x^2-0.3y^2)
# con -2.1 = x = 2.1;  -6 = y = 6
# Incluye un texto usando "paste" o "cat" para mostrar el resultado de tu evaluacion.
# 2) Evalua en el caso de x=-2, y=0.4
# 3) Grafica esta funcion

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 8. Vectores

x = c(1,2,4,56)
x

y = c(x,c(-1,5),x)
length(x)
length(y)

x = 1:20
4:-10

y=seq(2,5,0.3)
y

rep(2,5)

1:100   # que significan los numeros dentro de [ ]?

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Ejercicio: Crea el siguiente vector y guardalo con el nombre "vec": 
# -20,-18,-16,-14,-12,-10,0,1,1.1,1.2,...,1.9, 2, 5, 5.2,5.4,...,9.8,10,11,13

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# 8.1 Operaciones con vectores

x = 1:5
x^2
x+1
2*x
sin(x)
exp(sqrt(x))

x = c(1,2,-3,0) 
y = c(0,3,4,0)
# c: operador concatenar: la entrada es un conjunto de numeros y la salida es un vector
x+y
x*y
x/y
2*x-3*y

# Producto escalar y vectorial

prod.esc=function(u,v) as.numeric(u%*%v)  # as.numeric esta forzando a que sea un escalar
vecnorm=function(v) sqrt(prod.esc(v,v))
prod.vec=function(u,v){
  c(u[2]*v[3]-u[3]*v[2],u[3]*v[1]-u[1]*v[3],u[1]*v[2]-u[2]*v[1])
}


u1=c(1,2,3)
u2=c(4,5,6)

prod.esc(u1,u2); vecnorm(u1);prod.vec(u1,u2)

# O bien,
install.packages("pracma")
library(pracma)

dot(u1,u2); cross(u1,u2)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#  Ejercicio: Investiga lo que hacen los siguientes:
val = c(2,1,-4,4,56,-4,2)
sum(val)
mean(val)
min(val)
max(val)
range(val)
which.min(val)
which.max(val)
summary(val)
cumsum(val)
cumprod(val)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

x = c(2,4,-1,0,2,0.4,-8)
x[1]
x[2]+x[4]
i = 3
x[i]
x[i-1]
x[4]
x[-1]   # elimina el primer valor del vector x
x[c(-1,-5)]  # elimina el primer valor y el quinto del vector x

x = 3:10
x[1:4]
x[c(2,4,1)]

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#  Ejercicio: Que pasa?
x = c(10,3,4,1); x
ind = c(3,2,4,1);ind # una permutacion de 1,2,3,4
x[ind]  # util para reordenar vectores. En este caso a x lo acomoda segun las posiciones
# dada por "ind"
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#  Ejercicio: Ejecuta lo siguiente e interpreta los signos negativos dentro de [].
x = 3:10; x
x[-1]
x[c(1,3)] 
x[c(-1,-3)]
x[-c(1,3)]    # equivalente a lo anterior

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

x = c(100,2,200,4);x
x[x>50]
sum(x[x>50])
x>50; sum(x>50)


val = c(1,30,10,24,24,30,10,45)

val[sum(val >= 10 & val <= 40)]   # sum(val >= 10 & val <= 40)=6, es decir, val[6] y da la posicion 6
sum(val > 40 | val < 10) # | significa "o"
sum(val == 30) # == significa "igual a"
sum(val != 24) # != significa "diferente de"

mean(x>50); mean(x[x>50])

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#  Ejercicio: Ejecuta e interpreta el resultado
x = c(100,2,200,4)
sum(x>=4)
mean(x!=2)
x==100
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# 8.2 Ordenar vectores

x = c(2,3,4,5,3,1)
y = sort(x,decreasing=F)
y # ordenado
x # sin cambio

x = c(2,3,4,5,3,1)
y = c(3,4,1,3,8,9)
(ord = order(x))   # posicion de los numeros del vector x del mas chico al mayor

x[ord] # igual que sort(x)
y[ord] # y ordenado de acuerdo con x

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 9. Matrices

# Construimos una matriz de 3x3
A<-matrix(c(1,17,22,14,59,6,22,6,95),nrow=3,ncol=3,byrow=T)

B<-matrix(c(1,17,22,14,59,6,22,6,95),nrow=3,ncol=3,byrow=F)

B

det(A[1:2,1:2])

A[1,1]

# Calcular eigenvectores y eigenvalores 
eigen(A)

# Calcula la matriz inversa
solve(A)

# Transpuesta
t(A)

# crea una matriz identidad de n x n (aqui n = 3)
id.matrix<-diag(rep(1,3))

# crea una matriz cero de n x p (aqui, n = 4, p = 2)
matrix(0,nrow=4,ncol=2)


# Crea una segunda matriz de 3x3

C<-matrix(c(7,10,2,10,5,3,2,3,8),nrow=3,ncol=3,byrow=T)


# Sumar(restar) dos matrices

A+C
B-C


# Multiplica dos matrices

A%*%C

# Multiplica entrada por entrada

A*C

# Nota 1: Tener cuidado en

D<-matrix(c(3,7,1,9,6,5,1,2),nrow=4,ncol=2); D
E<-matrix(c(-2,1,0,-4),nrow=2,ncol=2); E

D%*%E

M<-matrix(c(3,7,1,9,6,5,1,2),nrow=4,ncol=2); M
N<-matrix(c(-2,1,0,-4,8,13,20,5),nrow=2,ncol=4); N

M%*%N

# Nota 2: Se requiere del simbolo % !!!

# Columnas y renglones

A = matrix(c(1,3,2,4),ncol=2); A
sin(A)

apply(A,2,sum)   # columnas
apply(A,1,mean)  # renglones

# Creando una tabla mediante matrices 
probs=c(0.45,0.05,0.01,0.48,0.70,0.50,0.07,0.25,0.49)
P=matrix(probs,nrow=3,ncol=3)
rownames(P)<-colnames(P)<-c("inferior","medio","superior")
P      
rowSums(P)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 10. Creacion de listas

x = list(nombre="UNAM", tipo="publica", campus=c("CU","Acatlan","Iztacala","Aragon"),
         nivel=c("licenciatura", "maestria", "doctorado"), estudiantes=c(112576,28018,5416))
x
str(x)
names(x)
x$nombre
x$tip    # abreviaturas OK
x$camp
x[2]
x$c[2]   # segunda entrada de la variable CAMPUS
x$c[c(1,3)]
x$es 

# Listas que se obtienen despues de aplicar alguna funcion

f = function(x) list(len=length(x),total=sum(x),mean=mean(x))

dat = 1:10
result = f(dat)
names(result)
result$len
result$tot
result$mean

# 10.1 Data-frames (listas especiales con componentes de la misma longitud pero con "modos"
# potencialmente distintos)

vec1<- 1:5    # numeric
vec2<- c("a","b","c","c","b")   # character

df<- data.frame(vec1,vec2)

str(df)
df$vec1
df$vec2[3]

matriz<- data.matrix(df)