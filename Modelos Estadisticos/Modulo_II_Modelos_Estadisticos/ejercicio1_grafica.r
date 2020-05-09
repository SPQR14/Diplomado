# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Ejercicio (entregar en clase) 
# 1)Escribe una funcion y nombrala como gustes que calcule: 80y^2*exp(-x^2-0.3y^2)
# con -2.1 = x = 2.1;  -6 = y = 6
# Incluye un texto usando "paste" o "cat" para mostrar el resultado de tu evaluacion.
# 2) Evalua en el caso de x=-2, y=0.4
# 3) Grafica esta funcion

##Equipo:
# Alberto Isaac Pico Lara
# Diana Angélica Martínez Hernández

library(rgl)

ejercicio1 = function(x,y){
  return(80 * y ^ 2 * exp(-x ^  2 - 0.3 * y ^ 2))
}

cat("x = ", -2, "y = ", 0.4, "El resultado de la funcion ejerciocio1 es: ", ejercicio1(-2, 0.4))

x <- seq(-2.1, 2.1, length = 50)
y <- seq(-6, 6, length = 50)

z = outer(x, y, ejercicio1)

persp(x, y, z,
      main="Ejercicio 1",
      zlab = "z",
      theta = 45, phi = 20,
      col = "green",shade=0.5)

rgl.open()
persp3d(x,y,z,col="blue", shade = 0.5)

