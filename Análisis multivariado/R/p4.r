library(readxl)
library(MASS)

estados = read_excel("estados.xlsx")
attach(estados)

matriz_estados = as.matrix(cbind(estados$Empleado, estados$`Jorn/Peón`, estados$`Trab. particular`, estados$Patrón, estados$`Trabaj. Familiar`, estados$`No especifico`))

stars(matriz_estados, labels = estados$Estado)


clust = hclust(dist(matriz_estados))

plot(clust)

clusters_estados = kmeans(matriz_estados, 6)
clusters_estados

clusters_estados$cluster


estados_m = data.frame(estados[,1:7], clase = as.vector(clusters_estados$cluster))

lda_estados = lda(grupo~.estados_m)



