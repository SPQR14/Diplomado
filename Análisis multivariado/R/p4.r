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


estados = data.frame(estados[,1:7], grupo = as.vector(clusters_estados$cluster))

lda_estados = lda(grupo~.,estados)
lda_estados

attributes(lda_estados)

lda_estados$prior

predict(lda_estados, estados[1:32,])$class

table(estados[,8], predict(lda_estados, estados[1:32,])$class)

proy_estados = predict(lda_estados, estados[,1:7])$x

plot(lda_estados)


proy_estados = predict(lda_estados, estados[,1:7])$x
plot(proy_estados, type="n")
text(proy_estados, labels = (estados$grupo), col = as.numeric(predict(lda_estados, estados[,1:7])$class))

lda_pred = predict(lda_estados, estados[,2:7]);lda_pred$posterior

qda_estados = qda(grupo~.,estados)

qda_estados



