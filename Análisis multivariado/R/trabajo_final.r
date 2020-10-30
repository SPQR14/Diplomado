canciones = read.csv(file = "datos_musica.csv")
attach(canciones)
library(reshape)


z = cor(canciones[,2:28])

z[z == 1] = NA
z[abs(z) < 0.5] = NA
z = na.omit(melt(z))
z = z[order(-abs(z$value)),]

zdf = as.data.frame(z)
zdf

pca_canciones = princomp(x=canciones[,2:28], cor = T)
pca_canciones

summary(pca_canciones)

prcomp(canciones[,2:28])

screeplot(princomp(canciones[,2:28]))



