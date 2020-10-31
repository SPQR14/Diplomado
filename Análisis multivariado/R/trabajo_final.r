canciones = read.csv(file = "datos_musica.csv")
attach(canciones)
library(reshape)

#Análisis de componentes principales
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

nuevo_df = princomp(canciones[,2:28])$scores[,1:15]

df_pca = as.data.frame(nuevo_df)

df_pca

df_pca = data.frame(df_pca[,1:15], genero = as.vector(canciones$genero))

df_pca = data.frame(nombre = as.vector(canciones$archivo), df_pca[,1:16])


pairs(df_pca[,2:15], col = as.numeric(df_pca$genero))






