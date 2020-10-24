library(MASS)
data(iris)

dim(iris)

datos = data.frame(iris[,1:4], clase=as.vector(iris[,5]))

lda(clase~.,datos)

lda_iris = lda(clase~.,datos)

attributes(lda_iris)

predict(lda_iris, iris[1:100,])$class

table(iris[,5],predict(lda_iris,iris[,1:4])$class)

especies = iris[,5]
lda(iris[,1:4],especies)

iris_proy = predict(lda_iris, iris[,1:4])$x

plot(iris_proy)

iris_proy = predict(lda_iris, iris[,1:4])$x
plot(iris_proy, type = "n")

text(iris_proy ,labels=as.numeric(iris$Species),col=as.numeric(predict(lda_iris,iris[,1:4])$class))

