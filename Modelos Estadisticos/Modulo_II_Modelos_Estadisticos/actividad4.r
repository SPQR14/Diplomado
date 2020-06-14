# Pico Lara Alberto Isaac
# 20/05/2020
# Comparación y asociación entre variables

# Cualitativa : Sexo
# Cuantitativa : Salario

datos1 = read.table("D:/Desarrollo/Diplomado/Modelos Estadisticos/Datos/Empleados.txt",header=TRUE,row.names=1)
attach(datos1)

mujer<- datos1[sexo=="1",]  # separamos por sexo
hombre<- datos1[sexo=="2",]

par(mfrow=c(1,2))
boxplot(mujer$salario,horizontal=T,main="Mujeres",xlab="Salario")
text(x = boxplot.stats(mujer$salario)$stats, labels = boxplot.stats(mujer$salario)$stats, y = 1.3)
boxplot(hombre$salario,horizontal=T,main="Hombres",xlab="Salario")
text(x = boxplot.stats(hombre$salario)$stats, labels = boxplot.stats(hombre$salario)$stats, y = 1.3)

coef_var_mujer = sd(mujer$salario)/mean(mujer$salario); coef_var_mujer
coef_var_hombre = sd(hombre$salario)/mean(hombre$salario) ; coef_var_hombre

res_apt<-matrix(fivenum(salario),nrow=1,ncol=5)    # sin separar por sexo fivenum -> resumir 5 números descriptivos
colnames(res_apt)<- c("Min","q1","q2","q3","Max")
rownames(res_apt)<- c("")
res_apt


round(cor(datos1[c(1,2,4:6,8,9)]),digits = 3)