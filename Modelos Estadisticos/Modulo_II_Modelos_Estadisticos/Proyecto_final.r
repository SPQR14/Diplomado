### Proyecto final Modelado Estadístico ###
### Fecha inicio:            30/05/2020 ###
### Fecha fin:               16/06/2020 ###

#Usando la ruta absoluta del equipo en el que se programó este Script
#Nombre del archivo del conjunto de datos: Consumo_Gasolina_Autos_Ene_2018.xlsx


library(readxl)
library(utf8)
library(MASS)
library(normtest) ###REALIZA 5 PRUEBAS DE NORMALIDAD###
library(nortest) ###REALIZA 10 PRUEBAS DE NORMALIDAD###
library(stats)
library(agricolae)
library(moments)
library(PASWR)

if (!require('devtools')) install.packages('devtools')
devtools::install_github('fhernanb/stests', force=TRUE)
library(stests) #pruebas de varianza

#setwd("../Desarrollo/Diplomado/Modelos Estadisticos/Modulo_II_Modelos_Estadisticos/")

df = read_excel("../Datos/Datos_tratados.xlsx")
#df = read.csv(file = "../Datos/Consumo_Gasolina_Autos_Ene_2018.csv")

dim(df)
names(df)
str(df)

attach(df)

### Variables cuantitativas ###

#Tablas y gráficos por transmisión
tabla_transmision_fa = table(Trans_resumen);tabla_transmision_fa
tabla_transmision_fr = prop.table(tabla_transmision_fa);tabla_transmision_fr
barplot(tabla_transmision_fr, col=3:4, ylim=c(0,0.8))
title("Diagrama de barras - Transmisión \n (Frecuencias relativas)")


#Tablas y graficos por combustible
tabla_combustible_fa = table(df$Comb.);tabla_combustible_fa
table_combustible_fr = prop.table(tabla_combustible_fa); table_combustible_fr
barplot(table_combustible_fr, col=3:4, ylim=c(0,1))
title("Diagrama de barras - Combustible \n (Frecuencias relativas)")

#tablas y gráficos por marca
tabla_marcas_fa = table(df$Marca);tabla_marcas_fa
tabla_marcas_fr = prop.table(tabla_marcas_fa);tabla_marcas_fr
par(mar=c(11,4,4,4))
barplot(tabla_marcas_fr, col=heat.colors(43), ylim=c(0,0.1), horiz = FALSE, las = 2)
title("Diagrama de barras - Marca \n (Frecuencias relativas)")

#tablas y gráficos por cilindros
tabla_cilindros_fa = table(df$Cilindros);tabla_cilindros_fa
tabla_cilindros_fr = prop.table(tabla_cilindros_fa);tabla_cilindros_fr
par(las=1)
barplot(tabla_cilindros_fr, col=heat.colors(43), ylim=c(0,0.6))
title("Diagrama de barras - Número de cilindros \n (Frecuencias relativas)")

#tablas y graficos por categoría
tabla_cat_fa  = table(df$Categoría); tabla_cat_fa
tabla_cat_fr = prop.table(tabla_cat_fa);tabla_cat_fr
par(mar=c(17,4,4,4))
barplot(tabla_cat_fr, col=heat.colors(43), ylim=c(0,0.35), horiz = FALSE, las = 2)
title("Diagrama de barras - Categoría \n (Frecuencias relativas)")

#tablas y graficos por año
tabla_modelo_fa = table(df$Modelo) ; tabla_modelo_fa 
tabla_modelo_fr = prop.table(tabla_modelo_fa); tabla_modelo_fr
par(mar=c(3,4,4,4))
barplot(tabla_modelo_fr, col = heat.colors(6), ylim = c(0,0.2))
title("Diagrama de barras - Modelo (año de lanzamiento) \n (Frecuencias relativas)")

#tablas de gráficos por calificación de contaminación del aire
tabla_calif_aire_fa = table(df$`Calificación Contam. Aire`); tabla_calif_aire_fa
tabla_calif_aire_fr = prop.table(tabla_calif_aire_fa); tabla_calif_aire_fr
barplot(tabla_calif_aire_fr, col = heat.colors(6), ylim = c(0,0.4))
title("Diagrama de barras - Calif. Cont. del aire (entre más alto, mejor) \n (Frecuencias relativas)")

#tablas de gráficos por calificación efecto invernadero
tabla_calif_gei_fa = table(df$`Calificación Gas Ef. Inv.`); tabla_calif_gei_fa
tabla_calif_gei_fr = prop.table(tabla_calif_gei_fa); tabla_calif_gei_fa
barplot(tabla_calif_gei_fr, col = heat.colors(6), ylim = c(0,0.4))
title("Diagrama de barras - Calif. Cont. del aire (entre más alto, mejor) \n (Frecuencias relativas)")


#2 Estimación puntual

y = df$`R. Ajust. (km/l)`
n = length(y)

#estimadores para la media
hist(y, breaks = 50,pch=20, prob=TRUE, main = "Redimiento ajustado en Km/l", xlab="Km/l")

#suponiendo normalidad

mu_gorro = mean(y);mu_gorro 
sigma_gorro = sqrt(var(y));sigma_gorro

#normal
(ajuste_2 = fitdistr(y, densfun = "normal"))
hist(y,breaks = 50, prob = TRUE, main = "Ajuste normal")
curve(dnorm(x, ajuste_2$estimate[1],ajuste_2$estimate[2]),col="blue", lwd=1, add=T) 
#la distribuión normal no ajusta del todo

#se prueba con Weibull
ajuste_3 = fitdistr(y, densfun  = "weibull"); ajuste_3
hist(y, breaks = 50 ,prob = TRUE, main = "Ajuste weibull")
curve(dweibull(x, ajuste_3$estimate[1],ajuste_3$estimate[2]),col="blue", lwd=1, add=T) 

#gamma
ajuste_4 = fitdistr(y, densfun = "gamma"); ajuste_3
hist(y,breaks = 50 ,prob = TRUE, main = "Ajuste gamma")
curve(dgamma(x, ajuste_4$estimate[1],ajuste_4$estimate[2]),col="blue", lwd=1, add=T)

k = 2
BIC_gamma= log(n)*k-(2*ajuste_4$loglik); BIC_gamma
BIC_weibull = log(n)*k-(2*ajuste_3$loglik); BIC_weibull
BIC_normal = log(n)*k-(2*ajuste_2$loglik); BIC_normal

hip1_media = ajuste_4$estimate[1]/ajuste_4$estimate[2];hip1
hip1_des = sqrt(ajuste_4$estimate[1]/ajuste_4$estimate[2]^2); hip1_des

p = 75
n = length(y)
porporción_muestral = qnorm(75, sqrt((26 * 75)/n)); porporción_muestral

x = df$`Calificación Gas Ef. Inv.`
y = df$`R. Ajust. (km/l)`

x_barra_r = sum(y)/n; x_barra_r
var_muestral_rendimiento = (1/(n-1)) * (sum(y^2) - sum(n*x_barra_r^2)); var_muestral_rendimiento

x_barra_gei  = sum(x)/n; media_muestral_gei
var_muestral_gei = (1/(n-1)) * (sum(x^2) - sum(n*x_barra_gei^2));var_muestral_gei

cov = (1/(n-1)) * (sum(x*y) - sum(n*x_barra_r * x_barra_gei)); cov

r = cov/(var_muestral_rendimiento*var_muestral_gei); r


x = df$`R. Ciudad (km/l)`
hist(x, breaks = 50, prob = TRUE, main = "Rendimiento en ciudad")
(ajuste_2 = fitdistr(x, densfun = "normal"))
hist(x,breaks = 50, prob = TRUE, main = "Ajuste normal")
curve(dnorm(x, ajuste_2$estimate[1],ajuste_2$estimate[2]),col="blue", lwd=1, add=T) 

kurtosis.norm.test(x, nrepl=2000)

t.test(x, alternative = "two.sided", conf.level = 0.83)$conf.int
stests::var.test(x, alternative = "two.sided", conf.level = 0.83)$conf.int

t.test(x, alternative = "two.sided", conf.level = 0.97)$conf.int
stests::var.test(x, alternative = "two.sided", conf.level = 0.97)$conf.int

mean(x)
var(x)

manual = df[Trans_resumen == "Manual",]
aut = df[Trans_resumen == "Automática",]

rendimiento_manual = manual$`R. Ciudad (km/l)`
rendimeinto_aut = aut$`R. Ciudad (km/l)`

t.test(x = rendimiento_manual, y = rendimeinto_aut, paired = FALSE, conf.level = 0.83)
stats::var.test(x = rendimiento_manual, y = rendimeinto_aut, paired = FALSE, conf.level = 0.83)

t.test(x = rendimiento_manual, y = rendimeinto_aut, paired = FALSE, conf.level = 0.97)
stats::var.test(x = rendimiento_manual, y = rendimeinto_aut, paired = FALSE, conf.level = 0.97)


