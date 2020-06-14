
# Diplomado en Tecnicas Estadisticas y Mineria de Datos
# MODULO II: Modelos Estadisticos
# Prof. Elizabeth Martinez
# FES-Acatlan, UNAM
# TUTORIAL 4: ALGUNAS DISTRIBUCIONES DE PROBABILIDAD PARAMETRICAS


#****************************************************************************************
#********************** DISTRIBUCIONES DE PROBABILIDAD DISCRETAS ************************
#****************************************************************************************
# R tiene ~20 distribuciones estadisticas. 
# Para cada distribucion R da 4 funciones: 
# 1. d (densidad o diferencial)
# 2. p (distribucion o integral)
# 3. q (quantile)
# 4. r (aleatorios)


# 1. Uniforme 

f_unif<- function(x){
  n= length(x)
  rep(1/n,n)  # para que cada valor de x tenga la misma probabilidad
}

x<- seq(1:12)
plot(x,f_unif(x),type = "h",xlim=c(0,12),ylim=c(0,0.1),col="red",lwd=2,ylab="f(x)",cex.axis=1.2,
     cex.lab=1.4,main="X~U(n=12)",cex.main=2)   # fmp
plot(x,cumsum(f_unif(x)),type = "s",col="red",lwd=2,ylab="F(x)",cex.axis=1.2,
     cex.lab=1.4)   # fda


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# 2. Bernoulli

f_ber<- function(x,p) p^{x}*(1-p)^{1-x}


dbinom(x=0:1,size=1,prob=0.56)   # en R

# Comparamos nuestra funcion con la programada en R

f_ber(0,0.7)
dbinom(0,1,0.7)

y<- c(0,1)
p<- 0.6
plot(y,f_ber(y,p),type = "h",ylim=c(0,1),col="red",lwd=2,xlab="x",ylab="f(x)",cex.axis=1.2,
     cex.lab=1.4,main="X~Ber(p=0.6)",cex.main=2)    # fmp
plot(y,cumsum(f_ber(y,p)),type = "s",ylim=c(0,1),col="red",lwd=2,xlab="x",ylab="F(x)",cex.axis=1.2,
     cex.lab=1.4)   # fda

# Ejercicio: Utilizando la funcion en R, grafica la fmp para X~Ber(p=0.47)

plot(y,f_ber(y,0.47),type = "h",ylim=c(0,1),col="red",lwd=2,xlab="x",ylab="f(x)",cex.axis=1.2,
     cex.lab=1.4,main="X~Ber(p=0.6)",cex.main=2)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 3. Binomial

f_bin<- function(x,n,p) choose(n,x)*(p^{x}*(1-p)^{n-x})

n<- 10
x<- seq(0:n)
p<- 0.2
plot(x,f_bin(x,n,p),type = "h",ylim=c(0,0.4),col="red",lwd=2,xlab="x",ylab="f(x)",cex.axis=1.2,
     cex.lab=1.4,main="X~Bin(n=10,p=0.2)",cex.main=2)
plot(x,cumsum(f_bin(x,n,p)),type = "s",ylim=c(0,1),col="red",lwd=2,xlab="x",ylab="F(x)",cex.axis=1.2,
     cex.lab=1.4)


# Usando la funcion de R
par(mfrow=c(1,2))
k <- c(1:20)
plot(k,dbinom(k,size=20,prob=0.3),type="h",xlab="k",ylab="fk")
text(15,0.10,"Bin(n=20,p=0.3)")
plot(k,cumsum(dbinom(k,size=20,prob=0.3)),type="s",xlab="k",ylab="Fk")
text(15,0.40,"Bin(n=20,p=0.3)")
par(mfrow=c(1,1))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 4. Binomial Negativa

# Si N=# ensayos antes del r-esimo exito
f_binneg_1<- function(n,r,p) choose(n-1,r-1)*((1-p)^{n-r})*p^{r}   

r<- 2
n<- seq(r:r+12)
p<- 0.3

plot(n,f_binneg_1(n,r,p),type = "h",col="red",lwd=2,xlab="x",ylab="f(x)",cex.axis=1.2,
     cex.lab=1.4,main="X~BinNeg(r=2,p=0.3)",cex.main=2)
plot(n,cumsum(f_binneg_1(n,r,p)),type = "s",ylim=c(0,1),col="red",lwd=2,xlab="x",ylab="F(x)",cex.axis=1.2,
     cex.lab=1.4)


# Si X=# fallas hasta observar el r-esimo exito
f_binneg_2<- function(x,r,p) choose(x+r-1,r-1)*((1-p)^{x})*p^{r}  
x<- seq(0,12)

plot(x,f_binneg_2(x,r,p),type = "h",col="red",lwd=2,xlab="x",ylab="f(x)",cex.axis=1.2,
     cex.lab=1.4,main="X~BinNeg(r=2,p=0.3)",cex.main=2)
plot(x,cumsum(f_binneg_2(x,r,p)),type = "s",ylim=c(0,1),col="red",lwd=2,xlab="x",ylab="F(x)",cex.axis=1.2,
     cex.lab=1.4)



# Usando la funcion de R
# X=N= # fallas antes de alcanzar el r-esimo exito
# x= r,r+1,r+2,....
# size=r (numero de exitos)

par(mfrow=c(1,2))
x <- seq(0:20)
plot(x,dnbinom(x,size=r,prob=0.15),type="h",xlab="x",ylab="fx")
text(15,0.05,"BinNeg(r=2,p=0.15)")   # fmp
plot(x,cumsum(dbinom(x,size=r,prob=0.15)),type="s",xlab="x",ylab="Fx")
text(15,0.27,"BinNeg(r=2,p=0.15)")  # fda
par(mfrow=c(1,1))


f_binneg_2(12,10,0.8)  # dnbinom(12,10,0.8)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 5. Geometrica

# Usando la funcion de R

par(mfrow=c(1,2))
x <- seq(0:10)
plot(x,dgeom(x,prob=0.15),type="h",xlab="x",ylab="fx")
text(8,0.1,"Geo(p=0.15)")
plot(x,cumsum(dgeom(x,prob=0.15)),type="s",xlab="x",ylab="Fx")
text(3.5,0.6,"Geo(p=0.15)")
par(mfrow=c(1,1))


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 6. Poisson

f_poi<- function(x,lambda) lambda^{x}*exp(-lambda)/factorial(x)


x<- seq(0:5)

plot(x,f_poi(x,lambda=0.5),type = "h",col="red",lwd=2,xlab="x",ylab="fx",cex.axis=1.2,
     cex.lab=1.4,main=expression(paste("X~Po(",lambda,"=",0.5,")")),cex.main=2)
plot(x,cumsum(f_poi(x,lambda=0.5)),type = "s",col="red",lwd=2,xlab="x",ylab="Fx",cex.axis=1.2,
     cex.lab=1.4)


plot(x,f_poi(x,lambda=3),type = "h",col="red",lwd=2,xlab="x",ylab="fx",cex.axis=1.2,
     cex.lab=1.4,main=expression(paste("X~Po(",lambda,"=",3,")")),cex.main=2)
plot(x,cumsum(f_poi(x,lambda=3)),type = "s",col="red",lwd=2,xlab="x",ylab="Fx",cex.axis=1.2,
     cex.lab=1.4)


# Usando la funcion de R

plot(x,dpois(x,lambda=6),type="h")


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 7. Hipergeometrica

# Usando la funcion en R

plot(x=0:5,dhyper(x=0:5, m=7, n=5, k=3),type = "h",col="red",lwd=2,xlab="x",ylab="fx",cex.axis=1.2,
     cex.lab=1.4,main="X~HGeo(N=12,K=7,n=3)",cex.main=2)   # K es el valor de m para R

plot(x=0:5,cumsum(dhyper(x=0:5, m=7, n=5, k=3)),type = "s",col="red",lwd=2,xlab="x",ylab="Fx",cex.axis=1.2,
     cex.lab=1.4,cex.main=2)   # K es el valor de m para R

#****************************************************************************************
#************************** APLICACION DE DISTRIBUCIONES DISCRETAS **********************
#****************************************************************************************

# Problema 1: Y~Bin(n=10,p=1/4) o W~Bin(n=10,p=3/4)

# P(Y<8)
pbinom(q=7,size=10,prob=1/4,lower.tail=T) #lower.tail = T --> colas izquieras o <= 

# P(W>2)
1-pbinom(q=2,size=10,prob=3/4,lower.tail=T)
pbinom(q = 2, size = 10, prob = 3/4, lower.tail = F)
pbinom(2, 10, 3/4, lower.tail = F)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Problema 2: N~BinNeg(r=3,p=0.74)

f_binneg_1(10,3,0.74)  # usando nuestra funcion (ver notas de clase)

# No podemos usar la funcion de R porque tiene otra definicion en terminos del
# numero de fracasos y el soporte es distinto.

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Problema 3: X~BinNeg(r=1,p=0.25) ? X~Geom(p=0.25)
# P(X>=6)=1-P(X<=5)

1-sum(f_binneg_1(1:5,1,0.25))   # usando nuestra funcion binomial negativa

1-pnbinom(4,size=1,prob=0.25,lower.tail = T)   # en R usando binomial negativa
# con r=1 exito (geometrica)

1-pgeom(4,prob = 0.25,lower.tail = T) # en R usando geometrica

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Problema 4: X~Po(lambda=2)

# b) X'=# personas que llegan a urgencias cada 20 min; X'~Po(4)
# P(X'=5)

dpois(x=5,lambda = 4)

# c) P(X'>=3)=1-P(X'<3)=1-P(X'<=2)

1-ppois(q=2,lambda = 4,lower.tail = T)
ppois(q=2,lambda = 4,lower.tail = F)    # alternativa

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Problema 5: X~Bin(n=100,p=0.015); Y~Po(l=100*0.015)

# a) P(X>=3)
pbinom(q=2,size=100,prob=0.015,lower.tail=F)
1-(dbinom(0,100,0.015)+dbinom(1,100,0.015)+dbinom(2,100,0.015))

# b) Y~Po(1.5)
# P(Y>=3)

ppois(q=2,lambda=100*0.015,lower.tail=F)

# Es preferible la distribucion exacta que una aproximacion.

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Problema 6: X~HGeo(N=10,K=6,n=5,x=3)
# P(X=3)

dhyper(x=3,m=6,n=4,k=5)  # usando la funcion de R


#****************************************************************************************
#********************** DISTRIBUCIONES DE PROBABILIDAD CONTINUAS ************************
#****************************************************************************************
# R tiene ~20 distribuciones estadisticas. 
# Para cada distribucion R da 4 funciones: 
# 1. d (densidad o diferencial)
# 2. p (distribucion o integral)
# 3. q (quantile)
# 4. r (aleatorios)


# 1. Uniforme

# Usando la funcion en R

plot(x=-5:5,dunif(x=-5:5,min=-5,max=5),type = "l",col="red",lwd=2,xlab="x",ylab="fx",cex.axis=1.2,
     cex.lab=1.4,main="X~U(a=-5,b=5)",cex.main=2)   

plot(x=-5:5,punif(q=-5:5,min=-5,max=5),type = "l",col="red",lwd=2,xlab="x",ylab="Fx",cex.axis=1.2,
     cex.lab=1.4,cex.main=2)   


#######################################################################

# 2. Exponencial 


# Usando la funcion en R

plot(x=seq(0,3,0.1),dexp(x=seq(0,3,0.1),rate=2.5),type = "l",col="red",lwd=2,xlab="x",ylab="fx",cex.axis=1.2,
     cex.lab=1.4,main=expression(paste("X~exp(",lambda,"=",2.5,")")),cex.main=2)   

plot(x=seq(0,3,0.1),pexp(q=seq(0,3,0.1),rate=2.5),type = "l",col="red",lwd=2,xlab="x",ylab="Fx",cex.axis=1.2,
     cex.lab=1.4,cex.main=2)   


#######################################################################

# 3. Gamma 


# Usando la funcion en R

plot(x=seq(0,3,0.05),dgamma(x=seq(0,3,0.05),shape=2,scale=1/2.5),type = "l",col="red",lwd=2,xlab="x",ylab="fx",cex.axis=1.2,
     cex.lab=1.4,main=expression(paste("X~Gamma(",alpha,"=",2,",",beta,"=",2.5,")")),cex.main=2)   

plot(x=seq(0,3,0.05),pgamma(q=seq(0,3,0.05),shape=2,scale=1/2.5),type = "l",col="red",lwd=2,xlab="x",ylab="Fx",cex.axis=1.2,
     cex.lab=1.4,cex.main=2)   


#######################################################################

# 4. Beta 

# Usando la funcion en R (a,b < 1)

p = seq(0,1, length=500)
plot(p, dbeta(p, 0.25, 0.25),type ="l",col=4,lwd=2,xlab="x",ylab="fx",cex.axis=1.2,
     cex.lab=1.4,main=expression(paste("X~Beta(",alpha,",",beta,")")),cex.main=2)   

lines(p, dbeta(p, 0.9, 0.5), type ="l", col=3,lwd=2)
legend(0.5,8, c("Be(0.25,0.25)","Be(0.9,0.5)"),lty=c(1,1,1,1),col=c(4,3,2,1))


plot(p, pbeta(p, 0.25, 0.25),type ="l",col=4,lwd=2,xlab="x",ylab="Fx",cex.axis=1.2,
     cex.lab=1.4,cex.main=2)   
lines(p, pbeta(p, 0.9, 0.5), type ="l", col=3,lwd=2)
legend(0.2,1, c("Be(0.25,0.25)","Be(0.9,0.5)"),lty=c(1,1,1,1),col=c(4,3,2,1))


# Usando la funcion en R (a,b > 1)

p = seq(0,1, length=500)
plot(p, dbeta(p, 1.2, 2),type ="l",col=4,lwd=2,xlab="x",ylab="fx",cex.axis=1.2,
     cex.lab=1.4,main=expression(paste("X~Beta(",alpha,",",beta,")")),cex.main=2,ylim=c(0,5))   

lines(p, dbeta(p, 2.5, 1.5), type ="l", col=3,lwd=2)
legend(0.5,4, c("Be(1.2,2)","Be(2.5,1.5)"),lty=c(1,1,1,1),col=c(4,3,2,1))


plot(p, pbeta(p, 1.2, 2),type ="l",col=4,lwd=2,xlab="x",ylab="Fx",cex.axis=1.2,
     cex.lab=1.4,cex.main=2,ylim=c(0,1))   

lines(p, pbeta(p, 2.5, 1.5), type ="l", col=3,lwd=2)
legend(0,0.8, c("Be(1.2,2)","Be(2.5,1.5)"),lty=c(1,1,1,1),col=c(4,3,2,1))

#######################################################################

# 5. Normal

x = seq(-20,20, length=1000)
plot(x, dnorm(x, mean=-2.5, sd=4),type ="l",col=4,lwd=2,xlab="x",ylab="fx",ylim=c(0,0.8),cex.axis=1.2,
     cex.lab=1.4,main=expression(paste("X~N(",mu,",",sigma,")")),cex.main=2)   

lines(x, dnorm(x, mean=0.5, sd=0.5), type ="l", col=3,lwd=2)
lines(x, dnorm(x, mean=3, sd=9), col=2,lwd=2) 
lines(x, dnorm(x, mean=-5, sd=1), col=1,lwd=2) 
legend(7,0.65, c("N(-2.5,4)","N(0.5,0.5)","N(3,9)", "N(-5,1)"),lty=c(1,1,1,1),col=c(4,3,2,1))


plot(x, pnorm(x, mean=-2.5, sd=4),type ="l",col=4,lwd=2,xlab="x",ylab="Fx",cex.axis=1.2,
     cex.lab=1.4,cex.main=2,ylim=c(0,1))   

lines(x, pnorm(x, mean=0.5, sd=0.5), type ="l", col=3,lwd=2)
lines(x, pnorm(x, mean=3, sd=9), col=2,lwd=2) 
lines(x, pnorm(x, mean=-5, sd=1), col=1,lwd=2) 
legend(-18,0.85, c("N(-2.5,4)","N(0.5,0.5)","N(3,9)", "N(-5,1)"),lty=c(1,1,1,1),col=c(4,3,2,1))


#****************************************************************************************
#************************** APLICACION DE DISTRIBUCIONES CONTINUAS **********************
#****************************************************************************************

# Problema 1: X~U(a=7,b=10)

# a) P(X<=8.8)

punif(8.8,7,10,lower.tail = T)

# b) P(7.4<X<9.5)

punif(9.5,7,10,lower.tail = T)-punif(7.4,7,10,lower.tail = T)

# c) P(X>=8.5)

punif(8.5,7,10,lower.tail = F)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Problema 2: 

# a) X= # personas que usan cajero 
# X~Po(lambda=6)

Lambda<- 6*1/60; Lambda   # por minuto

# Y= tiempo transcurrido entre la llegada de dos personas
# Y~exp(Lambda)

# a) P(Y>10)

pexp(10,rate=Lambda,lower.tail = F)

# b) P(Y>20)

pexp(20,rate=Lambda,lower.tail = F)

# c) P(Y<1)

pexp(1,rate=Lambda,lower.tail = T)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Problema 3: 

# T= tiempo de atencion en taquilla
# T~exp(lambda=4)

# Y= numero de dias en ser atendidas en menos de 3 min
# Y~Bin(6,p=P(T<3))

# P(T<3)

p<- pexp(3,rate=1/4,lower.tail = T); p

# P(Y>=4)

1-pbinom(3,size=6,prob=p,lower.tail=T)

pbinom(3,size=6,prob=p,lower.tail=F)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Problema 4: X~N(mu=1.5,sigma=0.2)

# P(1.5-d<X<1-5+d)=0.95

x0_1<- qnorm(p=0.025,mean=1.5,sd=0.2,lower.tail=T)
x0_2<- qnorm(p=0.025,mean=1.5,sd=0.2,lower.tail=F)

d1=x0_1-1.5; d1
d2=-(x0_2-1.5);d2

z0_1<- qnorm(p=0.025,mean=0,sd=1,lower.tail=T)   # estandarizada
z0_2<- qnorm(p=0.025,mean=0,sd=1,lower.tail=F)   # estandarizada

d_1=z0_1*0.2;d_1
d_2=z0_2*0.2;d_2

# Graficamos las normales
x<- seq(-5,5,by=0.02)
plot(x,dnorm(x,mean=1.5,sd=0.2),type="l",lwd=2,
     lty=1,col="red",xlab="x",ylab="fx",
     main="Normales")
lines(x,dnorm(x,mean=0,sd=1),col=4,lwd=2)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Problema 5: Y~Beta(alpha=3,beta=5)

# b) P(Y>0.73)

# Escribimos la funcion de probabilidad a mano para ver como se calculan los valores 
# de la funcion especial Beta

fbeta<- function(x){
  alpha=3
  beta=5
  gamma(alpha+beta)/(gamma(alpha)*gamma(beta))*x^(3-1)*(1-x)^(5-1)
}        

integrate(fbeta,0.73,1)

# usando la funcion de R
pbeta(q=0.73,shape1=3,shape2=5,lower.tail = F)






