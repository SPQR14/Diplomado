f_ber<- function(x,p) p^{x}*(1-p)^{1-x}
y<- c(0,1)

plot(y,f_ber(y,0.47),type = "h",ylim=c(0,1),col="red",lwd=2,xlab="x",ylab="f(x)",cex.axis=1.2,
     cex.lab=1.4,main="X~Ber(p=0.6)",cex.main=2)