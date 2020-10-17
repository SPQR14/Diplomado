v1 <- c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
v2 <- c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5)
v3 <- c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
v4 <- c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4)
v5 <- c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
v6 <- c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4)
m1 <- cbind(v1,v2,v3,v4,v5,v6)

par(mfrow=c(2,2))
matplot(m1[,1:2],col=c(1,1),type="b")
matplot(m1[,3:4],col=c(2,2),type="b")
matplot(m1[,5:6],col=c(3,3),type="b")
matplot(m1,type="b",col=c(1,1,2,2,3,3))
par(mfrow=c(1,1))

cor(m1)

library(psych)

fa(m1, nfactors =3, fm="pa", rotate = "none", max.iter=100)

fa(m1, nfactors = 3, fm="pa", rotate = "varimax")






