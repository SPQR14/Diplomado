library(readxl)
calif = read_excel("/home/spqr14/Escritorio/calif.xlsx")

attach(calif)

par(mfrow=c(3,3))
matplot(calif[,1:2],col=c(1,1),type="b")
matplot(calif[,3:4],col=c(2,2),type="b")
matplot(calif[,5:6],col=c(3,3),type="b")
matplot(calif[,7:8],col=c(4,4),type="b")
matplot(calif[,9:10],col=c(5,5),type="b")
matplot(calif[,11:12],col=c(6,6),type="b")
matplot(calif[,13:14],col=c(7,7),type="b")
matplot(calif,type="b",col=c(1,1,2,2,3,3))
par(mfrow=c(1,1))

cor(calif)

fa(calif,nfactors=1,fm="pa",rotate="promax")

fa(calif,nfactors=2,fm="pa",rotate="promax")

fa(calif,nfactors=3,fm="pa",rotate="promax")

fa(calif,nfactors=4,fm="pa",rotate="promax")

fa(calif,nfactors=5,fm="pa",rotate="promax")

fa(calif,nfactors=6,fm="pa",rotate="promax")

fa(calif,nfactors=7,fm="pa",rotate="promax")

fa(calif,nfactors=8,fm="pa",rotate="promax")

fa(calif,nfactors=9,fm="pa",rotate="promax")







