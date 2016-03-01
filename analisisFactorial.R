install.packages("psych")

M <- read.table("/Users/carche/geoScripts/Datos1.txt",header=T,sep=" ")

bartlett.test(M)

modelo1 <- princomp(M,cor=TRUE)

summary(modelo1)

loadings(modelo1)

plot(modelo1,type="lines")

modelo1 $scores

install.packages("GPArotation")

modelo2  <- principal(M,nfactors=2,rotate="varimax")

summary(modelo2)

