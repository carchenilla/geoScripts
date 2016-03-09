######   PRACTICA 5 - ANALISIS FACTORIAL 


#Lo primero de todo es instalar la librería "psych". A través de la interfaza de RStudio es muy sencillo (xddd)

#A continuación cargamos los datos del txt
#(que por cierto, tiene errores en las lineas 7,8 y 10 -> facepalm infinito)

#Mac  //// 
M <- read.table("/Users/carche/geoScripts/humedales.txt",header=T,sep=" ")
#Windows ////    M <- read.table("C:/Users/carchenilla/Documents/geoScripts/humedales.txt",header=T,sep=" ")
#Linux   //// M <- read.table("/home/carchenilla/geoScripts/humedales.txt", header=T, sep=" ")


#Ahora le hacemos el test de Bartlett para ver la esfericidad de los datos
#Mis dieses a la profe por pasarnos una matriz con columnas de texto que tenemos que quitar :D

bartlett.test(M[2:10])

#Nos sale un p valor < 0.05, por lo que podemos rechazar la hipotesis nula de que la varianza es la misma
#para todos los grupos. No podemos asumir homogeneidad entre varianzas


#Ahora le hacemos el princomp. 
#Mis dieses a la profe por pasarnos una matriz con columnas de texto que tenemos que quitar :D

modelo1 <- princomp(M[2:10],cor=TRUE)
summary(modelo1)

#Hay 3 autovalores, que son los 3 primeros, así que consideraremos 3 componentes, que además 
#conservan el 81% de la información. Quizás conviene pillar 4 (que es 0.98) y así subimos a 0.92?


loadings(modelo1)
plot(modelo1,type="lines")
modelo1 $scores
biplot(modelo1)

#Con el biplot se ven las 3 flechitas que te cagas 


#Ahora hay que instalar y "activar" la librería GPArotation
#Conesto hacemos otros modelos con rotaciones y esas cosas, pero que salen más o menos parecidos, con 3 flechitas

modelo2 <- principal(M[2:10], nfactors=3, rotate="varimax")
modelo2
summary(modelo2)
loadings(modelo2)
modelo2 $scores
biplot(modelo2)

#Para el tercer modelo utilizamos factanal, que produce un análisis basado en máxima 
#verosimilitud 

Modelo3<-factanal(M[2:10], 3,rotation="varimax")
print(Modelo3, digits=2, cutoff=.3, sort=TRUE)

load <- Modelo3$loadings[,1:3] 
plot(load,type="n") 

text(load,labels= names(M),cex=.7)