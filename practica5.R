######   PRACTICA 5 - ANALISIS FACTORIAL 


#Lo primero de todo es instalar la librería "psych". A través de la interfaza de RStudio es muy sencillo (xddd)

#A continuación cargamos los datos del txt
#(que por cierto, tiene errores en las lineas 7,8 y 10 -> facepalm infinito)

M <- read.table("C:/Users/carchenilla/Documents/geoScripts/humedales.txt",header=T,sep=" ")


#Ahora le hacemos el test de Bartlett para ver la esfericidad de los datos
#Ahora le hacemos el princomp. Agradecemos a la profe que nos cuente que a esta funcion solo hay que
#pasarle la parte numerica de la matriz :D

bartlett.test(M)


#Nos sale un p valor < 0.05, por lo que podemos rechazar la hipotesis nula de que la varianza es la misma
#para todos los grupos. No podemos asumir homogeneidad entre varianzas


#Ahora le hacemos el princomp. Agradecemos a la profe que nos cuente que a esta funcion solo hay que
#pasarle la parte numerica de la matriz :D

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