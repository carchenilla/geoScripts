#PRACTICA 4 GEOMETRIA COMPUTACIONAL


#Funcion para calcular la envolvente convexa usando fuerza bruta
#Recibe un numeric con las coordenadas de los puntos
#Devuelve una matriz con los puntos de la envolvente convexa (por filas)

getConvexHull <- function(P){
  plotInitialPoints(P);
  pairs = getPairs(P)     #generar parejas de puntos
  n = length(pairs)/4
  hull = matrix(0,nrow=0,ncol=4,byrow=TRUE)    #reservar espacio para matriz final
  for (k in 1:n){
    a = c(pairs[k,1],pairs[k,2])   #cogemos una pareja de puntos
    b = c(pairs[k,3],pairs[k,4])
    flag = TRUE       #flag para marcar si pertenecen a la envolvente
    i = 1
    while ((i<=(length(P)/2))&&flag){          #comprobamos si todos los puntos estan a la dcha de la pareja
      thisPoint = c(P[(i-1)*2+1],P[(i-1)*2+2])
      if (checkPoint(thisPoint,a,b)){     #se comprueban todos los puntos de P menos los propios de la pareja
        if(!(rightPos(thisPoint,a,b))){    #en cuanto uno de ellos no esta a la dcha salimos del bucle
          flag=FALSE                       #y continuamos con la siguiente pareja
        } 
      }
      i=i+1
    }
    if (flag){    #si todos los puntos estaban a la derecha, se anaden los 2 puntos a la matriz final
      hull = rbind(hull,c(a,b))
    }
  }
  finalMatrix <- matrix(hull, nrow=(length(hull)/4), ncol=4, byrow=FALSE)
  plotFinalPoints(finalMatrix)
  #rehacemos la matriz para regenerar los indices (al hacer rbind las filas no se anaden con numeros)
  #y la devolvemos pero solo con sus filas unicas, para eliminar repeticiones de puntos
}



#Funcion para calcular todas las parejas de puntos (variaciones sin repeticion)
#Recibe un numeric con las coordenadas de los puntos
#Devuelve una matriz con tantas filas como parejas haya y con 4 columnas, para almacenar las coordenadas
#de los 2 puntos de la pareja

getPairs <- function(P){  
  elems = factorial(length(P)/2)/(factorial(length(P)/2-2))
  cont = 1
  pairs <- matrix(0,nrow=elems,ncol=4,byrow=TRUE)   
  for (i in 1:(length(P)/2)){               
    for (j in 1:(length(P)/2)){
      if(j!=i){
        pairs[cont,1]=P[(i-1)*2+1]
        pairs[cont,2]=P[(i-1)*2+2]
        pairs[cont,3]=P[(j-1)*2+1]
        pairs[cont,4]=P[(j-1)*2+2] 
        cont = cont+1
      }
    }
  }
  return(pairs)
}


  
#Funcion para comprobar si un punto es igual a uno de la pareja
#Recibe el punto (p) a comprobar y los puntos de la pareja (a y b)
#Devuelve bool

checkPoint <- function(p,a,b){
  if (((p[1]==a[1])&&(p[2]==a[2]))||((p[1]==b[1])&&(p[2]==b[2]))){
    return(FALSE)
  }
  else{
    return(TRUE)
  }
}
  
  
  
#Funcion para comprobar si un punto queda a la dcha de una recta dada por 2 puntos
#Recibe el punto (p) a comprobar y dos puntos de la recta (a y b)
#Devuelve bool

rightPos<-function(p,a,b){
  A <- matrix(c(b[1]-a[1],b[2]-a[2],
                p[1]-a[1],p[2]-a[2]),nrow=2,ncol=2,byrow = TRUE)
  n <- det(A)
  if (n<0){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}


plotInitialPoints <- function(P){
    minx = min(P[seq(from=1, to=length(P), by=2)])
    miny = min(P[seq(from=2, to=length(P), by=2)])
    maxx = max(P[seq(from=1, to=length(P), by=2)])
    maxy = max(P[seq(from=2, to=length(P), by=2)])
    plot(c(minx-1,maxx+1),c(miny-1,maxy+1))
    for (i in seq(from=1, to=length(P), by=2)){
      text(P[i],P[i+1],'o')
    }
}

plotFinalPoints <- function(M){
  print(M)
  finalList <- c(M[1,1],M[1,2],M[1,3],M[1,4])
  print(finalList)
  cont = length(M)/4-1
  while (cont>0){
    p <- c(finalList[length(finalList)-1],finalList[length(finalList)])
    print(p)
    nextP <- findPoint(p,M)
    finalList <- c(finalList,nextP)
    print(finalList)
    cont = cont-1
  }
  xCoord = finalList[seq(from=1, to=length(P)-2, by=2)]
  yCoord = finalList[seq(from=2, to=length(P)-2, by=2)]
  polygon(xCoord,yCoord,lty=1,lwd=2, border="blue")
}

findPoint <- function(p,M){
  i=1
  while((M[i,1]!=p[1])&&(M[i,2]!=p[2])){
    i=i+1
  }
  print(i)
  return(c(M[i,3],M[i,4]))
}