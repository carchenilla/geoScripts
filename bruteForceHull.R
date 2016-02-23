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



#Funcion para calcular la envolvente convexa usando fuerza bruta
#Recibe un numeric con las coordenadas de los puntos
#Devuelve una matriz con los puntos de la envolvente convexa (por filas)

getConvexHull <- function(P){
  pairs = getPairs(P)     #generar parejas de puntos
  #FALTA LA FUNCION TOCHA
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

checkPos<-function(p,a,b){
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