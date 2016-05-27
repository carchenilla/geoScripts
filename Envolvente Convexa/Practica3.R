##################################################
#   PRACTICA 3:


#Funcion que calcula los vertices de una envolvente convexa dada una lista de aristas
#que no tiene por que estar ordenada

getVertex <- function(edges){       #Recibe un numerics donde cada grupo de cuatro 
  #elementos denota los dos puntos extremos de una arista
  
  verts <- rbind()                 #Cada grupo de 4 elementos representan las 4 coordenadas de los 2 vértices de la arista
  for (i in seq(from=1, to=length(edges), by=4)){
    x = c(edges[i],edges[i+1])  #cogemos los vertices que representan esa arista
    y = c(edges[i+2],edges[i+3])
    verts <- rbind(verts,x) #Se van metiendo en una matriz por filas
    verts <- rbind(verts,y)
  }
  newM = matrix(unique(verts),ncol = 2)    #matriz eliminando las filas repetidas, es decir, los vertices
  result <- c() #transformar matriz de vertices en numerics
  for (i in 1:nrow(newM)){
    result = c(result,c(newM[i,1],newM[i,2])) #para cada fila, concatenar a la lista final
  }
  return (result)
}