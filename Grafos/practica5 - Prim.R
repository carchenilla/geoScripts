##### PRACTICA 5 - GRAFOS - ALGORITMO DE PRIM 

edge <- setClass("edge",slots = c(cost="numeric",begin="numeric", end="numeric"))

primAlgorithm <- function(M){
  firstEdge = getFirstEdge(M)
  visitedEdges = c(firstEdge)
  visitedNodes = c(slot(firstEdge,"begin"), slot(firstEdge,"end"))
  
  #bucle
  newEdge = checkDestinations(M,visitedNodes)
  
  
}


getFirstEdge <- function(M){
  result = M[1,1]
  coord = c(1,1)
  for (i in 1:nrow(M)){
    for (j in 1:ncol(M)){
      if ((M[i,j]>0) && (M[i,j]<result)){
        result = M[i,j]
        coord = c(i,j)
      }
    }
  }
  return(edge(cost=result, begin=coord(1), end=coord(2)))
}

checkDestinations <- function(M,visited){
  candidates = c()
  for (i in 1:length(visited)){
    n = getMinEdge(M,visited(i),visited)
    candidates = c(candidates,n)
  }
  #minimo de candidates
}


getMinEdge <- function(M,row,visited){
  result = M[row,1]
  node = 1
  for (i in 1:ncol(M)){
    if ((M[row,i]>0) && (M[row,i]<result)){
      if (!(i %in% visited)){
        result = M[row,i]
        node=i 
      }
    }
  }
  return(edge(cost=result, begin=row, end=node))
}


##Funcion para generar matriz
caca <- function(){
  c = matrix(0,nrow=3,ncol=3)
  for (i in 1:3){
    for (j in 1:3){
      c[i,j]=j+3*(i-1)
    }
  }
  c[1,1]=999
  c[2,3]=1
  return(c)
}