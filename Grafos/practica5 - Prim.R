##### PRACTICA 5 - GRAFOS - ALGORITMO DE PRIM 

edge <- setClass("edge",slots = c(cost="numeric",begin="numeric", end="numeric"))

primAlgorithm <- function(M){
  firstEdge = getFirstEdge(M)
  visitedEdges = c(firstEdge)
  visitedNodes = c(slot(firstEdge,"begin"), slot(firstEdge,"end"))
  while (length(visitedNodes)<ncol(M)){
    newEdge = checkDestinations(M,visitedNodes)
    visitedNodes = c(visitedNodes, slot(newEdge,"end"))
    visitedEdges = c(visitedEdges, newEdge)
  }
  
  #reconstruir grafo con las aristas que hay en la lista de aristas visitadas
  newMatrix = matrix(0,nrow=nrow(M), ncol=ncol(M))
  for (i in 1:length(visitedEdges)){
    cost = slot(visitedEdges[[i]],"cost")
    begin = slot(visitedEdges[[i]],"begin")
    end = slot(visitedEdges[[i]],"end")
    newMatrix[begin,end]=cost
    newMatrix[end,begin]=cost
  }
  return(newMatrix)
}


getFirstEdge <- function(M){
  result = max(M)
  coord = c()
  for (i in 1:nrow(M)){
    for (j in 1:ncol(M)){
      if ((M[i,j]>0) && (M[i,j]<=result)){
        result = M[i,j]
        coord = c(i,j)
      }
    }
  }
  return(edge(cost=result, begin=coord[1], end=coord[2]))
}

checkDestinations <- function(M,visitedNodes){
  candidates = c()
  for (i in 1:length(visitedNodes)){
    n = getMinEdge(M,visitedNodes[i],visitedNodes)
    if (length(n)!=0){
      candidates = c(candidates,n)
    }
  }
  result = candidates[[1]]
  for (i in 1:length(candidates)){
    if((slot(candidates[[i]],"cost"))<(slot(result, "cost"))){
      result = candidates[[i]]
    }
  }
  return(result)
}


getMinEdge <- function(M,row,visitedNodes){
  result = max(M[row,])
  node=NULL
  for (i in 1:ncol(M)){
    if ((M[row,i]>0) && (M[row,i]<=result)){
      if (!(i %in% visitedNodes)){
        result = M[row,i]
        node=i 
      }
    }
  }
  if (length(node)==0){
    return(NULL)
  }
  return(edge(cost=result, begin=row, end=node))
}


##Matriz del ejemplo de la presentacion
M = matrix(0,nrow=7, ncol=7)
M[1,2]=5
M[2,1]=5
M[4,1]=6
M[1,4]=6
M[1,6]=7
M[6,1]=7
M[2,3]=5
M[3,2]=5
M[2,4]=4
M[4,2]=4
M[2,4]=5
M[4,2]=5
M[5,2]=4
M[2,5]=4
M[3,5]=3
M[5,3]=3
M[3,7]=3
M[7,3]=3
M[4,5]=2
M[5,4]=2
M[4,6]=2
M[6,4]=2
M[7,4]=3
M[4,7]=3
M[5,7]=1
M[7,5]=1
M[7,6]=2
M[6,7]=2