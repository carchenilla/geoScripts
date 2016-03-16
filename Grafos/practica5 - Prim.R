##### PRACTICA 5 - GRAFOS - ALGORITMO DE PRIM 

#asumimos grafo NO dirigido

#necesaria la librería "igraph" para poder dibujar el grafo y su arbol soporte
#necesaria la librería "Matrix" para poder sacar el triangulo superior de una matriz (está incluida en el paquete del sistema)


#tipo de datos propio para guardar la información de las aristas
edge <- setClass("edge",slots = c(cost="numeric",begin="numeric", end="numeric"))


#algoritmo de prim
#recibe: matriz de adyacencia con los costes de cada arista
#devuelve: matriz de adyacencia del arbol soporte de minimo coste
primAlgorithm <- function(M){
  plotGraph(M,1)
  firstEdge = getFirstEdge(M)   #elegimos la arista de menor peso
  visitedEdges = c(firstEdge)   #marcamos esa arista
  visitedNodes = c(slot(firstEdge,"begin"), slot(firstEdge,"end"))    #marcamos los nodos visitados
  while (length(visitedNodes)<ncol(M)){     #repetir hasta visitar todos los nodos
    newEdge = checkDestinations(M,visitedNodes)   #buscar nueva arista
    visitedNodes = c(visitedNodes, slot(newEdge,"end"))   #marcar el nuevo nodo visitado
    visitedEdges = c(visitedEdges, newEdge)     #marcar la arista visitada
  }
  newMatrix = matrix(0,nrow=nrow(M), ncol=ncol(M))      #reconstruir grafo con las aristas que hay en la lista de aristas visitadas
  for (i in 1:length(visitedEdges)){
    cost = slot(visitedEdges[[i]],"cost")
    begin = slot(visitedEdges[[i]],"begin")
    end = slot(visitedEdges[[i]],"end")
    newMatrix[begin,end]=cost
    newMatrix[end,begin]=cost
  }
  plotGraph(newMatrix,2)
  return(newMatrix)
}


#calcular arista de menor coste
#recibe: matriz de adyacencia con los costes de cada arista
#devuelve: arista de menor coste
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


#calcular menor arista accesible a partir de los nodos ya visitados
#recibe: matriz de adyacencia con los costes de cada arista y la lista de nodos visitados
#devuelve: arista con menor coste accesible desde los nodos visitados
checkDestinations <- function(M,visitedNodes){
  candidates = c()  #lista de candidatos
  for (i in 1:length(visitedNodes)){    #para cada nodo visitado calculamos la menor arista accesible desde él y que no lleve a otro nodo ya visitado
    n = getMinEdge(M,visitedNodes[i],visitedNodes)
    if (length(n)!=0){    #si desde ese nodo se encontró una arista accesible
      candidates = c(candidates,n)    #la metemos en la lista de candidatos
    }
  }
  result = candidates[[1]]    #elegimos la menor arista de la lista de candidatos
  for (i in 1:length(candidates)){
    if((slot(candidates[[i]],"cost"))<(slot(result, "cost"))){
      result = candidates[[i]]
    }
  }
  return(result)
}


#calcular menor arista accesible desde un nodo dado y que no lleve a un nodo ya visitado
#recibe: matriz de adyacencia con los costes de cada arista, el nodo desde el que buscar y la lista de nodos visitados
#devuelve: si encuentra la arista accesible de menor coste y que no lleve a un nodo ya visitado, la devuelve
#          en caso contrario devuelve null
getMinEdge <- function(M,row,visitedNodes){
  result = max(M[row,])
  node=NULL
  for (i in 1:ncol(M)){
    if ((M[row,i]>0) && (M[row,i]<=result)){  #si la arista es de menor o igual coste que la del resultado provisional
      if (!(i %in% visitedNodes)){  #y si no lleva a un nodo ya visitado
        result = M[row,i]   #se sobreescribe el resultado provisional
        node=i 
      }
    }
  }
  if (length(node)==0){ #si no se modificó node (porque no se encontró arista desde este nodo) se devuelve null
    return(NULL)
  }
  return(edge(cost=result, begin=row, end=node))
}


#dibujar el grafo 
#recibe: matriz de adyacencia del grafo
#devuelve: nada. Solo hace el plot del grafo que represente M
plotGraph <- function(M, m){
  L = matrix(triu(M),nrow=nrow(M),ncol=ncol(M))
  g1 <- graph.adjacency(L,mode="undirected", weighted=TRUE, diag=FALSE)
  if (m==1){
    plot(g1, main="grafo original")
  }
  else{
    plot(g1, main="arbol soporte")
  }
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