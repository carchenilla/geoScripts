getVertex <- function(edges){       #Recibe una lista de numerics
  result <- rbind()
  for(i in 1:length(edges)){
    for (j in seq(from=1, to=3, by=2)){
      x = edges[[i]][j]
      y = edges[[i]][j+1]
      result <- rbind(result,c(x,y)) 
    }
  }
  unique(result)    #Devuelve una matriz cuyas filas son los vertices
}