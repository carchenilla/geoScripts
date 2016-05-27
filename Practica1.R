##################################################
#   PRACTICA 1:


calcArea<-function(v,w){
    a = c(0,0)
    b = a+v
    c = b+w
    d = a+w
    plotFig(c(a[1],b[1],c[1],d[1]),c(a[2],b[2],c[2],d[2]))
    base <- sqrt(c[1]^2+c[2]^2)
    points <- list(b,d)
    M <- rbind(c(c[2],-c[1]), 
               c(c[1],c[2]))
    for (i in 1:length(points)){  
      aux = points[[i]]
      N <- c(0,
           aux[1]*c[1]+aux[2]*c[2])
      p1 <- solve(M,N)
      points[[i]]<-c(p1[1],p1[2])
    }
    h1 <- sqrt((b[1]-points[[1]][1])^2+(b[2]-points[[1]][2])^2)
    h2 <- sqrt((d[1]-points[[2]][1])^2+(d[2]-points[[2]][2])^2)
    base*h1/2+base*h2/2
}

plotFig <- function(vx,vy){
  x <-c(vx[1]-1,vx[3]+1)
  y <-c(vy[1]-1,vy[3]+1) 
  plot (x,y,main = paste("Poligono"))
  polygon(vx,vy,col="orange",lty=1,lwd=2, border="blue")
}
