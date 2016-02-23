##################################################
#   PRACTICA 1:

Dibujo<-function(p,r){
    x <- c(p[1]-1,p[1]+1)
    y <- r(x)
    a <- min(p[2],y[1])
    b <- max(p[2],y[2])
    plot (x,y,type='l',ylim=c(a,b))
    text(p[1],p[2],'o')
}

Proyeccion<-function(p,r){
    x<-p[1]
    yeval <- r(x)
    if(p[2]>yeval){
        print("arriba")
    }
    else if(p[2]==yeval){
        print("en la recta") 
    }
    else{
        print("abajo")
    }
}

    
checkPos<-function(p,r){
  a<-c(1,r(1))
  b<-c(2,r(2))
  A <- matrix(c(b[1]-a[1],b[2]-a[2],p[1]-a[1],p[2]-a[2]),nrow=2,ncol=2,byrow = TRUE)
  n <- det(A)
  if (n>0){
    print("arriba")
  }
  else if (n==0){
    print("en la recta")
  }
  else{
    print("abajo")
  }
}
 

calcArea<-function(vx,vy){
    v <- c(vx[3]-vx[1],vy[3]-vy[1])
    base <- sqrt(v[1]^2+v[2]^2)
    points <- list(rep(0, 2))
    for (i in 1:2){
        a <- rbind(c(v[2],-v[1]), 
                   c(v[1],v[2]))
        b <- c(-(v[1]*vy[1]-v[2]*vx[1]),
               -(-v[2]*vy[2*i]-v[1]*vx[2*i]))
        p1 <- solve(a, b)
        points[[i]]<-c(p1[1],p1[2])
    }
    h1 <- sqrt((vx[2]-points[[1]][1])^2+(vy[2]-points[[1]][2])^2)
    h2 <- sqrt((vx[4]-points[[2]][1])^2+(vy[4]-points[[2]][2])^2)
    base*h1/2+base*h2/2
}

plotFig <- function(vx,vy){
  x <-c(vx[1]-1,vx[3]+1)
  y <-c(vy[1]-1,vy[3]+1) 
  plot (x,y,main = paste("Poligono"))
  polygon(vx,vy,col="orange",lty=1,lwd=2, border="blue")
}
