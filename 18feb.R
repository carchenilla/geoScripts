#Sistema de referencia y 

plotSys <- function(p,o,v1,v2){   #No tira bien
  xmin <- min(o[1],p[1])
  xmax <- max(o[1],p[1])
  ymin <- min(o[2],p[2])
  ymax <- max(o[2],p[2])
  v <- c(p[1]-o[1],p[2]-o[2])
  l <- sqrt(v[1]^2+v[2]^2)
  x = (o[1]-1:0.5:o[1]+l)
  y = (o[2]-1:0.5:o[2]+l)
  plot(x,y,main="Sistema de referencia")
  text(o[1],o[2],'o')
  arrows(o[1],o[2],o[1]+v1[1],o[2]+v1[2],col="green",lwd=3)
  arrows(o[1],o[2],o[1]+v2[1],o[2]+v2[2],col="blue",lwd=3)
  text(p[1],p[2],'p')
}


toNewRefSys <- function(p,o,v1,v2){
  A <- rbind(c(v1[1],v2[1]),
             c(v1[2],v2[2]))
  B<- c(p[1]-o[1],p[2]-o[2])
  solve(A,B)
}


getTraslation <- function(p,q){
  vx <- q[1]-p[1]
  vy <- q[2]-p[2]
  cbind(c(vx,vy))
}

getRotation <- function(p,o,a){
  M <- rbind(c(cos(a)*(p[1]-o[1])-sin(a)*(p[2]-o[2])+o[1]),
        c(sin(a)*(p[1]-o[1])+cos(a)*(p[2]-o[2])+o[2]))
  c(M[1][1],M[2][1])
}

getSimetryLine <-function(p,r){
  p1 <- c(1,r(1))
  p2 <- c(2,r(2))
  v <- c(p2[1]-p1[1],p2[2]-p1[2])
  m <- v[2]/v[1]
  n <- r(0)
  t = atan(m)
  mat1 =rbind(c(-n*sin(2*t)),
            c(n+n*cos(2*t)))
  mat2 = rbind(c(cos(2*t),sin(2*t)),
             c(sin(2*t),-cos(2*t)))
  mat3 = mat2%*%p
  sol = mat1+mat3
  c(sol[1],sol[2])
}


getHomotecy <- function(p,o,r){
  mat1 <- rbind(c(o[1]*(1-r)),
                c(o[2]*(1-r)))
  mat2 <- rbind(c(o[1]),
                c(o[2]))
  mat2 <- r*mat2
  mat1+mat2
}