#PRACTICA 4.2 GEOMETRIA COMPUTACIONAL
#Ejercicios de clase del plano afin

toNewRefSys <- function(p,o,v1,v2){     #Cambio de referencia usando sistema con matrices
  A <- rbind(c(v1[1],v2[1]),     
             c(v1[2],v2[2]))
  B<- c(p[1]-o[1],p[2]-o[2])
  solve(A,B)
}

plotPointAndChangeRefSys <- function(p,o,v1,v2){ #Cambio de referencia y plot del punto 
  minX = min(c(v1[1],v2[1],p[1],o[1]))   #Valores min y max para que el plot encaje bien
  maxX = max(c(v1[1],v2[1],p[1],o[1]))
  minY = min(c(v1[2],v2[2],p[2],o[2]))
  maxY = max(c(v1[2],v2[2],p[2],o[2]))
  plot(c(minX-1,maxX+1),c(minY-1,maxY+1),main="sistema de referencia")
  text(o[1],o[2],'o')
  text(p[1],p[2],'X')
  arrows(o[1],o[2],o[1]+v1[1],o[2]+v1[2],col="orange",lwd=3)
  arrows(o[1],o[2],o[1]+v2[1],o[2]+v2[2],col="dark green",lwd=3)
  arrows(0,0,0,1,col="green",lwd=3)
  arrows(0,0,1,0,col="red",lwd=3)
  return(toNewRefSys(p,o,v1,v2))#Devolvemos el nuevo punto
}


getTraslationAndPlot <- function(p,q){
  minX = min(c(p[1],q[1]))   #Valores min y max para que el plot encaje bien
  maxX = max(c(p[1],q[1]))
  minY = min(c(p[2],q[2]))
  maxY = max(c(p[2],q[2]))
  plot(c(minX-1,maxX+1),c(minY-1,maxY+1),main="traslacion")
  text(q[1],q[2],'Q')
  text(p[1],p[2],'P')
  vx <- q[1]-p[1]
  vy <- q[2]-p[2]
  arrows(p[1],p[2],q[1],q[2],col="orange",lwd=3)
  vector <- cbind(c(vx,vy))   #usamos cbind para dar la matriz en forma de columna
  return(vector)#devolvemos el vector
}


getRotationAndPlot <- function(p,q,a){     #Angulo debe estar en radianes
  M <- rbind(c(cos(a)*(p[1]-q[1])-sin(a)*(p[2]-q[2])+q[1]),   #Matriz para hallar el nuevo punto
        c(sin(a)*(p[1]-q[1])+cos(a)*(p[2]-q[2])+q[2]))
  newP = c(M[1][1],M[2][1])
  minX = min(c(p[1],newP[1],q[1]))   #Valores min y max para que el plot encaje bien
  maxX = max(c(p[1],newP[1],q[1]))
  minY = min(c(p[2],newP[2],q[2]))
  maxY = max(c(p[2],newP[2],q[2]))
  plot(c(minX-1,maxX+1),c(minY-1,maxY+1),main="rotacion")
  arrows(q[1],q[2],p[1],p[2],col="orange",lwd=3)
  arrows(q[1],q[2],newP[1],newP[2],col="red",lwd=3)
  text(newP[1],newP[2],'nP')
  text(p[1],p[2],'P')
  text(q[1],q[2],'o')
  return(newP)#devolvemos el nuevo punto
}


getSimetryAndPlot <-function(p,r){
  p1 <- c(1,r(1))    #Calculo del simetrico a partir de la formula matricial
  p2 <- c(2,r(2))
  v <- c(p2[1]-p1[1],p2[2]-p1[2])
  n <- r(0)
  t = atan(v[2]/v[1])
  mat1 =rbind(c(-n*sin(2*t)),
            c(n+n*cos(2*t)))
  mat2 = rbind(c(cos(2*t),sin(2*t)),
             c(sin(2*t),-cos(2*t)))
  mat3 = mat2%*%p
  sol = mat1+mat3
  minX = min(c(p[1],sol[1]))   #Valores min y max para que el plot encaje bien
  maxX = max(c(p[1],sol[1]))
  minY = min(c(p[2],sol[2]))
  maxY = max(c(p[2],sol[2]))
  plot(c(minX-1,maxX+1),c(minY-1,maxY+1),main="simetria")   #Plot de la recta y los 2 puntos
  polygon(c(minX-1,maxX+1),r(c(minX-1,maxX+1)),lty=1,lwd=2, border="orange")
  text(p[1],p[2],'X')
  text(sol[1],sol[2],'X\'')
  return(sol)#devolvemos el nuevo punto
}

ecuacionesRectaAndPlot <- function(P,v){
  if(length(P)!=2&&length(v)!=2){
    stop("Parameters should be 2D point and vector")
  }else{
    print("Ecuación paramétrica:")
    print(paste("x=",P[1]," + λ*",v[1],sep=""))
    print(paste("y=",P[2]," + λ*",v[2],sep=""))
    
    print("")
    
    print("Ecuación continua:")
    print(paste("(x-",P[1],")/",v[1],"=(y-",P[2],")/",v[2],sep=""))
    
    print("")
    
    print("Ecuación explícita:")
    m=v[2]/v[1]
    print(paste("y=",m,"x+",P[2]-m*P[1],sep=""))
    
    print("")
    
    print("Ecuación general:")
    print(paste(v[2],"x+",-v[1],"y+",-v[2]*P[1]+v[1]*P[2],"=0",sep=""))
    plot(c(0,5),c(v[1],5+v[2]),main="recta")   #Plot de la recta
    polygon(c(0,5),c(v[1],5+v[2]),lty=1,lwd=2, border="orange")
  }
}