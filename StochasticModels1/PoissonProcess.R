#This script simulate Poisson Processes homogeneous (HPP) and non homogeneous, 
#using either the library "poisson" or by transforming a HPP

library("poisson")
help("poisson")

par(mfrow=c(3,1))

n.events<-10
n.sim<-30
Times<-hpp.sim(rate = 5, num.events = n.events, num.sims = n.sim)
plot(Times[,1],0:n.events,type="s",ylab="N(t)",las=1,xlab="t",xlim=c(0,max(Times)))
for(i in 2:n.sim)lines(Times[,i],0:n.events,col=i,type="s",
                   main="PPH")


intensity <- function(t) 1.1-cos(2*pi*t/10)
Times.nhh<-nhpp.sim(rate=5, num.events=n.events, num.sims = n.sim,prob.func=intensity)

curve(intensity,0,10,xlab="t",ylab="lambda(t)")
plot(Times.nhh[,1],0:n.events,type="s",ylab="N(t)",las=1,xlab="t",xlim=c(0,max(Times.nhh)),
     main="PP non Homogeneous")
for(i in 2:n.sim)lines(Times.nhh[,i],0:n.events,col=i,type="s")

hist(Times,main="Histograma de los Tiempos de ocurrencia (Caso Homogeneo)")
hist(Times.nhh,main="Histograma de los Tiempos de ocurrencia (Caso No Homogeneo)")

#PP non homogeneous by transforming PPH

m.inv<-function(x){
  u<-seq(0,10,.001)
  y<-u[findInterval(x,cumsum(intensity(u))*.001)]
  return(y)
}

curve(intensity,0,10,xlab="t",ylab="lambda(t)")
curve(m.inv,.1,2.1,xlab="t",ylab="m.inv(t)")


Times.nhhT<-rbind(rep(0,n.sim),apply(Times,2,m.inv))

plot(Times[,1],0:n.events,type="s",ylab="N(t)",las=1,xlab="t",xlim=c(0,max(Times)))
for(i in 2:n.sim)lines(Times[,i],0:n.events,col=i,type="s",
                       main="PPH")

plot(Times.nhhT[,1],0:n.events,type="s",ylab="N(t)",las=1,xlab="t",xlim=c(0,max(Times.nhh)),
     main="PP non Homogeneous")
for(i in 2:n.sim)lines(Times.nhhT[,i],0:n.events,col=i,type="s")
