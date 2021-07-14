#This script simulates the Markov chain that models the Gambler's Ruin
#The gambler’s objective is to reach a total fortune of N, 
#without first getting ruined (running out of money).
p<-.4
q<-1-p
#Initial fortune
X0<-10
#Total fortune
N<-30
#n Steps andm trajectories
m<-3
n<-50
Xnm<-matrix(X0,nrow=m,ncol=n+1)
for(rind in 1:m){
  xis<-sample(c(-1,1),size = n,prob = c(p,q),replace = TRUE)
  for(nind in 1:n) 
    if(Xnm[rind,nind]>0 & Xnm[rind,nind]<N)Xnm[rind,nind+1]<- Xnm[rind,nind]+xis[nind]
    else Xnm[rind,nind+1]<- Xnm[rind,nind]
}

#Plot usando paqueterias adicionales
library(ggplot2)
library(reshape2)

#ggplot needs a dataframe
data <- as.data.frame(t(Xnm))
data$n <- 0:n 
plot_data <- melt(data,id.var="n")

ggplot(plot_data, aes(x=n,y=value,group=variable,colour=variable)) +
  geom_point(cex=2)+
  geom_line(lwd=1)+
  ggtitle(paste0("Gambler's Ruin (N=",N," , X0=", X0,")"))+ylab(expression(X[n]))+
  theme(legend.position = "none",plot.title = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(angle = 0,hjust = 1, vjust = 0.5))


#matplot(0:n,t(Xnm),type="b",pch=20,lty=1,xlab = "n",ylab=expression(X[n]),
#        main=paste0("Ruina del jugador (N=",N," , X0=", X0,")"))
