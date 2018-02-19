#Load the alphas and the vital rates

read.csv("lbes_lasthenia_bromus.txt", header=T, row.names = 1, sep="\t") -> lbes
read.csv("lasthenia_bromus_early.txt", header=T, row.names = 1, sep="\t") -> alphas

#create an empty matrix to store the niche differences data
nd_fd <-matrix(NA, nrow= 6, ncol=4)
colnames(nd_fd) <- c("species", "range", "niche_diff", "fit_diff")
nd_fd[,1] <- c("Bromus", "Bromus", "Bromus", "Lactuca","Lactuca","Lactuca")
nd_fd[,2] <- c("Early", "Middle", "Late", "Early", "Middle", "Late")

##Compute niche differences first
##Calculate niche differences only for the experimental alphas
sp_names<-row.names(lbes)
nd<- matrix(NA, nrow= 2, ncol=2)
rownames(nd)<-sp_names
colnames(nd)<-sp_names

for( i in 1:2){
  for(j in 1:2){
    
    nd[i,j] <- 1-(sqrt((alphas[i,j]*alphas[j,i])/(alphas[i,i]*alphas[j,j])))                             
  }
}

##Set negative nd to zero
#nd[nd<0]<-0
#load the value into the matrix
nd_fd[1,3]<-nd[1,2]

read.csv("lasthenia_bromus_middle.txt", header=T, row.names = 1, sep="\t") -> alphas

sp_names<-row.names(lbes)
nd<- matrix(NA, nrow= 2, ncol=2)
rownames(nd)<-sp_names
colnames(nd)<-sp_names

for( i in 1:2){
  for(j in 1:2){
    
    nd[i,j] <- 1-(sqrt((alphas[i,j]*alphas[j,i])/(alphas[i,i]*alphas[j,j])))                             
  }
}

##Set negative nd to zero
#nd[nd<0]<-0
#load the value into the matrix
nd_fd[2,3]<-nd[1,2]

read.csv("lasthenia_bromus_late.txt", header=T, row.names = 1, sep="\t") -> alphas

sp_names<-row.names(lbes)
nd<- matrix(NA, nrow= 2, ncol=2)
rownames(nd)<-sp_names
colnames(nd)<-sp_names

for( i in 1:2){
  for(j in 1:2){
    
    nd[i,j] <- 1-(sqrt((alphas[i,j]*alphas[j,i])/(alphas[i,i]*alphas[j,j])))                             
  }
}

##Set negative nd to zero
nd[nd<0]<-0
#load the value into the matrix
nd_fd[3,3]<-nd[1,2]


read.csv("lbes_lasthenia_lactuca.txt", header=T, row.names = 1, sep="\t") -> lbes
read.csv("lasthenia_lactuca_early.txt", header=T, row.names = 1, sep="\t") -> alphas

sp_names<-row.names(lbes)
nd<- matrix(NA, nrow= 2, ncol=2)
rownames(nd)<-sp_names
colnames(nd)<-sp_names

for( i in 1:2){
  for(j in 1:2){
    
    nd[i,j] <- 1-(sqrt((alphas[i,j]*alphas[j,i])/(alphas[i,i]*alphas[j,j])))                             
  }
}

##Set negative nd to zero
#nd[nd<0]<-0
#load the value into the matrix
nd_fd[4,3]<-nd[1,2]

read.csv("lasthenia_lactuca_middle.txt", header=T, row.names = 1, sep="\t") -> alphas

sp_names<-row.names(lbes)
nd<- matrix(NA, nrow= 2, ncol=2)
rownames(nd)<-sp_names
colnames(nd)<-sp_names

for( i in 1:2){
  for(j in 1:2){
    
    nd[i,j] <- 1-(sqrt((alphas[i,j]*alphas[j,i])/(alphas[i,i]*alphas[j,j])))                             
  }
}

##Set negative nd to zero
#nd[nd<0]<-0
#load the value into the matrix
nd_fd[5,3]<-nd[1,2]

read.csv("lasthenia_lactuca_late.txt", header=T, row.names = 1, sep="\t") -> alphas

sp_names<-row.names(lbes)
nd<- matrix(NA, nrow= 2, ncol=2)
rownames(nd)<-sp_names
colnames(nd)<-sp_names

for( i in 1:2){
  for(j in 1:2){
    
    nd[i,j] <- 1-(sqrt((alphas[i,j]*alphas[j,i])/(alphas[i,i]*alphas[j,j])))                             
  }
}

##Set negative nd to zero
#nd[nd<0]<-0
#load the value into the matrix
nd_fd[6,3]<-nd[1,2]


#Now the fitness differences

#Compute fitness differences
#Demographic differences

read.csv("lbes_lasthenia_bromus.txt", header=T, row.names = 1, sep="\t") -> lbes
read.csv("lasthenia_bromus_early.txt", header=T, row.names = 1, sep="\t") -> alphas

dd <-outer(lbes$eta -1, lbes$eta -1, FUN="/")
rownames(dd)<-sp_names
colnames(dd)<-sp_names

#Competitive response differences

crd<- matrix(NA, nrow= 2, ncol=2)
rownames(crd)<-sp_names
colnames(crd)<-sp_names

for( i in 1:2){
  for(j in 1:2){
    
    crd[i,j] <- (sqrt((alphas[j,i]*alphas[j,j])/(alphas[i,i]*alphas[i,j])))                             
  }
}

# Multiply demographic differences by competitive response differences to obtain an estimate of average fitness differences. 

fd<-log(dd*crd)

nd_fd[1,4]<-fd[1,2]


read.csv("lasthenia_bromus_middle.txt", header=T, row.names = 1, sep="\t") -> alphas

dd <-outer(lbes$eta -1, lbes$eta -1, FUN="/")
rownames(dd)<-sp_names
colnames(dd)<-sp_names

#Competitive response differences

crd<- matrix(NA, nrow= 2, ncol=2)
rownames(crd)<-sp_names
colnames(crd)<-sp_names

for( i in 1:2){
  for(j in 1:2){
    
    crd[i,j] <- (sqrt((alphas[j,i]*alphas[j,j])/(alphas[i,i]*alphas[i,j])))                             
  }
}

# Multiply demographic differences by competitive response differences to obtain an estimate of average fitness differences. 

fd<-log(dd*crd)

nd_fd[2,4]<-fd[1,2]

read.csv("lasthenia_bromus_late.txt", header=T, row.names = 1, sep="\t") -> alphas

dd <-outer(lbes$eta -1, lbes$eta -1, FUN="/")
rownames(dd)<-sp_names
colnames(dd)<-sp_names

#Competitive response differences

crd<- matrix(NA, nrow= 2, ncol=2)
rownames(crd)<-sp_names
colnames(crd)<-sp_names

for( i in 1:2){
  for(j in 1:2){
    
    crd[i,j] <- (sqrt((alphas[j,i]*alphas[j,j])/(alphas[i,i]*alphas[i,j])))                             
  }
}

# Multiply demographic differences by competitive response differences to obtain an estimate of average fitness differences. 

fd<-log(dd*crd)

nd_fd[3,4]<-fd[1,2]


read.csv("lbes_lasthenia_lactuca.txt", header=T, row.names = 1, sep="\t") -> lbes
read.csv("lasthenia_lactuca_early.txt", header=T, row.names = 1, sep="\t") -> alphas

dd <-outer(lbes$eta -1, lbes$eta -1, FUN="/")
rownames(dd)<-sp_names
colnames(dd)<-sp_names

#Competitive response differences

crd<- matrix(NA, nrow= 2, ncol=2)
rownames(crd)<-sp_names
colnames(crd)<-sp_names

for( i in 1:2){
  for(j in 1:2){
    
    crd[i,j] <- (sqrt((alphas[j,i]*alphas[j,j])/(alphas[i,i]*alphas[i,j])))                             
  }
}

# Multiply demographic differences by competitive response differences to obtain an estimate of average fitness differences. 

fd<-log(dd*crd)

nd_fd[4,4]<-fd[1,2]

read.csv("lasthenia_lactuca_middle.txt", header=T, row.names = 1, sep="\t") -> alphas

dd <-outer(lbes$eta -1, lbes$eta -1, FUN="/")
rownames(dd)<-sp_names
colnames(dd)<-sp_names

#Competitive response differences

crd<- matrix(NA, nrow= 2, ncol=2)
rownames(crd)<-sp_names
colnames(crd)<-sp_names

for( i in 1:2){
  for(j in 1:2){
    
    crd[i,j] <- (sqrt((alphas[j,i]*alphas[j,j])/(alphas[i,i]*alphas[i,j])))                             
  }
}

# Multiply demographic differences by competitive response differences to obtain an estimate of average fitness differences. 

fd<-log(dd*crd)

nd_fd[5,4]<-fd[1,2]


read.csv("lasthenia_lactuca_late.txt", header=T, row.names = 1, sep="\t") -> alphas

dd <-outer(lbes$eta -1, lbes$eta -1, FUN="/")
rownames(dd)<-sp_names
colnames(dd)<-sp_names

#Competitive response differences

crd<- matrix(NA, nrow= 2, ncol=2)
rownames(crd)<-sp_names
colnames(crd)<-sp_names

for( i in 1:2){
  for(j in 1:2){
    
    crd[i,j] <- (sqrt((alphas[j,i]*alphas[j,j])/(alphas[i,i]*alphas[i,j])))                             
  }
}

# Multiply demographic differences by competitive response differences to obtain an estimate of average fitness differences. 

fd<-log(dd*crd)

nd_fd[6,4]<-fd[1,2]


#Now lets do the plotting 
plot(nd_fd[1:3,3], nd_fd[1:3,4], pch=16, lwd=2, xlab="Niche differences", ylab="Fitness differences (log. transformed)",  ylim=c(0,8), xlim=c(0,1))
points(nd_fd[4:6,3], nd_fd[4:6,4], pch=1, lwd=2)
curve(log(1/(1-x)), add=T, col="red", lwd=3)
legend(0, 8, c("Bromus", "Lactuca"), col = c(1, 1), lwd=c(1,1), lty=c(0,0),  pch=c(16,1), bty="n",
       merge = TRUE)

arrows(x0=0.9895, x1=1, y0=4.6, y1=8, length=0, lty=1, lwd=3, col="red")

#Bromus
text(x=0.47, y=1.3, "Early")
text(x=0.35, y=0.9, "Middle")
text(x=0.18, y=1.3, "Late")

#Lactuca
text(x=0.87, y=5, "Early")
text(x=0.87, y=4.65, "Middle")
text(x=0.89, y=4.3, "Late")

text(x=0.8,y=7.5, "Exclusion", font=3)
text(x=0.8,y=0.2, "Coexistence", font=3)




