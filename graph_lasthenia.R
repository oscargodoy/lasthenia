#graph for lasthenia Paper
#loading the data for phenology and height
phe<-read.table('Plot_Fenologia.txt', header=T, sep="\t")
alt<-read.table('Plot_Altura.txt', header=T, sep="\t")

#First phenology and later height
par(mfrow=c(1,2))

attach(phe)
plot(Niche.differences[1:3], log(1/Fitness.differences[1:3] + 1), pch=16, lwd=1, xlab="Niche differences", ylab="Fitness differences (log. transformed)", main= "(A) Phenology", ylim=c(0,8), xlim=c(0,1))
points(Niche.differences[4:6], log(1/Fitness.differences[4:6]), pch=1, lwd=2)
curve(log(1/(1-x)), add=T, col="red", lwd=3)
legend(0, 8, c("Bromus", "Lactuca"), col = c(1, 1), lwd=c(1,1), lty=c(0,0),  pch=c(16,1), bty="n",
       merge = TRUE)
#Bromus
text(x=0.18, y=1.3, "Early")
text(x=0.5, y=1.3, "Middle")
text(x=0.4, y=0.9, "Late")

#Lactuca
text(x=0.87, y=5, "Early")
text(x=0.87, y=4.65, "Middle")
text(x=0.89, y=4.3, "Late")

detach(phe)

attach(alt)
plot(Niche.differences[1:3], log(1/Fitness.differences[1:3]), pch=16, lwd=1, xlab="Niche differences", ylab=" ", main= "(B) Height", ylim=c(0,8), xlim=c(0,1))
points(Niche.differences[4:6], log(1/Fitness.differences[4:6]), pch=1, lwd=2)
curve(log(1/(1-x)), add=T, col="red", lwd=3)
legend(0, 8, c("Bromus", "Lactuca"), col = c(1, 1), lwd=c(2,2), lty=c(0,0),  pch=c(16,1), bty="n",
       merge = TRUE)
text
detach(alt)


