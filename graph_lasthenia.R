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

arrows(x0=0.989, x1=0.989, y0=4.6, y1=8, length=0, lty=1, lwd=3, col="red")
#Bromus
text(x=0.18, y=1.3, "Late")
text(x=0.47, y=1.3, "Early")
text(x=0.35, y=0.9, "Middle")

#Lactuca
text(x=0.87, y=5, "Early")
text(x=0.87, y=4.65, "Middle")
text(x=0.89, y=4.3, "Late")

text(x=0.8,y=7.5, "Exclusion", font=3)
text(x=0.8,y=0.2, "Coexistence", font=3)
detach(phe)

attach(alt)
plot(Niche.differences[1:3], log(1/Fitness.differences[1:3]), pch=16, lwd=1, xlab="Niche differences", ylab=" ", main= "(B) Height", ylim=c(0,8), xlim=c(0,1))
points(Niche.differences[4:6], log(1/Fitness.differences[4:6]), pch=1, lwd=2)
curve(log(1/(1-x)), add=T, col="red", lwd=3)
legend(0, 8, c("Bromus", "Lactuca"), col = c(1, 1), lwd=c(2,2), lty=c(0,0),  pch=c(16,1), bty="n",
       merge = TRUE)
arrows(x0=0.989, x1=0.989, y0=4.6, y1=8, length=0, lty=1, lwd=3, col="red")
#Bromus
text(x=0.18, y=2, "Small")
text(x=0.19, y=1.4, "Medium")
text(x=0.18, y=0.54, "Tall")

#Lactuca
text(x=0.87, y=5.9, "Small")
text(x=0.86, y=5.5, "Medium")
text(x=0.9, y=4.6, "Tall")

text(x=0.8,y=7.5, "Exclusion", font=3)
text(x=0.8,y=0.2, "Coexistence", font=3)

detach(alt)


