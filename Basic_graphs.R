pdf("mygraph.pdf")
cars<- mtcars
plot(cars$wt,cars$mpg)
with(cars,{abline(lm(mpg~wt))})
title("Regression of MPG on weight")
dev.off()
dose  <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
plot(dose,drugA,type = "b")
help(plot)
opar<-par(no.readonly = TRUE)
opar
help(par)
par(lty = 2,pch = 17)
plot(dose,drugA,type = "b")
par(opar)
plot(dose,drugA,type = "b")
plot(dose,drugA,type = "b",lty = 2,pch = 17,cex = 1.5,lwd = 2,col = c("blue", brewer.pal(3,"BuGn")))
install.packages("RColorBrewer")
library(RColorBrewer)
help(RColorBrewer)
n <- 7
mycolors <- brewer.pal(n, "Set2")
bar<-c(1,2,3,4,5,6,7,8,9)
barplot(bar, col=mycolors)
n <- 10
mycolors <- rainbow(n)
pie(rep(1, n), labels=mycolors, col=mycolors)
mygrays <- gray(0:n/n)
pie(rep(1, n), labels=mygrays, col=mygrays)
par(font.lab=3, cex.lab=1.5, font.main=4, cex.main=2)
barplot(bar , col= rainbow(10) , main = "Bar plot",xlab = "X-axis",ylab="y-axis")
par(opar)
barplot(bar , col= rainbow(10) , main = "Bar plot",xlab = "X-axis",ylab="y-axis")
windowsFonts(A=windowsFont("Arial Black"),
             B=windowsFont("Bookman Old Style"),
             C=windowsFont("Comic Sans MS"))
par(opar)
par(pin=c(2,3))
par(cex=1.5)
par(cex.axis = 1.5,font.axis = 3)
plot(dose,drugA,type="b",pch = 19,lty=2,col=c("red",rainbow(10)))
plot(dose,drugB,type="b",pch = 23,lty=6,col=c("blue",rainbow(10)))
x<-c(1:10)
y<-x
z<-10/x
par(mar = c(5,4,4,8)+0.1)
plot(x,y,type = "b",pch = 21,col="red",yaxt = "n",lty = 3,ann = FALSE)
lines(x,z,type = "b",pch = 22,col="blue",lty = 2)
axis(2,at=x,lables = x,col.axis = "red",las = 2)
axis(4,at=z,labels = round(z,digits = 2),col.axis = "blue",las = 2,cex.axis = 0.7,tick = -0.1)
title("Creating Axis",xlab = "X values",ylab = "Y=X")
abline(v=seq(1, 10, 2), lty=2, col="blue")
library(Hmisc)
par(opar)
x<- c(1:7)
y<- c(8:14)
plot(x,y,ylim=c(0, 14))
abline(h=y,lty = 2, col = "red")
abline(v=x,lty = 2, col = "red")
help("legend")
par(opar)
par(lwd = 2, cex = 1.5,font.lab = 2)
plot(dose,drugA,type = "b",pch = 15,lty = 1 ,col="red",ylim = c(0,60),main = "Drug A vs Drug B",xlab = "Drug Dosage",ylab = "Drug Response")
abline(h=c(30),lwd = 1.5,lty = 2,col = "gray")
lines(dose,drugB,type = "b",pch = 17,lty = 2,col = "blue")
library(Hmisc)
legend("topleft",inset =.01,title = "Drug Type",c("A","B"),lty = c(1,2),pch = c(15,17),col = c("red","blue"))
par(opar)
help("legend")
attach(mtcars)
plot(wt,mpg)
text(wt,mpg,row.names(mtcars),cex = 0.6,pos = 4 , col = "red")
help("plotmath")
attack(mtcars)
par(mfrow = c(2,2))
plot(wt,mpg)
plot(wt,disp)
hist(wt)
boxplot(wt)
par(opar)
detach(mtcars)
help("layout")
attach(mtcars)
opar<-par(no.readonly = TRUE)
layout(matrix(c(1,2,3,3),2,2,byrow = FALSE))
hist(wt)
hist(mpg)
hist(disp)
