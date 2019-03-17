#barplot
?barplot()
library(vcd)
opar<-par(no.readonly = TRUE)
mydata<-Arthritis
count<-table(mydata$Improved)
barplot(count,horiz = TRUE,main = "Horizontal Bar plot",xlab = "Frequency",ylab = "Improvement")
barplot(count,main = "Simple Bar plot",ylab = "Frequency",xlab = "Improvement")
plot(mydata$Improved,main = "Simple Bar plot",ylab = "Frequency",xlab = "Improvement")
plot(mydata$Improved,horiz = TRUE,main = "Simple Bar plot",xlab = "Frequency",ylab = "Improvement")
attach(Arthritis)
View(Arthritis)
count<-table(Improved,Treatment)
count
barplot(count)
library(Hmisc)
barplot(count,beside = TRUE,col = rainbow(3),main="Grouped Bar Plot")
legend(locator(1),title = "Key",c(row.names(count)),pch = 15 ,col = rainbow(3))
states<-data.frame(state.region,state.x77)
View(states)
means<-aggregate(states$Illiteracy,by=list(states$state.region),FUN = mean)
means
means<-means[order(means$x),]
means
par(mar = c(4,4,1,1))
par(las = 1)
barplot(means$x,names.arg = means$Group.1,col = rainbow(4))
legend(locator(1),title = "Key",legend =  c("North Central","Northeast","West","South"),pch = 15,col = rainbow(4))

#spinogram
attach(Arthritis)
count<-table(Treatment,Improved)
spine(count,main="Spinogram Treatment vs Impovement")

#Histogram
opar<-par(no.readonly = TRUE)
par(mfrow = c(2,2))
hist(Arthritis$Age)
par(opar)
par(mfrow = c(2,2))
hist(mtcars$mpg)
hist(mtcars$mpg,breaks = 12,col = "red",main = "Histogram of MPG",xlab = "MPG")
hist(mtcars$mpg,breaks = 12,col = "red",main = "Histogram of MPG",xlab = "MPG",freq = FALSE)
rug(jitter(mtcars$mpg))
lines(density(mtcars$mpg),col="blue",lwd = 2)
hist(mtcars$mpg,breaks = 12,col = "red",main = "Histogram of MPG",xlab = "MPG")
x<-mtcars$mpg
xfit<-seq(min(x),max(x),length = 40)
xfit
yfit<-dnorm(xfit,mean = mean(x),sd = sd(x))
yfit<-yfit*diff(hsmids[1:2])*length(x)
box()

#kernal density plot
d<-density(mtcars$mpg)
plot(d)
View(mtcars[order(mtcars$mpg),])
install.packages("sm")
library(sm)
par(opar)
sm.density.compare(mtcars$mpg,mtcars$cyl,lwd = 2)

#box plot
boxplot(mtcars$mpg,main="Box plot",ylab = "MPG")
boxplot.stats(mtcars$mpg)
boxplot(mpg~cyl,data = mtcars,main = "Box plot")
boxplot(mpg~cyl,data = mtcars,notch = TRUE,main = "Notched box plot", varwidth = TRUE)
mtcars$cyl.f<-factor(mtcars$cyl,levels = c(4,6,8),labels = c("4","6","8"))
mtcars$am.f<-factor(mtcars$am,levels = c(0,1),labels = c("auto","standard"))
boxplot(mpg~am.f*cyl.f,data = mtcars,main = "Conbined box plot",col = c("red","blue"),ylim = c(10,35))

#violin plot
install.packages("vioplot")                     
x1<-mtcars$mpg[mtcars$cyl == 4]
x2<-mtcars$mpg[mtcars$cyl == 6]
x3<-mtcars$mpg[mtcars$cyl == 8]
library(vioplot)
vioplot(x1,x2,x3,col = rainbow(3))

#dot chart
dotchart(mtcars$mpg, labels=row.names(mtcars),cex = 0.8)
x<-mtcars[order(mtcars$mpg),]
x$cyl <-factor(x$cyl)
x$color[x$cyl==4]<-"red"
x$color[x$cyl==6]<-"blue"
x$color[x$cyl==8]<-"green"
dotchart(x$mpg,label = row.names(x),col = x$color,groups = x$cyl)
