age <- c(1,3,5,2,11,9,3,9,12,3)
weight <- c(4.4,5.3,7.2,5.2,8.5,7.3,6.0,10.4,10.2,6.1)
mean(weight)
sd(weight)
cor(age,weight)
plot(age,weight)
getwd()
setwd("C:/Users/ashwi/Desktop/Ashwin/Data_mining_Concepts")
ls()
options()
history(2)
library()
.libPaths()
search()
install.packages("gclus")
help(package = "gclus")
help.start()
install.packages("vcd")
library(vcd)
data("Arthritis")
Arthritis
help(Arthritis)
example(Arthritis)

a<-c(1,2,3,4,5,6,7,8,9)
b <- c("a","c","c","d","e","f","g","h","i")
c<-c(10:18)
a[3]
a[c(1,2,3)]
a[1:6]
rnames<-c("r1","r2","r3")
cnames<-c("c1","c2","c3")
mymatrix<- matrix(a,nrow = 3,ncol = 3,byrow = TRUE,dimnames = list(c("r1","r2","r3"),c("c1","c2","c3")))
mymatrix[,1]
mymatrix[1,]    
mymatrix[2,2]
mymatrix[1:2,2]
mymatrix[1,c(1:2)]
myarray<-array(1:24,c(2,3,4),dimnames(list(c("a1","a2"),c("b1","b2","b3"),c("c1","c2","c3","c4"))))
myarray[c(1,2)]
mydata<-data.frame(a,b,c)
mydata
table(mydata$a,mydata$b)
mtcars

patientID<-c(1,2,3,4)
age<-c(25,34,28,52)
diabetes<-c("type1","type2","type1","type1")
status<-c("poor","improved","excellent","poor")
diabetes<-factor(diabetes)
status<-factor(status,order = TRUE)
patientdata<-data.frame(patientID,age,diabetes,status)
patientdata
str(patientdata)
new_data<-data.frame(a,b,c)
new_data<=edit(mydata)
fix(new_data)
head(a,-1)
ls()
class(new_data)
class(a)
mode(a)
mode(new_data)
class(new_data)
names(new_data)[2] <- "renaning b"
new_data
