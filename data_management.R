mydata<-data.frame(x1 - c(2, 2, 6, 4),x2 = c(3, 4, 2, 8))
mydata$sum<-mydata$x1+mydata$x2
mydata<-transform(mydata,sum=x1+x2,mean=(x1+x2)/2)
mydata
manager<-c(1,2,3,4,5)
date<-c("10/24/08", "10/28/08", "10/1/08", "10/12/08", "5/1/09")
country<-c("US","US","UK","UK","UK")
gender<-c("M","F","F","M","F")
age<-c(32,45,25,39,99)
q1<-c(5,3,3,3,2)
q2<-c(4,5,5,3,2)
q3<-c(5,2,5,4,1)
q4<-c(5,5,5,NA,2)
q5<-c(5,5,2,NA,1)
leadership<-data.frame(manager,date,country,gender,age,q1,q2,q3,q4,q5,stringsAsFactors = FALSE)
leadership$age[leadership$age == 99] <-NA
leadership$agecat[leadership$age>75]<-"Elder"
leadership$agecat[leadership$age>=55 & leadership$age<=75]<-"Middle Age"
leadership$agecat[leadership$age <55] <-"Young"
library(car)
?recode()
x<-rep(1:5,3)
x
y<-recode(x,"c(1,3,5)='A';else='B'")
y
a<-c(75:100)
b<-c(55,74)
c<-c(1:54)
leadership$agecat<-recode(leadership$age,"a ='Elder';b='Middle age';else='Young'")
leadership$agecat
fix(leadership)
leadership
mydate<-as.Date(c("2019-03-11","2019-03-10"))
mydate
strdate<-c("11-03-2018","10-03-2018")
mydate1<-as.Date(strdate,"%d-%m-%Y")
mydate1
Sys.Date()
date()
order(leadership$age)
leadership[order(-leadership$age),]
?order()
leadership[2,1:11]
a<-c(1,2,3)
b<-c(4,5)
b<-append(b,6)
c<- c(8,9,7)
mergetry<-rbind(a,b)
mergetry
mergetry2<-cbind(a,b)
mergetry2
newdata <- leadership[, 6:10]
newdata
newdata <- leadership[c("q1", "q2", "q3", "q4", "q5")]
newdata
myvars <- paste("q", 1:5, sep="")
myvars
help("paste")
paste(1,2,sep = "")
newdata <- leadership[c(-8,-9)]
newdata
leadership$q3 <- leadership$q4 <- NULL
leadership
?subset()
?subset()
mysample <- leadership[sample(1:4, 3, replace=FALSE),]
nrow(leadership)
mysample
library("sampling")
install.packages("sqldf")
library(sqldf)
newdf<-sqldf("select * from mtcars where carb=1 order by mpg")
newdf
