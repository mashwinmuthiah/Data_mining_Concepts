Student <- c("John Davis","Angela Williams","Bullwinkle Moose",
             "David Jones","Janice Markhammer",
             "Cheryl Cushing","Reuven Ytzrhak",
             "Greg Knox","Joel England","Mary Rayburn")
math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
english <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)
roster <- data.frame(Student,math,science,english,
                     stringsAsFactors = FALSE)
x<-c(1,2,3,4,5,6,7,8)
mean(x,trim = 0.5,na.rm = TRUE)
sd(x)
set.seed(1234)
runif(5)
library(MASS)
mean <- c(230.7, 146.7, 3.6)                                           
sigma <- matrix( c(15360.8, 6721.2, -47.1,                              
                   6721.2, 4700.9, -16.5,
                   -47.1,  -16.5,   0.3), nrow=3, ncol=3)
set.seed(1234)
mydata <- mvrnorm(500, mean, sigma)   
mydata<- as.data.frame(mydata)
names(mydata)<-c(x,y,z)
apply(mydata,1,mean)
?apply()
x<-pretty(c(-3,3),30)
length(x)
y<-dnorm(x)
plot(x,y,type = "l")
x <- c("ab", "cde", "fghij")
nchar(x)
length(x)
x<-"ashwin"
substr(x,2,4)
substr(x,2,4)<-'222'
substr(x,1,1) <- toupper(toupper(substr(x,1,1)))
mydata<-matrix(rnorm(30),nrow = 6)
mydata
round(apply(mydata,1,mean),2)
sample(1:10,1)
x<-list(sample(1:100,5))
set.seed(1234)
sapply(x,mean)
mean(x)
x<-sample(1:100,5)
mean(x)
options(digits = 2)
roster
z<-scale(roster[,2:4])
z
score<-apply(z,1,mean)
score
roster<-cbind(roster,score)
roster
y<-quantile(roster$score,c(.8,.6,.4,.2))
y
roster$grade[roster$score>=y[1]]<-"A"
roster$grade[roster$score<y[1]&roster$score>=y[2]]<-"B"
roster$grade[roster$score<y[2]&roster$score>=y[3]]<-"C"
roster$grade[roster$score<y[3]&roster$score>=y[4]]<-"D"
roster$grade[roster$score<y[4]]<-"F"
roster
name <-strsplit(roster$Student," ")
name
firstname<-sapply(name, "[",1)
lastname<-sapply(name,"[",2)
roster<-cbind(firstname,lastname,roster[,-1])
roster[order(lastname,firstname),]
for (i in 1:10) print("Hello")
i <-10
while (i>0) {print("hello");i<-i-1}
mystats<-function(x,parametric=TRUE,print = FALSE){
  if(parametric){
    center<-mean(x);spread<-sd(x)
  }else{center<-median(x);spread<-mad(x)
  }
  if(print&parametric){
    cat("Mean=", center, "\n", "SD=", spread, "\n")
  } else if (print & !parametric) {
    cat("Median=", center, "\n", "MAD=", spread, "\n")
  }
  results<-list(center = center,spread = spread)
  return(results)
}
x<-rnorm(500)
y<-mystats(x)
y$center
y <- mystats(x, parametric=FALSE, print=TRUE)
y
mydate<-function(type="long") {
  message("hi")
switch(type,long = format(Sys.time(),"%A %B %d %Y"),
       short = format(Sys.time(), "%m-%d-%y"),
            cat(type,"is not recoganized type\n"))
}
mydate("long")
mydate("oko")
mydate("short")
?warning()
?mtcars
cars<-mtcars[1:4,1:4]
mtcars[1:4,1:4]
t(cars)
agdata<-aggregate(cars,by=list(cars$mpg,cars$cyl),mean)
agdata
install.packages("reshape2")
mydata <- read.table(header=TRUE, sep=" ", text="
ID Time X1 X2
1 1 5 6
1 2 3 5
2 1 6 1
2 2 2 4
")
library(reshape2)
md<-melt(mydata,id=c("ID","Time"))
md
dcast(md, ID~variable)
dcast(md, ID+Time~variable)
dcast(md, ID+variable~Time)
