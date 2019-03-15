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
