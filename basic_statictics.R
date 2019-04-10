myvar<-c("mpg",'hp',"wt")
data<-mtcars[myvar]
summary(mtcars[myvar])
sapply(mtcars[myvar],mean)

mystats<-function(x){
  m<-mean(x)
  n<-length(x)
  s<-sd(x)
  skew<-sum((x-m)^3/s^3)/n
  kurt<-sum((x-m)^4/s^4)/n-3
  return(c(n=n,mean=m,stdev=s,skew=skew,kurtosis=kurt))
}
library(Hmisc)
sapply(mtcars[myvar],mystats)
describe(mtcars[myvar])
install.packages("pastecs")
library(pastecs)
stat.desc(data)
library(psych)
describe(data)
Hmisc::describe(data)

aggregate(data,by = list(am = mtcars$am),sd)
aggregate(data,by = list(am = mtcars$am),mean)
dstat<-function(x){
  print(x)
  sapply(x, mystats)
}
by(data,mtcars$am,dstat)
install.packages("doBy")
library(doBy)
doBy::summaryBy(mpg+hp+wt~am,data = mtcars,FUN = mean)

psych::describeBy(data,list(am=mtcars$am))

psych::describe.by(data,list(am=mtcars$am))

library(vcd)
head(Arthritis)

table(Arthritis$Improved)
prop.table(table(Arthritis$Improved))*100

table(Arthritis$Improved,Arthritis$Sex)

prop.table(xtabs(~Arthritis$Improved+Arthritis$Sex))*100       

margin.table(table(Arthritis$Treatment),2)
             
addmargins(prop.table(xtabs(~Arthritis$Improved+Arthritis$Sex))*100,2)

library(gmodels)
CrossTable(Arthritis$Treatment,Arthritis$Improved)


table(Arthritis$Treatment,Arthritis$Sex,Arthritis$Improved)
ftable(addmargins(xtabs(~Arthritis$Treatment+Arthritis$Sex+Arthritis$Improved)))

ftable(addmargins(prop.table(table(Arthritis$Treatment,Arthritis$Sex,Arthritis$Improved))))

chisq.test(xtabs(~Arthritis$Treatment+Arthritis$Sex))
fisher.test(xtabs(~Arthritis$Treatment+Arthritis$Improved))
mantelhaen.test(xtabs(~Arthritis$Treatment+Arthritis$Improved+Arthritis$Sex))

assocstats(xtabs(~Arthritis$Treatment+Arthritis$Sex))
