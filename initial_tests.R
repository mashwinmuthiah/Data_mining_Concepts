library(carData)
library(car)
library(ggplot2)
library(GGally)
library(dummies)


# Raw dataset
rawdataset <- read.csv("C:\\Users\\ashwi\\Desktop\\Project\\UCI_Credit_Card.csv")

# Removed ID
rawdataset <- rawdataset[,-1]

new.data <- rawdataset
new.data$LIMIT_BAL <- log(rawdataset$LIMIT_BAL)

# Applied log to LIMIT_BAL
par(mfcol=c(1,1))
par(mfcol=c(2,2))
plot(density(rawdataset$LIMIT_BAL),main= "Desnity plot Before log")
plot(density(log(rawdataset$LIMIT_BAL)),main= "Desnity plot After log")
boxplot(log(rawdataset$LIMIT_BAL))


# Education groups
table(new.data$SEX)
table(new.data$EDUCATION)
aggregate(cbind(AGE, LIMIT_BAL) ~  EDUCATION, data=rawdataset, FUN=mean)
aggregate(cbind(AGE, LIMIT_BAL) ~ EDUCATION, data=rawdataset, FUN=median)

# Marriage
table(rawdataset$MARRIAGE)
aggregate(cbind(AGE, LIMIT_BAL) ~ MARRIAGE, data=rawdataset, FUN=mean)
aggregate(cbind(AGE, LIMIT_BAL) ~ default.payment.next.month + MARRIAGE, data=rawdataset, FUN=median)

# Repayment Status
par(mfrow = c(1,1))
plot(table(rawdataset$PAY_0, rawdataset$default.payment.next.month))
plot(table(rawdataset$PAY_6, rawdataset$default.payment.next.month))

summary(rawdataset$PAY_AMT1)
boxplot(rawdataset$PAY_AMT1)
plot(density(rawdataset$PAY_AMT1))
plot(density(log(rawdataset$PAY_AMT1)))
boxplot(log(rawdataset$PAY_AMT1))

boxplot(rawdataset$PAY_AMT3)
plot(density(rawdataset$PAY_AMT3))
plot(density(log(rawdataset$PAY_AMT3)))
boxplot(log(rawdataset$PAY_AMT3))

cor(rawdataset$PAY_AMT1,rawdataset$LIMIT_BAL)


summary(new.data)

# Dummy columns
rawdataset$SEX_Male <- dummy(rawdataset$SEX, sep = "_")[,1]
rawdataset[,c("SEX","SEX_Male")]

rawdataset <- cbind(rawdataset, dummy(rawdataset$EDUCATION, sep = '_'))
ggpairs(data = rawdataset,
        columns = c(24,26,27,28,29,30,31,32))

