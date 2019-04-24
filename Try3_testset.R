library(caret)
library(lattice)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(rattle)
library(tree)
library(gmodels)
library(RColorBrewer)
#loding the data and viewing.
a_train<-read.csv("C:\\Users\\ashwi\\Desktop\\Project\\LogTrainingSet.csv")
a_valid<-read.csv("C:\\Users\\ashwi\\Desktop\\Project\\LogValidationSet.csv")

#change the numerical to factor
a_train$SEX<-as.factor(a_train$SEX)
a_train$EDUCATION<-as.factor(a_train$EDUCATION)
a_train$MARRIAGE<-as.factor(a_train$MARRIAGE)
a_valid$SEX<-as.factor(a_valid$SEX)
a_valid$EDUCATION<-as.factor(a_valid$EDUCATION)
a_valid$MARRIAGE<-as.factor(a_valid$MARRIAGE)
View(a_valid)

logit.class<-glm(default.payment.next.month ~LIMIT_BAL+AGE+PAY_0+PAY_2+PAY_3+
                   PAY_4+PAY_5+PAY_6+PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5+
                   PAY_AMT6+SEX+EDUCATION+
                   MARRIAGE,
                 family=binomial(link="logit"),data=a_train)
summary(logit.class)

logit.class<-glm(default.payment.next.month ~ LIMIT_BAL+PAY_0+PAY_2+PAY_3+PAY_6+PAY_AMT3+MARRIAGE,family=binomial(link="logit"),data=a_train)
summary(logit.class)

par(mfrow=c(2,2))
plot(logit.class)

logit<-predict.glm(logit.class,a_valid)
odds<-exp(logit)
head(odds)

probability<-predict.glm(logit.class,a_valid,type="response")
head(probability)

coefficients(logit.class)[1]
p <-ifelse(logit.class$fitted.values >= 0.5,0,1)
table(p, a_train$default.payment.next.month)

p <-ifelse(logit.class$fitted.values >= 0.9,0,1)
table(p, a_train$default.payment.next.month)

p <-ifelse(logit.class$fitted.values >= 0.1,0,1)

p <-ifelse(logit.class$fitted.values >= 0,0,1)

p <-ifelse(logit.class$fitted.values >= 0.03,0,1)
View(a_valid)

pr<-ifelse(probability <= 0.3,0,1)

install.packages("e1071")
library(e1071)
confusionMatrix(factor(a_valid[,25]),factor(pr))

library(ROCR)
rocChart(pr,a_valid[,25])
