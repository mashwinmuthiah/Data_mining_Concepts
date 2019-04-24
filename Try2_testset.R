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
View(a_train)
sum(is.na(a_train))

#change the numerical to factor
a_train$SEX<-as.factor(a_train$SEX)
a_train$EDUCATION<-as.factor(a_train$EDUCATION)
a_train$MARRIAGE<-as.factor(a_train$MARRIAGE)
a_valid$SEX<-as.factor(a_valid$SEX)
a_valid$EDUCATION<-as.factor(a_valid$EDUCATION)
a_valid$MARRIAGE<-as.factor(a_valid$MARRIAGE)

#creating dummy variables for sex,education and marriage
dummy_cat<-fastDummies::dummy_cols(a_train[3:5])
head(dummy_cat[4:13])
dummy_cat_v<-fastDummies::dummy_cols(a_valid[3:5])
head(dummy_cat_v[4:13])

#adding the dummies to the dataframe and removing orginal
#and creating a new df with dummies(wd)
a_train_wd<-cbind(a_train[-3:-5],dummy_cat[4:13])
View(a_train_wd)
a_valid_wd<-cbind(a_valid[-3:-5],dummy_cat_v[4:13])
View(a_valid_wd)

library(C50)
#running a regression tree
set.seed(123)
colnames(a_train_wd)
RT1<-rpart(default.payment.next.month ~ LIMIT_BAL+AGE+PAY_0+PAY_2+PAY_3+
             PAY_4+PAY_5+PAY_6+PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5+
             PAY_AMT6+SEX_2+SEX_1+EDUCATION_2+EDUCATION_1+EDUCATION_3+
             EDUCATION_4+EDUCATION_0+MARRIAGE_1+MARRIAGE_2+MARRIAGE_3,
           data = a_train_wd,
           method="anova",control = rpart.control(maxdepth = 3))
print(RT1)
summary(RT1)
prp(RT1, type = 1, extra = 1, split.font = 1, varlen = -10)
plotcp(RT1)

par(mfrow = c(1,1))
RT1.pred<-predict(RT1,a_train_wd,type="vector")
RT1.valid<-predict(RT1,a_valid_wd,type="vector")
RMSE(RT1.pred,a_train_wd$default.payment.next.month)
RMSE(RT1.valid,a_valid_wd$default.payment.next.month)


library(e1071)
View(a_valid_wd[22])
pr<-ifelse(RT1.valid >= 0.3,1,0)
confusionMatrix(factor(a_valid_wd[,22]),factor(pr))

library(ROCR)
rocChart(pr,a_valid[,25])


par(mfrow=c(1,2))
boxplot(RT1.pred-a_train_wd$default.payment.next.month,main="Boxplot for training data error")
boxplot(RT1.valid-a_valid_wd$default.payment.next.month,main="Boxplot for validation data error")
par(mfrow=c(1,1))


RT1.cv<-rpart(default.payment.next.month ~ LIMIT_BAL+AGE+PAY_0+PAY_2+PAY_3+
                PAY_4+PAY_5+PAY_6+PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5+
                PAY_AMT6+SEX_2+SEX_1+EDUCATION_2+EDUCATION_1+EDUCATION_3+
                EDUCATION_4+EDUCATION_0+MARRIAGE_1+MARRIAGE_2+MARRIAGE_3,
              data=a_train_wd,method="anova",cp=0.00001,minsplit=2,xval=5)
printcp(RT1.cv)

RT1.pruned<-prune(RT1.cv, cp = RT1.cv$cptable[which.min(RT1.cv$cptable[,"xerror"]),"CP"])
prp(RT1.pruned, type = 1, extra = 1, split.font = 1, varlen = -10)
pruned.valid<-predict(RT1.pruned,a_valid_wd,type="vector")
RMSE(pruned.valid,a_valid_wd$default.payment.next.month)

length(pruned.valid)
pr<-ifelse(pruned.valid >= 0.3,1,0)
confusionMatrix(factor(a_valid_wd[,22]),factor(pr))

library(ROCR)
rocChart(pr,a_valid[,25])

#Making price as categorical response
TC<-read.csv("C:\\Users\\ashwi\\Desktop\\Project\\TrainingSet.csv")
TC$binneddefault<-as.factor(as.numeric(cut(TC$default.payment.next.month,5)))
View(TC$binneddefault)
TC$SEX<-as.factor(TC$SEX)
TC$EDUCATION<-as.factor(TC$EDUCATION)
TC$MARRIAGE<-as.factor(TC$MARRIAGE)
set.seed(123)
colnames(TC)
CT<-rpart(binneddefault~LIMIT_BAL+AGE+PAY_0+PAY_2+PAY_3+
            PAY_4+PAY_5+PAY_6+PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5+
            PAY_AMT6+SEX+EDUCATION+
            MARRIAGE,data = TC,
          method="class",control = rpart.control(maxdepth = 3))
print(CT)
prp(CT, type = 1, extra = 1, split.font = 1, varlen = -10)

plot(CT,margin=0.07)
text(CT,cex=0.8)

plot(RT1,margin=0.07)
text(RT1,cex=0.8)

#Running model on validation data
TC_valid<-read.csv("C:\\Users\\ashwi\\Desktop\\Project\\ValidationSet.csv")
TC_valid$SEX<-as.factor(TC_valid$SEX)
TC_valid$EDUCATION<-as.factor(TC_valid$EDUCATION)
TC_valid$MARRIAGE<-as.factor(TC_valid$MARRIAGE)

predict(CT,TC_valid)
predict(RT1,a_valid_wd)
