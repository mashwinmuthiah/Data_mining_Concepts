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
a_train<-read.csv("C:\\Users\\ashwi\\Desktop\\Project\\TrainingSet.csv")
a_valid<-read.csv("C:\\Users\\ashwi\\Desktop\\Project\\ValidationSet.csv")
sum(is.na(a_train))

n_train = as.data.frame(scale(a_train[,-25]))
n_valid = as.data.frame(scale(a_valid[,-25]))

#change the numerical to factor
n_train$SEX<-as.factor(n_train$SEX)
n_train$EDUCATION<-as.factor(n_train$EDUCATION)
n_train$MARRIAGE<-as.factor(n_train$MARRIAGE)
n_valid$SEX<-as.factor(n_valid$SEX)

n_valid$EDUCATION<-as.factor(n_valid$EDUCATION)
n_valid$MARRIAGE<-as.factor(n_valid$MARRIAGE)

#creating dummy variables for sex,education and marriage
dummy_cat<-fastDummies::dummy_cols(n_train[3:5])
head(dummy_cat[4:13])
dummy_cat_v<-fastDummies::dummy_cols(n_valid[3:5])
head(dummy_cat_v[4:13])

#adding the dummies to the dataframe and removing orginal
#and creating a new df with dummies(wd)
n_train_wd<-cbind(n_train[-3:-5],dummy_cat[4:13])

n_valid_wd<-cbind(n_valid[-3:-5],dummy_cat_v[4:13])



defualttrain <- a_train$default.payment.next.month
defualtvalid<- a_valid$default.payment.next.month

library(kknn)
library(class)

knn.1 = knn(n_train_wd,n_valid_wd,cl = defualttrain,k= 1,prob = TRUE)
table(knn.1,defualtvalid)
confusionMatrix(factor(knn.1),factor(defualtvalid))

knn.2 = knn(n_train_wd,n_valid_wd,cl = defualttrain,k= 2,prob = TRUE)
confusionMatrix(factor(knn.2),factor(defualtvalid))


knn.3 = knn(n_train_wd,n_valid_wd,cl = defualttrain,k= 3,prob = TRUE)
confusionMatrix(factor(knn.3),factor(defualtvalid))


knn.4 = knn(n_train_wd,n_valid_wd,cl = defualttrain,k= 4,prob = TRUE)
confusionMatrix(factor(knn.4),factor(defualtvalid))


knn.5 = knn(n_train_wd,n_valid_wd,cl = defualttrain,k= 5,prob = TRUE)
confusionMatrix(factor(knn.5),factor(defualtvalid))

