#installing and loading packages needed.
library(caret)
library(lattice)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(rattle)
library(tree)
library(gmodels)
library(RColorBrewer)

#loading CSV file
TC<-read.csv("ToyotaCorolla_csv_data.csv")
View(TC)
str(TC)

#Removing 'ID' column
TC<-TC[,-1]

#Creating Dummy variables for 'Fuel_Type' and 'Color'
dummies<-fastDummies::dummy_cols(TC$Fuel_Type)
str(dummies[3])
TC$CNG<-dummies[4]
TC$Petrol<-dummies[3]
TC$Diesel<-dummies[2]

#Removing original 'Fuel_type' column
TC<-TC[,-7]
TC<-TC[1:1436,]

#Creating dummy variables for 'Color'
dummies_col<-fastDummies::dummy_cols(TC$Color)
head(dummies_col)
TC$Beige<-dummies_col[11]
TC$Black<-dummies_col[4]
TC$Blue<-dummies_col[2]
TC$Green<-dummies_col[8]
TC$Grey<-dummies_col[6]
TC$Red<-dummies_col[7]
TC$Silver<-dummies_col[3]
TC$Violet<-dummies_col[10]
TC$White<-dummies_col[5]
TC$Yellow<-dummies_col[9]

#Removing original predictor 'Color'
TC<-TC[,-9]                     

#Partitioning into training, validation and test data
set.seed(111)
train.index<-sample(row.names(TC),0.5*dim(TC)[1])
traindf<-TC[train.index,]
rem.index<-setdiff(row.names(TC),train.index)
remdf<-TC[rem.index,]
valid.index<-sample(row.names(remdf),0.3*dim(TC)[1])
validdf<-TC[valid.index,]
test.index<-setdiff(row.names(remdf),valid.index)
testdf<-TC[test.index,]
traindf<-as.data.frame(traindf)

#Running a regression tree:
#(a)	Age_08_04, HP & KM are the three important car specifications
RT1<-rpart(Price~Age_08_04+KM+unlist(CNG)+unlist(Petrol)+HP+Automatic+Doors+
             Quarterly_Tax+Mfr_Guarantee+Guarantee_Period+Airco+Automatic_airco+
             CD_Player+Powered_Windows+Sport_Model+Tow_Bar,data=traindf,
           method="anova",control = rpart.control(maxdepth = 3))
printcp(RT1)
summary(RT1)

#Displaying the Regression tree:
prp(RT1, type = 1, extra = 1, split.font = 1, varlen = -10)
plotcp(RT1)

#Running training, validation and test datasets and calculating the error for each
RT1.pred<-predict(RT1,traindf,type="vector")
RT1.valid<-predict(RT1,validdf,type="vector")
RMSE(RT1.pred,traindf$Price)
RMSE(RT1.valid,validdf$Price)
RT1.test<-predict(RT1,testdf,type="vector")
RMSE(RT1.test,testdf$Price)

#Plotting boxplot for errors:
par(mfrow=c(1,3))
boxplot(RT1.pred-traindf$Price,main="Boxplot for training data error")
boxplot(RT1.valid-validdf$Price,main="Boxplot for validation data error")
boxplot(RT1.test-testdf$Price,main="Boxplot for test data error")
par(mfrow=c(1,1))

#Cross-validation Procedure
RT1.cv<-rpart(Price~Age_08_04+KM+unlist(CNG)+unlist(Diesel)+unlist(Petrol)+HP+Automatic+Doors+
                Quarterly_Tax+Mfr_Guarantee+Guarantee_Period+Airco+Automatic_airco+
                CD_Player+Powered_Windows+Sport_Model+Tow_Bar,data=traindf,method="anova",cp=0.00001,minsplit=2,xval=5)
printcp(RT1.cv)

#Pruning the tree
RT1.pruned <- prune(RT1.cv, cp = RT1.cv$cptable[which.min(RT1.cv$cptable[,"xerror"]),"CP"])
prp(RT1.pruned, type = 1, extra = 1, split.font = 1, varlen = -10)
summary(RT1.pruned)

#Running validation dataset through the pruned tree
pruned.valid<-predict(RT1.pruned,validdf,type="vector")
RMSE(pruned.valid,validdf$Price)

#Making price as categorical response
TC$Binnedprice<-as.factor(as.numeric(cut(TC$Price,20)))
View(TC$Binnedprice)
TC_new<-TC[,-2]
set.seed(111)
train.index2<-sample(row.names(TC_new),0.5*dim(TC_new)[1])
traindf2<-TC_new[train.index2,]
rem.index2<-setdiff(row.names(TC_new),train.index2)
remdf2<-TC_new[rem.index2,]
valid.index2<-sample(row.names(remdf2),0.3*dim(TC_new)[1])
validdf2<-TC_new[valid.index2,]
test.index2<-setdiff(row.names(remdf2),valid.index2)
testdf2<-TC_new[test.index2,]
CT<-rpart(Binnedprice~Age_08_04+KM+unlist(CNG)+unlist(Diesel)+unlist(Petrol)+HP+Automatic+
            Doors+Quarterly_Tax+Mfr_Guarantee+Guarantee_Period+Airco+Automatic_airco
          +CD_Player+Powered_Windows+Sport_Model+Tow_Bar,data=traindf2,method="class",control = rpart.control(maxdepth = 3))
printcp(CT)
prp(CT, type = 1, extra = 1, split.font = 1, varlen = -10)
summary(CT)

plot(CT,margin=0.07)
text(CT,cex=0.8)

plot(RT1,margin=0.07)
text(RT1,cex=0.8)

#Running model on new test data 
new_test<-data.frame(Age_08_04=77,KM=117000,CNG=0,Diesel=0,Petrol=1,
                     HP=110,Automatic=0,Doors=5,Quarterly_Tax=100,
                     Mfr_Guarantee=0,Guarantee_Period=3,Airco=1,Automatic_airco=0,
                     CD_Player=0,Powered_Windows=0,Sport_Model=0,Tow_Bar=1)
predict(RT1,new_test)
predict(CT,new_test)

max(TC[which(TC_new$Binnedprice == 3),2])
min(TC[which(TC_new$Binnedprice == 3),2])
mean(TC[which(TC_new$Binnedprice == 3),2])

#From Regression tree, the value predicted is 7980.74 & from classification tree we take the mean value which is 7934.188.
#Magnitude of difference = 7980.74-7934.18=46.56
#Advantages
#Regression will implicitly perform variable screening or feature selection
#Easy to interpret
#Disadvantages
#High complexity
#Time consuming
