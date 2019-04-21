#loading dataset
df<-read.csv("boston.csv")
head(df)

#seperating training and test dataset
set.seed(111)
sample<-sample.int(n=nrow(df),size = floor(0.75*nrow(df)),replace = FALSE)
train<-df[sample,]
test<-df[-sample,]
x_train<-train[,1:13]
x_test<-test[,1:13]
y_train<-train[,14]
y_test<-test[,14]

#fitting linear model
my_fit<-lm("medv ~ .",data = train)

#details of the model
summary(my_fit)
coefficients(my_fit)
confint(my_fit)
vcor(my_fit)
par(mfrow = c(2,2))
plot(my_fit)
anova(my_fit)

