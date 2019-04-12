women

#simple liner regression
myfit<-lm(weight~height,data = women)
summary(myfit)

women$weight

fitted(myfit)

residuals(myfit)

plot(women$height,women$weight)
abline(myfit)

coefficients(myfit)

#polynpmial regression

fit2<-lm(weight ~ height + I(height^2),data = women)
summary(fit2)

plot(women$height,women$weight)
lines(women$height,fitted(fit2))

coefficients(fit2)

library(car)
scatterplot(weight ~ height,data = women)

#Multiple linear regression
states<-as.data.frame(state.x77[,c("Murder", "Population","Illiteracy", "Income", "Frost")])

scatterplotMatrix(states,col = "red")

fit3<-lm(Murder ~ Population+Illiteracy+Income+Frost,data = states)
summary(fit3)
coefficients(fit3)

#multiple linear regression with interactions

View(mtcars)
fit4<-lm(mpg ~ hp+wt+hp:wt , data = mtcars)
summary(fit4)

install.packages("effects")
library(effects)
plot(effect("hp:wt",fit4,xlevels = list(wt=c(2.2,3.2,4.2))),multiline = TRUE)
