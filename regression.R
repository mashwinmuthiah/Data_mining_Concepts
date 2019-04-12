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

#regression diagnostics
confint(fit3)

#A Typical approch
par(mfrow = c(2,2))
plot(myfit)

plot(fit2)

newfit2<-lm(weight~height+I(height^2),data = women[-c(13,15),])
plot(newfit2)

plot(fit3)

#An enhanced approch
## Normality
library(car)
par(mfrow=c(1,1))
qqPlot(fit3, labels=row.names(states), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")

residplot <- function(fit, nbreaks=10) {
  z <- rstudent(fit)
  hist(z, breaks=nbreaks, freq=FALSE)
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
}
residplot(fit3)

#independence
durbinWatsonTest(fit3)

#linearity
crPlots(fit3)

#homoscendasticity
ncvTest(fit3)
spreadLevelPlot(fit3) 

#global test
library(gvlma)
gvmode<- gvlma(fit3)
summary(gvmode)

#outliers
library(car)
outlierTest(fit3)

#High-leverage points
plot(hatvalues(fit3))
p <- length(coefficients(fit3))
n <- length(fitted(fit3))
identify(1:length(fitted(fit3)),hatvalues(fit3),names(hatvalues(fit3)))
abline(h =c(2,3)*p/n,col = "red",lty = 2)

#Influential observations
## Cook's D test
cutoff<-4/(nrow(states)-length(fit3$coefficients)-2)
plot(fit3,which = 4)
abline(h = cutoff)

#av Plot
avPlots(fit3 , ask = FALSE)
avPlots(fit3, ask=FALSE, id.method="identify")
par(mfrow = (c(1,1)))
influencePlot(fit3,id.method = "identical")


#CORRECTIVE MEASURES
##Transforming
summary(powerTransform(states$Murder))

boxTidwell(Murder~Population+Illiteracy,data=states)

#comparing models
anova(myfit,fit2)
AIC(myfit,fit2)

#Variable selection
##stepwise regression
library(MASS)
fit_vs <- lm(Murder ~ Population + Illiteracy + Income + Frost,
          data=states)
stepAIC(fit_vs,direction = "backward")

#all subset regression
library(leaps)
leaps <-regsubsets(Murder ~ Population + Illiteracy + Income +
                     Frost, data=states, nbest=4)
plot(leaps,scale = "adjr2")
?regsubsets

library(car)
subsets(leaps,statistic = "cp")
abline(1,1)
