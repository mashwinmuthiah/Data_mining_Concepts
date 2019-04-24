data<-read.csv("C:\\Users\\ashwi\\Desktop\\Project\\UCI_Credit_Card.csv")
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(corrplot)

dim(data)
str(data)
summary(data)

View(data)


data$SEX<-as.factor(data$SEX)
data$EDUCATION<-as.factor(data$EDUCATION)
data$MARRIAGE<-as.factor(data$MARRIAGE)


ggplot(data=data)+            
  geom_boxplot(
    aes(                      
      x=SEX,
      y=AGE
    )
  ) 

ggplot(data=data,
       aes(x=AGE)) +
  geom_bar(fill = "green", col = "blue")


ggplot(data = data, aes(x = AGE, fill=SEX)) +
  geom_histogram( bins = 30, alpha = 0.5)

ggplot(data = data, aes(x = AGE , fill= EDUCATION )) +
  geom_histogram( bins = 30, alpha = 0.5 )

qplot(EDUCATION, LIMIT_BAL, data=data, geom=c("boxplot"), 
      fill=EDUCATION, main="LIMIT_AMT by Education",
      xlab="EDUCATION", ylab="CREDIT amount")

qplot(AGE, SEX, data=data, geom=c("jitter"), 
      fill=AGE, main="AGE/SEX ratio",
      xlab="AGE", ylab="SEX")

qplot(SEX, LIMIT_BAL, data=data, geom=c("boxplot"), 
      fill=SEX, main="LIMIT_AMT by SEX",
      xlab="SEX", ylab="CREDIT amount")



ggplot(data=data, aes(x=SEX,fill=SEX)) + geom_bar() +
  labs(title = "Distribution by Gender", x ="Gender",fill = "Gender") +
  scale_fill_manual(values=c("#56B4E9", "#FF9999")) +
  theme(axis.text.x = element_text(angle = 45,hjust=1))

ggplot(data=data, aes(x=EDUCATION,fill=EDUCATION)) + geom_bar() +
  labs(title = "Distribution by Education Level", x ="Education Level",fill = "Education Level") +
  theme(axis.text.x = element_text(angle = 45,hjust=1))

ggplot(data=data, aes(x=MARRIAGE,fill=MARRIAGE)) + geom_bar() +
  labs(title = "Distribution by Marital Status", x ="Marital Status",fill = "Marital Status") +
  theme(axis.text.x = element_text(angle = 45,hjust=1))



ggplot(data=data, aes(x=EDUCATION,fill=SEX)) + geom_bar(position='dodge') +
  labs(title = "Distribution by Education & Gender", x ="EDUCATION",fill = "GENDER") +
  scale_fill_manual(values=c("#56B4E9", "#FF9999")) +
  theme(axis.text.x = element_text(angle = 45,hjust=1))

ggplot(data=data, aes(x=MARRIAGE,fill=SEX)) + geom_bar(position='dodge') +
  labs(title = "Distribution by Marital Status & Gender", x ="Marital Status",fill = "Gender") +
  scale_fill_manual(values=c("#56B4E9", "#FF9999")) +
  theme(axis.text.x = element_text(angle = 45,hjust=1))

ggplot(data=data, aes(x=EDUCATION,fill=MARRIAGE)) + geom_bar(position='dodge') +
  labs(title = "Distribution by Education & Marital Status", x ="Education",fill = "Marital Status") +
  theme(axis.text.x = element_text(angle = 45,hjust=1))


ggplot(data = data, aes(x = AGE)) + 
  geom_histogram(bins = 50, fill = "purple", col = "blue", alpha = 0.3) + 
  scale_x_continuous(breaks = seq(min(0), max(90), by = 5), na.value = TRUE)


ggplot(data, aes(x=EDUCATION, fill = EDUCATION)) + 
  geom_bar() +
  labs(title="Density Plot", 
       subtitle="Clients Grouped by Education",
       caption="Source: UCI Credit Card",
       x="Education",
       fill="Education")

ggplot(data, aes(x=EDUCATION, fill = EDUCATION)) + 
  geom_density(aes(fill=factor(EDUCATION)), alpha=0.8) + 
  labs(title="Density Plot", 
       subtitle="Clients Grouped by Education",
       caption="Source: UCI Credit Card",
       x="Education",
       fill="Education")


#Payment data 
#let us now see for pay_2
ggplot(
  data = data,
  aes(
    x = MARRIAGE,
    y = default.payment.next.month,
    fill = MARRIAGE,
    color = MARRIAGE
  )
) + geom_jitter(alpha = 0.5) + theme(legend.position = "bottom")

ggplot(data = data,
       aes(
         x = MARRIAGE,
         y = LIMIT_BAL,
         fill = MARRIAGE,
         color = MARRIAGE
       )) + geom_boxplot(alpha = 0.5, outlier.colour = "orange") + scale_y_continuous(breaks = seq(min(0), max(2000000), by = 50000), na.value = TRUE) + 
  theme(legend.position = "bottom")

ggplot(data = data, aes(
  x = SEX,
  y = AGE,
  fill = SEX,
  color = SEX
)) + geom_boxplot(alpha = 0.5, outlier.colour = "orange") + scale_y_continuous(breaks = seq(min(0), max(100), by = 5), na.value = TRUE) + 
  theme(legend.position = "bottom")

ggplot(data = data, aes(x = PAY_5, fill = PAY_5)) + 
  geom_bar() + 
  scale_y_continuous(breaks = seq(min(0), max(20000), by = 500), na.value = TRUE)

