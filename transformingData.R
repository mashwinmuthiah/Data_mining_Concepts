library(caTools)
library(tidyverse)

# Raw data
rawdata <- read.csv("UCI_Credit_Card.csv")

# Removing ID
newdata <- rawdata[,-1]

# Removing Education level 5 and 6
newdata <- newdata %>% filter(EDUCATION <= 4)

# Removing Marriage level 0
newdata <- newdata %>% filter(MARRIAGE != 0)

# Binning PAY_0-PAY_6 into 3 categories:
## 0: Payed Duly  1: Payed within 2 months  2: Payed after 2 months
for (i in seq(6,11)) {
  newdata[,i][newdata[,i] <= 0] <- 0
  newdata[,i][newdata[,i] ==2] <- 1
  newdata[,i][newdata[,i] >=3] <- 2
}

# Changing -ve bill_amount to 0
for (i in seq(12,17)) {
  newdata[,i][newdata[,i] < 0] <- 0
}

logdata <- newdata

# Log of LIMIT_BAL, BILL_AMT1-6, PAY_AMT1-6 
logdata$LIMIT_BAL <- log(newdata$LIMIT_BAL)

for (i in seq(12,23)) {
  logdata[,i] <- log(newdata[,i]+1)
}

#Splitting Data
set.seed(123)
fractionTraining   <- 2/3
fractionValidation <- 6/30
fractionTest       <- 4/30

sampleSizeTraining   <- floor(fractionTraining   * nrow(newdata))
sampleSizeValidation <- floor(fractionValidation * nrow(newdata))
sampleSizeTest       <- floor(fractionTest       * nrow(newdata))

indicesTraining    <- sort(sample(seq_len(nrow(newdata)), size=sampleSizeTraining))
indicesNotTraining <- setdiff(seq_len(nrow(newdata)), indicesTraining)
indicesValidation  <- sort(sample(indicesNotTraining, size=sampleSizeValidation))
indicesTest        <- setdiff(indicesNotTraining, indicesValidation)

# Without log data
trainingdata   <- newdata[indicesTraining, ]
validationdata <- newdata[indicesValidation, ]
testdata       <- newdata[indicesTest, ]

# Log data
logtrainingdata   <- logdata[indicesTraining, ]
logvalidationdata <- logdata[indicesValidation, ]
logtestdata       <- logdata[indicesTest, ]

# Exporting to csv
write.csv(trainingdata, "TrainingSet.csv")
write.csv(validationdata, "ValidationSet.csv")
write.csv(testdata, "TestSet.csv")

write.csv(logtrainingdata, "LogTrainingSet.csv")
write.csv(logvalidationdata, "LogValidationSet.csv")
write.csv(logtestdata, "LogTestSet.csv")
