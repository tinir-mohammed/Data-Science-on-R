library(dplyr)
library(class)
library(gmodels)
library(corrplot)
library(naniar)

fullSet <- read.csv("default of credit card clients.csv",skip = 1, header = TRUE)
cleanForKNN <- select(fullSet, 
                       -c(ID, EDUCATION, SEX, MARRIAGE, AGE,
                          BILL_AMT1, BILL_AMT2, BILL_AMT3, BILL_AMT4, BILL_AMT5, BILL_AMT6)
)

View(cleanForKNN)

#no missing values
vis_miss(fullSet)

#scatterplot
attach(cleanForKNN)
plot(cleanForKNN, mpg, main="Scatterplot Example",
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)

# create correlation plot
correlations <- cor(fullSet[,13:25])
corrplot(correlations, method="circle")

min_max_scale <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# get 80% of data
randData <- sample(1:nrow(cleanForKNN), 0.8 * nrow(cleanForKNN))

# apply normalization to all except default status
defaultNorm <- as.data.frame(lapply(cleanForKNN[, c(1:13)], min_max_scale))
summary(defaultNorm)

# split dataset for train and test data
creditTrain <- defaultNorm[randData,]
creditTest <- defaultNorm[-randData,]

# get labels according to dataset
creditTrainLabel <- cleanForKNN[randData, 14]
creditTestLabel <- cleanForKNN[-randData, 14]

# knn prediction
predictDefault <- knn(creditTrain, creditTest, cl = creditTrainLabel, k = 5)

# confusion matrix
cf_knnDefault <- table(predictDefault, creditTestLabel)
cf_knnDefault

TP <- cf_knnDefault[1,1]
TP

FN <- cf_knnDefault[1,2]
FN

FP <- cf_knnDefault[2,1]
FP

TN <- cf_knnDefault[2,2]
TN

accuracyCF <- (TP + TN)/(TP + FN + FP + TN)
accuracyCF
## 0.7936667

# calculating sensitivity
sensitivity <- TP/(TP + FN)
sensitivity
## 0.8385571

# calculating specifity
specificity <- TN/(TN + FP)
specificity
## 0.5480043

# precision
precision <- TP/(TP + FP)
precision
# 0.910336

# recall
recall <- sensitivity
recall
## 0.8385571

# F-measure
f_measure <- (2 * TP)/(2 * TP + FP + FN)
f_measure
## 0.8729735

# error rate
error_rate <- 1 - accuracyCF
error_rate
## 0.2063333

#table summary
test_summary <- c(accuracyCF,sensitivity,specificity,precision,recall,f_measure,error_rate)
test_summary <- as.data.frame(test_summary)
rownames(test_summary) <- c("Accuracy","Sensitivity","Specificity",
                            "Precision","Recall","F-Measure","Error rate")
test_summary

CrossTable(x = creditTestLabel, y = predictDefault, prop.chisq=FALSE)
summary()

head(cleanForKNN$default.payment.next.month)

# create correlation plot
correlations <- cor(cleanForKNN[,1:14])
corrplot(correlations, method="circle")
plot(cleanForKNN$default.payment.next.month)
hist(cleanForKNN$default.payment.next.month, 
     main = "Number of defaulters and non defaulters",
     xlab = "Defaulters", xlim = c(0,1), breaks = 2)
