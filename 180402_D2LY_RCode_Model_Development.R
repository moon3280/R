library(party)
library(randomForest)

setwd('C:/Users/37040/Documents/Cases/D2LY - Micro Focus/RCodes')

#Read in datasets
HitRate = read.csv('180402_D2LY_HPE_HitRate.csv')

#Convert Binary Variables to Factors
HitRate$Hit <- as.factor(HitRate$Hit)
HitRate$Binary_LFR_Revenue <- as.factor(HitRate$Binary_LFR_Revenue)
HitRate$Binary_ADM_MFR <- as.factor(HitRate$Binary_ADM_MFR)
HitRate$Binary_ESP_MFR <- as.factor(HitRate$Binary_ESP_MFR)
HitRate$Binary_IMG_MFR <- as.factor(HitRate$Binary_IMG_MFR)
HitRate$Binary_ITOM_MFR <- as.factor(HitRate$Binary_ITOM_MFR)
HitRate$Binary_Platform_MFR <- as.factor(HitRate$Binary_Platform_MFR)

#Random Forest
rforest_output <- randomForest(Hit ~ MFR_Revenue + Binary_LFR_Revenue + Binary_ADM_MFR + Binary_ESP_MFR + Binary_IMG_MFR + Binary_ITOM_MFR
+ Binary_Platform_MFR + Employees + Industry, data = HitRate)

#Variable Importance from Random Forest
print(importance(rforest_output,type = 2))

# Decision Tree based on top ranked variables
fit <- rpart(Hit ~ MFR_Revenue + Binary_LFR_Revenue + Employees + Industry, method ='class', data=HitRate)
rpart.plot(fit)


#Creating New Variables
attach(HitRate)
HitRate$MFR_Revenue_High <- ifelse(HitRate$MFR_Revenue >= 3000, 1, 0)


#logistic regression model
model <- glm (Hit ~ Binary_LFR_Revenue + MFR_Revenue_High, data = HitRate, family = binomial)
summary(model)
