library(data.table)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(car)
library(plyr)
library(scales)
library(MLmetrics)

setwd("C:/Users/Alvin Lim/Desktop/NTU Stuff/Analytics I")

dfinsurance <- fread('premium2.csv', stringsAsFactors = T, na.strings = c("NA", "missing", "N/A", "", "m", "M", "na", "."))

View(dfinsurance)
sum(is.na(dfinsurance))   ## 0 NA values 
which(is.na(dfinsurance))
ncol(dfinsurance) ## 12 columns in total
nrow(dfinsurance)

## Convert height from cm to m
dfinsurance$Height <- dfinsurance$Height/100

#### Question 1 ####
## Calculating BMI in a new column
## Reference for BMI Calculation: https://www.cdc.gov/healthyweight/assessing/bmi/adult_bmi/index.html#Interpreted
## BMI = Weight / (Height)^2
## Weight to be in Kg, Height to be in m

dfinsurance$BMI <- dfinsurance$Weight/(dfinsurance$Height^2)


#### Question 2 ####

dfinsurance$Gender <- factor(dfinsurance$Gender)

## Only gender needs to be factorised

#### Question 3 ####
## Data Exploration

cor(dfinsurance$Age,dfinsurance$Premium)
cor(dfinsurance$Height,dfinsurance$Premium)
cor(dfinsurance$Weight,dfinsurance$Premium)
cor(dfinsurance$NumMajorSurgeries,dfinsurance$Premium)
cor(dfinsurance$BMI,dfinsurance$Premium)

summary(dfinsurance)


## Premium Differences between Gender
ggplot(data = dfinsurance, aes(fill=Gender, x=Gender, y=Premium)) + 
  geom_bar(position="dodge", stat="summary", fun = "mean") + 
  scale_fill_manual("Gender",values=c("0"="Dark Blue","1" = "orange"), labels = c("Female","Male"))+
  labs(title = "Average Premium of Both Genders") + 
  ylab("Premium") + scale_x_discrete(labels = c("Female","Male")) + 
  scale_y_continuous(labels = dollar)

## Premium Differences between Presence of Diabetes by Gender
ggplot(data = dfinsurance , mapping = aes(x = Gender , y = Premium, fill = Gender)) + 
  geom_boxplot(alpha = 0.3) + theme(legend.position = "none") + 
  facet_grid(. ~ Diabetes,labeller = as_labeller(c("0" = "Absence of Diabetes", "1" = "Presence of Diabetes"))) + 
  labs(y = "Premium" , title = "Premium Differences between Presence of Diabetes by Gender")+ 
  scale_x_discrete(labels=c("0" = "Female", "1" = "Male")) + scale_y_continuous(labels=dollar)

## Premium Differences between Presence of High Blood Pressure by Gender
ggplot(data = dfinsurance , mapping = aes(x = Gender , y = Premium, fill = Gender)) + 
  geom_boxplot(alpha = 0.3) + theme(legend.position = "none") + 
  facet_grid(. ~ HighBloodPressure,labeller = as_labeller(c("0" = "Absence of High Blood Pressure", "1" = "Presence of High Blood Pressure"))) + 
  labs(y = "Premium" , title = "Premium Differences between Presence of High Blood Pressure by Gender")+ 
  scale_x_discrete(labels=c("0" = "Female", "1" = "Male")) + scale_y_continuous(labels=dollar)

## Premium Differences between Presence of High Blood Pressure by Gender
ggplot(data = dfinsurance , mapping = aes(x = Gender , y = Premium, fill = Gender)) + 
  geom_boxplot(alpha = 0.3) + theme(legend.position = "none") + 
  facet_grid(. ~ Transplant,labeller = as_labeller(c("0" = "Absence of Transplant", "1" = "Presence of Transplant"))) + 
  labs(y = "Premium" , title = "Premium Differences between Presence of Transplant by Gender")+ 
  scale_x_discrete(labels=c("0" = "Female", "1" = "Male")) + scale_y_continuous(labels=dollar)

## Premium Differences between Presence of Chronic Disease by Gender
ggplot(data = dfinsurance , mapping = aes(x = Gender , y = Premium, fill = Gender)) + 
  geom_boxplot(alpha = 0.3) + theme(legend.position = "none") + 
  facet_grid(. ~ ChronicDisease,labeller = as_labeller(c("0" = "Absence of Chronic Disease", "1" = "Presence of Chronic Disease"))) + 
  labs(y = "Premium" , title = "Premium Differences between Presence of Chronic Disease by Gender")+ 
  scale_x_discrete(labels=c("0" = "Female", "1" = "Male")) + scale_y_continuous(labels=dollar)

## Premium Differences between Presence of Allergy by Gender
ggplot(data = dfinsurance , mapping = aes(x = Gender , y = Premium, fill = Gender)) + 
  geom_boxplot(alpha = 0.3) + theme(legend.position = "none") + 
  facet_grid(. ~ Allergy,labeller = as_labeller(c("0" = "Absence of Allergy", "1" = "Presence of Allergy"))) + 
  labs(y = "Premium" , title = "Premium Differences between Presence of Allergy by Gender")+ 
  scale_x_discrete(labels=c("0" = "Female", "1" = "Male")) + scale_y_continuous(labels=dollar)

## Premium Differences between Presence of Cancer In Family by Gender
ggplot(data = dfinsurance , mapping = aes(x = Gender , y = Premium, fill = Gender)) + 
  geom_boxplot(alpha = 0.3) + theme(legend.position = "none") + 
  facet_grid(. ~ CancerInFamily,labeller = as_labeller(c("0" = "Absence of Cancer In Family", "1" = "Presence of Cancer In Family"))) + 
  labs(y = "Premium" , title = "Premium Differences between Presence of Cancer In Family by Gender")+ 
  scale_x_discrete(labels=c("0" = "Female", "1" = "Male")) + scale_y_continuous(labels=dollar)

## Premium Based on BMI
ggplot(data = dfinsurance, aes(x=BMI,y=Premium))+geom_point() +
  facet_grid(.~Gender,labeller = as_labeller(c("0" = "Female", "1" = "Male")))+
  labs(title = "Premium Based on BMI") +ylab("Premium")+xlab("BMI")+ 
  scale_y_continuous(labels = dollar)+ geom_smooth(method = lm)

## Premium Based on Age
ggplot(data = dfinsurance, aes(x=Age,y=Premium))+geom_point() +
  facet_grid(.~Gender,labeller = as_labeller(c("0" = "Female", "1" = "Male")))+
  labs(title = "Premium Based on Age") +ylab("Premium")+xlab("Age")+ 
  scale_y_continuous(labels = dollar)+ geom_smooth(method = lm)

## Premium Based on Height
ggplot(data = dfinsurance, aes(x=Height,y=Premium))+geom_point() +
  facet_grid(.~Gender,labeller = as_labeller(c("0" = "Female", "1" = "Male")))+
  labs(title = "Premium Based on Height") +ylab("Premium")+xlab("Height")+ 
  scale_y_continuous(labels = dollar)+ geom_smooth(method = lm)

## Premium Based on Weight
ggplot(data = dfinsurance, aes(x=Weight,y=Premium))+geom_point() +
  facet_grid(.~Gender,labeller = as_labeller(c("0" = "Female", "1" = "Male")))+
  labs(title = "Premium Based on Weight") +ylab("Premium")+xlab("Weight")+ 
  scale_y_continuous(labels = dollar)+ geom_smooth(method = lm)

## Premium Based on NumMajorSugeries
ggplot(data = dfinsurance, aes(x=NumMajorSurgeries,y=Premium))+geom_point() +
  facet_grid(.~Gender,labeller = as_labeller(c("0" = "Female", "1" = "Male")))+
  labs(title = "Premium Based on Weight") +ylab("Premium")+xlab("Weight")+ 
  scale_y_continuous(labels = dollar)+ geom_smooth(method = lm)

ggplot(data = dfinsurance,aes(x = NumMajorSurgeries, y=Premium))+geom_bar(position="dodge",stat ="summary",fun="mean",fill="Dark Blue")+
  scale_color_manual(values = c("Dark Blue")) +scale_y_continuous(labels = dollar) +
  labs(title = "Number of Major Surgeries vs Premium") + 
  xlab("Number of Major Surgeries")+
  ylab("Premium")

ggplot(data = dfinsurance , mapping = aes(x = Gender , y = Premium, fill = Gender)) + 
  geom_boxplot(alpha = 0.3) + theme(legend.position = "none") + 
  facet_grid(. ~ NumMajorSurgeries,labeller = as_labeller(c("0" = "0","1" = "1","2"="2","3"="3"))) + 
  labs(y = "Premium" , title = "Premium Differences between Number of Major Surgeries by Gender")+ 
  scale_x_discrete(labels=c("0" = "Female", "1" = "Male")) + scale_y_continuous(labels=dollar)

#### Question 4 (CART without Train-Test Split) ####
set.seed(2004)
CART <- rpart(`Premium` ~., data = dfinsurance, method = 'anova')

# plots the maximal tree and results.
rpart.plot(CART, nn= T, main = "Maximal Tree")

# prints the maximal tree m2 onto the console.
print(CART)

# Scaling Variable Impt so as to rep as percentage impts
ScaledVariableImptoriginal <- round(100*CART$variable.importance/sum(CART$variable.importance),2)
ScaledVariableImptoriginal


# Display the pruning sequence and 10-fold CV errors, as a chart.
plotcp(CART, main = "Prune Sequence CV Errors in dfinsurance")

## Automated geometric mean finding
CVerror.cap <- CART$cptable[which.min(CART$cptable[,"xerror"]), "xerror"] + CART$cptable[which.min(CART$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (CART$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cp.opt = ifelse(i > 1, sqrt(CART$cptable[i,1] * CART$cptable[i-1,1]), 1)

# Plug optimal cp to prune model
CARTOptimal <- prune(CART, cp = cp.opt)

printcp(CARTOptimal, digits = 3)
plotcp(CARTOptimal, main = "Post-Pruning Sequence CV Errors in dfinsurance")

## RMSE
sqrt(97526*0.253)

print(CARTOptimal)
rpart.plot(CARTOptimal, nn= T, main = "Pruned Tree")

## Variable Importance

CARTOptimal$variable.importance

# Scaling Variable Impt so as to rep as percentage impt 
ScaledVariableImpt <- round(100*CARTOptimal$variable.importance/sum(CARTOptimal$variable.importance),2)
ScaledVariableImpt
ScaledVariableImpt[ScaledVariableImpt > 3]

summary(CARTOptimal)

#### Question 4 (CART with Train-Test Split) ####
set.seed(2004)
samplecart <- sample.int(n = nrow(dfinsurance), size = floor(.70*nrow(dfinsurance)), replace = F)
trainsetcart <- dfinsurance[samplecart, ]
testsetcart  <- dfinsurance[-samplecart, ]
summary(trainsetcart$Premium)
summary(testsetcart$Premium)

set.seed(2004)
CARTMax <- rpart(Premium ~., data = trainsetcart, method = 'anova')
printcp(CARTMax, digits = 3)

# plots the maximal tree and results.
rpart.plot(CARTMax, nn= T, main = "Maximal Tree Generated")

## Automated geometric mean finding
CVerror.cap <- CARTMax$cptable[which.min(CARTMax$cptable[,"xerror"]), "xerror"] + CARTMax$cptable[which.min(CARTMax$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree
i <- 1; j<- 4
while (CARTMax$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cp.opt = ifelse(i > 1, sqrt(CARTMax$cptable[i,1] * CARTMax$cptable[i-1,1]), 1)

# Plug optimal cp to prune model
CARTOptimal1 <- prune(CARTMax, cp = cp.opt)
printcp(CARTOptimal1, digits = 3)

print(CARTOptimal1)

rpart.plot(CARTOptimal1, nn = T, main = "Pruned Tree") # Plotting optimal tree


## Variable Importance

CARTOptimal1$variable.importance

# Scaling Variable Impt so as to rep as percentage impt 
ScaledVariableImpt <- round(100*CARTOptimal1$variable.importance/sum(CARTOptimal1$variable.importance))
ScaledVariableImpt[ScaledVariableImpt > 3]

summary(CARTOptimal1)


## RMSE
RMSEcart.train <- sqrt(mean(residuals(CARTOptimal1)^2))  # RMSE on train set
RMSEcart.train
summary(abs(residuals(CARTOptimal1)))  # Check Min Abs Error and Max Abs Error.

carttestset.prediction <- predict(CARTOptimal1, newdata = testsetcart)
carttestset.error <- testsetcart$Premium - carttestset.prediction

RMSEcart.test <- sqrt(mean(carttestset.error^2))  # RMSE on test set
RMSEcart.test
summary(abs(carttestset.error))

## Train set and test set accuracy of model
c(RMSEcart.train, RMSEcart.test)

## MAPE
MAPE.CART <- MAPE(testsetcart$Premium,carttestset.prediction)
MAPE.CART

#### Question 4 (Linear without Train-Test Split) ####
lm1 <- lm(Premium ~ ., data = dfinsurance)
summary(lm1)
lm1.stepped <- step(lm1) # Akaike Information Criterion
lm1.stepped
vif(lm1.stepped) # GVIF of all variables < 1.4
summary(lm1.stepped)

# Model equation obtained from backward elimination: `Benefit per order` ~ `Order Item Discount Rate`+ `Order Item Product Price`+`Order Item Profit Ratio` + `Department Id`

#### Question 4 (Linear with Train-Test Split) ####
## Train-test split using seed of 2004 for reproducability and consistency in train test splitting
## Tried using caTools sample.split, same train-test split cannot be reproduced despite setting same seed value
## Hence, using base R sample.int to carry out train-test split
set.seed(2004)
samplelinear <- sample.int(n = nrow(dfinsurance), size = floor(.70*nrow(dfinsurance)), replace = F)
trainsetlinear <- dfinsurance[samplelinear, ]
testsetlinear  <- dfinsurance[-samplelinear, ]
summary(trainsetlinear$Premium)
summary(testsetlinear$Premium)

## Developing final linear regression model using trainset as training data
lmpremium <- lm(Premium ~. ,data = trainsetlinear)
summary(lmpremium) # Adjusted R^2 value = 0.6547. Decrease from 0.6552 is not a significant drop. 
residuals(lmpremium) 
vif(lmpremium) # VIF values < 1.4 except Height, Weight and BMI

## Testing
# Trainset Error
RMSElinear.train <- sqrt(mean(residuals(lmpremium)^2))  # RMSE on trainset based on m5 model.
summary(abs(residuals(lmpremium)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
predict.lm.test <- predict(lmpremium, newdata = testsetlinear)
testsetlinear.error <- testsetlinear$Premium - predict.lm.test

# Testset Errors
RMSElinear.test <- sqrt(mean(testsetlinear.error^2))
summary(abs(testsetlinear.error))

## Train set and test set accuracy of model
c(RMSElinear.train, RMSElinear.test)

## MAPE
MAPE.Linear <- MAPE(testsetlinear$Premium,predict.lm.test)
MAPE.Linear

## Collating RMSE
RMSE <- c(RMSEcart.test, RMSElinear.test)
name <- c("Cart RMSE", "Linear RMSE")

dfrmse = data.frame(name,RMSE)
dfrmse

## Collating MAPE
MAPE <- c(MAPE.CART*100,MAPE.Linear*100)
name1 <- c("Cart MAPE","Linear MAPE")

dfmape = data.frame(name1,MAPE)
dfmape

range(testsetcart$Premium)
nrow(testsetcart)

SI <- RMSEcart.test/mean(testsetcart$Premium)*100
SI
       