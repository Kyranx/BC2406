#### Importing Libraries and Dataset ####

## Installing required packages
install.packages("data.table")
install.packages("corrplot")
install.packages("stringr")
install.packages("tidyr")
install.packages("splitstackshape")
install.packages("countrycode")
install.packages("sf")
install.packages("units")
install.packages("maps")
install.packages("rnaturalearth")
install.packages("tmap")
install.packages("ggmap")
install.packages("geosphere")
install.packages("reprex")
install.packages("ggfortify") 
install.packages("GGally") 
install.packages("regclass")
install.packages("prophet")


## Importing required libraries
library(data.table)
library(corrplot)
library(rpart)
library(rpart.plot)
library(stringr)
library(tidyr)
library(splitstackshape)
library(reshape2)
library(lubridate)
library(nnet)
library(countrycode)
library(translateR)
library(dplyr)
library(maps)
library(sf) 
library(tidyverse)
library(units)
library(tmap)
library(ggmap)
library(RJSONIO)
library(geosphere)
library(caTools)
library(ggplot2)
library(stats)
library(ggfortify)
library(car)
library(prophet)
library(forecast)
library(scales)

## Importing dataset
setwd("C:/Users/Alvin Lim/Desktop/NTU Stuff/Analytics I")
dfsupplychain <- fread('DataCoSupplyChainDataset.csv', stringsAsFactors = T, na.strings = c("NA", "missing", "N/A", "", "m", "M", "na", "."))



#### Data Cleaning ####

set.seed(2004)
View(dfsupplychain)
sum(is.na(dfsupplychain))   ## 336209 NA values 
which(is.na(dfsupplychain))
ncol(dfsupplychain) ## 53 columns in total

## Dropping columns with NA values and other columns containing identifiers which does not impact analysis
dfsupplychain[, c("Customer City",
        "Customer Email",
        "Customer Fname", 
        "Customer Lname",
        "Customer Password",
        "Customer Id",
        "Customer Zipcode", 
        "Customer Street", 
        "Order Customer Id",
        "Order Id", 
        "Order Item Cardprod Id",
        "Order Item Id",
        "Order Zipcode", 
        "Product Card Id", 
        "Product Category Id", 
        "Product Description", 
        "Product Image",
        "Product Status"):=NULL] 

View(dfsupplychain)
sum(is.na(dfsupplychain))    ## 0 NA Values after dropping
which(is.na(dfsupplychain))
ncol(dfsupplychain) ## 13 columns after dropping
colnames(dfsupplychain)[20] <- "datetime" ## renaming order date column 
colnames(dfsupplychain)[34] <- "shippingdatetime"

## Splitting order date and time
dfsupplychain$orderdate <- sapply(strsplit(as.character(dfsupplychain$datetime),' '), "[", 1)
dfsupplychain$ordertime <- sapply(strsplit(as.character(dfsupplychain$datetime),' '), "[", 2)
dfsupplychain$shippingdate <- sapply(strsplit(as.character(dfsupplychain$shippingdatetime),' '), "[", 1)
dfsupplychain$shippingtime <- sapply(strsplit(as.character(dfsupplychain$shippingdatetime),' '), "[", 2)
dfsupplychain$orderdate <- parse_date_time(dfsupplychain$orderdate, orders = c("ymd", "dmy", "mdy")) ## convert orderdate column to date
class(dfsupplychain$orderdate) ## Check if the format is correct
dfsupplychain$shippingdate <- parse_date_time(dfsupplychain$shippingdate, orders = c("ymd", "dmy", "mdy")) ## convert shippingdate column to date
class(dfsupplychain$shippingdate) ## Check if the format is correct

dfsupplychain$orderquarter <- quarter(dfsupplychain$orderdate)
dfsupplychain$shippingquarter <- quarter(dfsupplychain$shippingdate) ## Extracting quarter information

## Translate Order Country to English
custom_dict <- data.frame(spanish = countrycode::codelist$cldr.name.es,
                          english = countrycode::codelist$cldr.name.en,
                          stringsAsFactors = FALSE)
dfsupplychain$ordercountry <- countrycode(dfsupplychain$`Order Country`, "spanish", "english", custom_dict = custom_dict)

# Some values were not matched unambiguously: Canada, Costa de Marfil, Guinea-Bissau, Hong Kong, Macedonia, Qatar, República Checa, República de Gambia, República del Congo, Rumania, Suazilandia, SudAfrica

dfsupplychain$ordercountry[is.na(dfsupplychain$ordercountry)]<-"" #removes the NAs 
dfsupplychain$ordercountry <- ifelse(dfsupplychain$ordercountry == "", paste(dfsupplychain$`Order Country`,dfsupplychain$ordercountry, sep = ""),dfsupplychain$ordercountry)


dfsupplychain$ordercountry[dfsupplychain$ordercountry == "Costa de Marfil"] <- "Ivory Coast"
dfsupplychain$ordercountry[dfsupplychain$ordercountry == "República Checa"] <- "Czech Republic"
dfsupplychain$ordercountry[dfsupplychain$ordercountry == "República de Gambia"] <- "Republic of Gambia"
dfsupplychain$ordercountry[dfsupplychain$ordercountry == "República del Congo"] <- "Republic of Congo"
dfsupplychain$ordercountry[dfsupplychain$ordercountry == "Rumania"] <- "Romania"
dfsupplychain$ordercountry[dfsupplychain$ordercountry == "Suazilandia"] <- "Swaziland"
dfsupplychain$ordercountry[dfsupplychain$ordercountry == "SudAfrica"] <- "South Africa"
dfsupplychain$customercountry <- as.character(dfsupplychain$`Customer Country`)
dfsupplychain$customercountry[dfsupplychain$customercountry == "EE. UU."] <- "United States"

## Check both columns
distinct(dfsupplychain,ordercountry)
distinct(dfsupplychain,customercountry)

## Processing geospatial information
capitals1 <- filter(world.cities, capital == 1)
dfsupplychain$ordercountry1 <- capitals1$country.etc[match(dfsupplychain$ordercountry, capitals1$country.etc)]
dfsupplychain$ordercountry1[is.na(dfsupplychain$ordercountry1)] <- ""
unique(dfsupplychain$ordercountry[dfsupplychain$ordercountry1==""])
##  [1] "South Korea"          "United States"        "Congo - Kinshasa"     "United Kingdom"       "Myanmar (Burma)"      "Trinidad & Tobago"   
## [7] "Macedonia"            "Hong Kong"            "Republic of Congo"    "Bosnia & Herzegovina" "South Sudan"          "Republic of Gambia"  
dfsupplychain$ordercountry1 <- ifelse(dfsupplychain$ordercountry1 == "", dfsupplychain$ordercountry,dfsupplychain$ordercountry1)


## no hong kong in world.cities dfsupplychain$test[dfsupplychain$test == "Hong Kong"] <- "Korea South"
dfsupplychain$ordercountry1[dfsupplychain$ordercountry1 == "South Korea"] <- "Korea South"
dfsupplychain$ordercountry1[dfsupplychain$ordercountry1 == "United States"] <- "USA"
dfsupplychain$ordercountry1[dfsupplychain$ordercountry1== "Congo - Kinshasa"] <- "Congo Democratic Republic"
dfsupplychain$ordercountry1[dfsupplychain$ordercountry1 == "United Kingdom"] <- "UK"
dfsupplychain$ordercountry1[dfsupplychain$ordercountry1 == "Myanmar (Burma)"] <- "Myanmar"
dfsupplychain$ordercountry1[dfsupplychain$ordercountry1 == "Trinidad & Tobago"] <- "Trinidad and Tobago"
dfsupplychain$ordercountry1[dfsupplychain$ordercountry1 == "Macedonia"] <- "North Macedonia"
dfsupplychain$ordercountry1[dfsupplychain$ordercountry1 == "Republic of Congo"] <- "Congo"
dfsupplychain$ordercountry1[dfsupplychain$ordercountry1 == "Bosnia & Herzegovina"] <- "Bosnia and Herzegovina"
dfsupplychain$ordercountry1[dfsupplychain$ordercountry1 == "South Sudan"] <- "Sudan"
dfsupplychain$ordercountry1[dfsupplychain$ordercountry1 == "Republic of Gambia"] <- "Gambia"

dfsupplychain$orderlat <- capitals1$lat[match(dfsupplychain$ordercountry1,capitals1$country.etc)]
dfsupplychain$orderlong <- capitals1$long[match(dfsupplychain$ordercountry1,capitals1$country.etc)]
dfsupplychain$orderlat <- ifelse(dfsupplychain$ordercountry1 == "Hong Kong", "22.3193", dfsupplychain$orderlat)
dfsupplychain$orderlong <- ifelse(dfsupplychain$ordercountry1 == "Hong Kong", "114.1694", dfsupplychain$orderlong)
dfsupplychain$orderlat <- ifelse(dfsupplychain$ordercountry1 == "Montenegro", "42.7087", dfsupplychain$orderlat)
dfsupplychain$orderlong <- ifelse(dfsupplychain$ordercountry1 == "Montenegro", "19.3744", dfsupplychain$orderlong)
dfsupplychain$orderlat <- ifelse(dfsupplychain$ordercountry1 == "North Macedonia", "41.6086", dfsupplychain$orderlat)
dfsupplychain$orderlong <- ifelse(dfsupplychain$ordercountry1 == "North Macedonia", "21.7453", dfsupplychain$orderlong)
dfsupplychain$orderlat <- ifelse(dfsupplychain$ordercountry1 == "Serbia", "44.0165", dfsupplychain$orderlat)
dfsupplychain$orderlong <- ifelse(dfsupplychain$ordercountry1 == "Serbia", "21.0059", dfsupplychain$orderlong)
sum(is.na(dfsupplychain))

## Distance measurement (For logistic regression, and CART)
l <- set_units(1:10, m) # Length
set_units(l, cm)
set_units(l, cm) + l
a <- set_units(355, ha)
set_units(a, km2)
vel <- set_units(seq(20, 50, 10), km/h)
set_units(vel, m/s)
test <- st_as_sf(dfsupplychain, coords = c("orderlong","orderlat"),crs = 4326) ## Assigning geometry point to new variable
test1 <- st_as_sf(dfsupplychain, coords = c("Longitude","Latitude"),crs = 4326) ## Assigning geometry point to new variable
test <- test[-c(1:22)]
test1 <- test1[-c(1:22)]
dfsupplychain$Distance <- st_distance(test1, test, by_element = TRUE)
dfsupplychain$Distance <- set_units(dfsupplychain$Distance, "km")
View(dfsupplychain)

## Cleaning up columns after finding distance
dfsupplychain$orderlong <- NULL
dfsupplychain$orderlat <- NULL
dfsupplychain$Latitude <- NULL
dfsupplychain$Longitude <- NULL
dfsupplychain$ordercountry1 <- NULL
dfsupplychain$`Order Country` <- NULL
dfsupplychain$`Customer Country` <- NULL
dfsupplychain$shippingtime <- NULL

## Cleaning up date and time columns after splitting into date, time and quarter
dfsupplychain$ordertime <- NULL
dfsupplychain$datetime <- NULL
dfsupplychain$shippingdatetime <- NULL
dfsupplychain$shippingdate <- NULL

## Factorising categorical variables
dfsupplychain$`Delivery Status` <- factor(dfsupplychain$`Delivery Status`)
dfsupplychain$Late_delivery_risk <- factor(dfsupplychain$Late_delivery_risk)
dfsupplychain$`Department Name` <- factor(dfsupplychain$`Department Name`)
dfsupplychain$`Department Id` <- factor(dfsupplychain$`Department Id`)
dfsupplychain$`Customer Segment` <- factor(dfsupplychain$`Customer Segment`)
dfsupplychain$`Order Status` <- factor(dfsupplychain$`Order Status`)
dfsupplychain$`Shipping Mode` <- factor(dfsupplychain$`Shipping Mode`)
dfsupplychain$orderquarter <- factor(dfsupplychain$orderquarter)
dfsupplychain$shippingquarter <- factor(dfsupplychain$shippingquarter)
dfsupplychain$`Category Id` <- factor(dfsupplychain$`Category Id`)
dfsupplychain$`ordercountry` <- factor(dfsupplychain$`ordercountry`)
dfsupplychain$`customercountry` <- factor(dfsupplychain$`customercountry`)

## Saving cleaned dataset as csv file

dfsupplychain.cleaned <- copy(dfsupplychain)
write.csv(dfsupplychain.cleaned, "supplychain_cleaned.csv")



#### Data Exploration/Visualisation ####

#### >>Graphs for Data Exploration/Visualisation ####

dfdataexplore <- copy(dfsupplychain.cleaned)
dfdataexplore$orderyear <- year(dfdataexplore$orderdate) #Extracting year information from date
View(dfdataexplore)

## Delivery Status grouped by Department Name
ggplot(data = dfdataexplore)+geom_bar(position = "dodge", 
                                  mapping = aes(x=`Department Name`,fill=`Delivery Status`))+scale_fill_manual("Delivery Status",values=c("Advance shipping" = "Dark Blue","Late delivery" = "Orange","Shipping on time" = "Green", "Shipping canceled" = "Red"),
                                                                                                               labels = c("Advanced Delivery","Late Delivery","Shipping canceled", "Shipping on time"))+labs(title = "Delivery Statuses Per Department")+xlab("Department Name")+ylab("No. of Orders")+scale_x_discrete(labels = c("Apparel", "Book Shop", "Discs Shop", "Fan Shop", "Fitness", "Footwear", "Golf", "Health and Beauty", "Outdoors", "Pet Shop", "Technology"))
## Delivery Status grouped by Quarter
ggplot(data = dfdataexplore)+geom_bar(position = "dodge", mapping = aes(x=`orderquarter`,fill=`Delivery Status`))+scale_fill_manual("Delivery Status",values=c("Advance shipping" = "Dark Blue","Late delivery" = "Orange","Shipping on time" = "Green", "Shipping canceled" = "Red"),
                                                                                                                                labels = c("Advanced Delivery","Late Delivery","Shipping canceled", "Shipping on time"))+labs(title = "Delivery Statuses Per Quarter")+xlab("Quarter")+ylab("No. of Orders")
## Delivery Status grouped by Year
ggplot(data = dfdataexplore)+geom_bar(position = "dodge", mapping = aes(x=`orderyear`,fill=`Delivery Status`))+scale_fill_manual("Delivery Status",values=c("Advance shipping" = "Dark Blue","Late delivery" = "Orange","Shipping on time" = "Green", "Shipping canceled" = "Red"),
                                                                                                                             labels = c("Advanced Delivery","Late Delivery","Shipping canceled", "Shipping on time"))+labs(title = "Delivery Statuses Per Year")+xlab("Year")+ylab("No. of Orders")

## Total Benefit per Year
ggplot(data=dfdataexplore, aes(x=orderyear, y=sum(`Benefit per order`))) + geom_bar(stat="identity",color="dark blue", fill="white") +labs(title = "Total Benefit per Year") + xlab("Year") + ylab("Benefit")+scale_y_continuous(labels = dollar)

## Total Sales per Year
ggplot(data=dfdataexplore, aes(x=orderyear, y=sum(`Sales`))) + geom_bar(stat="identity",color="dark blue", fill="white") +labs(title = "Total Sales per Year") + xlab("Year") + ylab("Sales")+scale_y_continuous(labels = dollar)

## Delivery Statuses per Shipping Mode
ggplot(data = dfdataexplore)+geom_bar(position = "dodge", 
                                  mapping = aes(x=`Shipping Mode`,fill=`Delivery Status`))+scale_fill_manual("Delivery Status",values=c("Advance shipping" = "Dark Blue","Late delivery" = "Orange","Shipping on time" = "Green", "Shipping canceled" = "Red"),
                                                                                                             labels = c("Advanced Delivery","Late Delivery","Shipping canceled", "Shipping on time"))+labs(title = "Delivery Statuses Per Shipping Mode")+xlab("Shipping Mode")+ylab("No. of Orders")+scale_x_discrete(labels = c("First Class","Same Day", "Second Class","Standard Class"))
## Late Deliveries per Shipping Mode
ggplot(data = dfdataexplore)+geom_bar(position = "dodge", 
                                  mapping = aes(x=`Shipping Mode`,fill=`Late_delivery_risk`))+scale_fill_manual("Late Delivery",values=c("0" = "Dark Blue","1" = "Orange"),
                                                                                                                labels = c("0","1"))+labs(title = "Late Deliveries Per Shipping Mode")+xlab("Shipping Mode")+ylab("No. of Orders")+scale_x_discrete(labels = c("First Class","Same Day", "Second Class","Standard Class"))



#### >>K means Clustering ####

dfKmeans <- copy(dfsupplychain.cleaned)
dfKmeans$DistanceinGM <- dfKmeans$Distance/1000 #Distance in millions used for the k-means clustering algorithm in order for the clustering to put less emphasis on distance and reflect relationship between other factors


## Based on documentation for K means clustering, only numerical values should be used
## Using "Benefit per order", "Sales per customer", "Days for shipping (real)", "Order Item Discount Rate", "Order Item Profit Ratio", "DistanceinGM"
mydata = select(dfKmeans,c("Benefit per order", "Sales per customer", "Days for shipping (real)", "Order Item Discount Rate", "Order Item Profit Ratio", "DistanceinGM"))


#Within sum of square (WSS) plot to find optimal number of clusters

wssplot <- function(data, nc = 15, seed = 2004)
{
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data,centers=i)$withinss)
  }
  plot(1:nc, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
}

wssplot(mydata)
#Graph shows optimal number of clusters at 4 (Elbow method)
#Increasing number of clusters beyond this point would have diminishing returns
#since variability of observations within each cluster does not decrease significantly

## K-means clustering analysis
K.means.analysis.4 <- kmeans(mydata,4)

## autoplot function to plot principal component analysis (PCA) graph
autoplot(K.means.analysis.4,mydata,frame=TRUE)

## Cluster Centres
K.means.analysis.4$centers

## Visualising clusters
Kmeans.clusters = copy(dfKmeans)
Kmeans.clusters$clusters = factor(K.means.analysis.4$cluster, ordered = T, levels = c(1,2,3,4))

## Number of observations in each cluster
table(Kmeans.clusters$clusters)

## Plotting benefit per order based on cluster
ggplot(Kmeans.clusters, aes(x=`clusters`, y=`Benefit per order`, fill=clusters)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none")

## Plotting sales per customer based on cluster
ggplot(Kmeans.clusters, aes(x=`clusters`, y=`Sales per customer`, fill=clusters)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none")



#### Data Modelling - Continuous Y Variable ####

#### >>Linear Regression - Benefit per order  ####

dfLinearReg <- copy(dfsupplychain.cleaned)

## Dropping irrelevant variables (Based on reasons stipulated in the report)
## (Some columns capture similar information whereas others do not directly affect order profitability)
dfLinearReg[, c("Customer State",
        "Category Id",
        "Category Name",
        "Days for shipping (real)",
        "Days for shipment (scheduled)",
        "Delivery Status",
        "Late_delivery_risk",
        "Customer Segment",
        "Market",
        "Order City",
        "Order Item Total", 
        "Order Profit Per Order", 
        "Order Region", 
        "Order State",
        "Order Status", 
        "Product Price", 
        "Product Name", 
        "Order Item Quantity",
        "orderdate", 
        "Shipping Mode",
        "Sales per customer",
        "Sales",
        "Order Item Discount",
        "Department Name"):=NULL]

## Using backward elimination linear regression on the full dataset to determine significant variables
## All the independent variables are entered into the equation first and each one is deleted one at a time if they do not contribute to the regression equation
lm1 <- lm(`Benefit per order` ~ ., data = dfLinearReg)
summary(lm1)
lm1.stepped <- step(lm1) # Akaike Information Criterion
lm1.stepped
vif(lm1.stepped) # GVIF of all variables < 1.7

# Model equation obtained from backward elimination: `Benefit per order` ~ `Order Item Discount Rate`+ `Order Item Product Price`+`Order Item Profit Ratio` + `Department Id`


## Train-test split using seed of 2004 for reproducability and consistency in train test splitting
## Tried using caTools sample.split, same train-test split cannot be reproduced despite setting same seed value
## Hence, using base R sample.int to carry out train-test split
set.seed(2004)
sample <- sample.int(n = nrow(dfLinearReg), size = floor(.70*nrow(dfLinearReg)), replace = F)
trainset <- dfLinearReg[sample, ]
testset  <- dfLinearReg[-sample, ]
summary(trainset$`Benefit per order`)
summary(testset$`Benefit per order`)


## Developing the linear regression model using identified significant variables and trainset as training data
lmBenefitTry <- lm(`Benefit per order` ~ `Order Item Discount Rate`+ `Order Item Product Price`+`Order Item Profit Ratio` + `Department Id`, data = trainset)
summary(lmBenefitTry) # Adjusted R^2 value = 0.6944
# Since only the dummy variable of Department Id 3 is statistically significant, to remove Department Id from model

## Developing final linear regression model using statistically significant variables and trainset as training data
lmBenefit <- lm(`Benefit per order` ~ `Order Item Discount Rate`+ `Order Item Product Price`+`Order Item Profit Ratio`, data = trainset)
summary(lmBenefit) # Adjusted R^2 value = 0.6933. Decrease from 0.6944 is not a significant drop. 
residuals(lmBenefit) 
vif(lmBenefit) # VIF values < 1.1 

## Testing
# Trainset Error
RMSE.train <- sqrt(mean(residuals(lmBenefit)^2))  # RMSE on trainset based on m5 model.
summary(abs(residuals(lmBenefit)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
predict.lm.test <- predict(lmBenefit, newdata = testset)
testset.error <- testset$`Benefit per order` - predict.lm.test

# Testset Errors
RMSE.test <- sqrt(mean(testset.error^2))
summary(abs(testset.error))

## Train set and test set accuracy of model
c(RMSE.train, RMSE.test)

## Linear Regression model Visualisation Graph (Discount Rate)
ggplot(data = testset, aes(x = `Benefit per order`, y = testset$`Order Item Discount Rate`)) + 
  geom_point()+facet_grid(orderquarter~.) + 
  labs(title = "Discount Rate vs Benefit per order vs Order Quarter") + 
  geom_smooth(method = "lm", se = TRUE)+ylab("Order Item Discount Rate")

## Linear Regression model Visualisation Graph (Product Price)
ggplot(data = testset, aes(x = `Benefit per order`, y = testset$`Order Item Product Price`)) + 
  geom_point()+facet_grid(orderquarter~.) + 
  labs(title = "Product Price vs Benefit per order vs Order Quarter") + 
  geom_smooth(method = "lm", se = TRUE)+ylab("Order Item Product Price")



#### >>CART Methodology (Regression) - Benefit per order ####

dfContCART <- copy(dfsupplychain.cleaned)

## Dropping Order Profit Per Order, Order Item Total, sales per customer and sales, 
## since they are different ways of presenting information captured in "Benefit per Order"
dfContCART$`Sales per customer` <- NULL
dfContCART$Sales <- NULL
dfContCART$`Order Profit Per Order` <- NULL
dfContCART$`Order Item Total` <- NULL

## Department Name, Category Name and Product Price information are represented as Department Id, Category Id and Order Item Product Price
dfContCART$`Department Name`<- NULL
dfContCART$`Category Name`<- NULL
dfContCART$`Product Price` <- NULL

## Train-test split using seed of 2004 for reproducability and consistency in train test splitting
set.seed(2004)
sample <- sample.int(n = nrow(dfContCART), size = floor(.70*nrow(dfContCART)), replace = F)
trainset <- dfContCART[sample, ]
testset  <- dfContCART[-sample, ]
summary(trainset$`Benefit per order`)
summary(testset$`Benefit per order`)


# Since rpart() completes phrase 1 & 2 automatically, 
# to change minsplit and cp to grow the tree to the maximal
set.seed(2004)
CARTBenefitMax <- rpart(`Benefit per order` ~., data = trainset, method = 'anova',control = rpart.control(minsplit = 5000, cp = 0))
printcp(CARTBenefitMax, digits = 3)

# plots the maximal tree and results.
rpart.plot(CARTBenefitMax, nn= T, main = "Maximal Tree Generated")

## Automated geometric mean finding
CVerror.cap <- CARTBenefitMax$cptable[which.min(CARTBenefitMax$cptable[,"xerror"]), "xerror"] + CARTBenefitMax$cptable[which.min(CARTBenefitMax$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree
i <- 1; j<- 4
while (CARTBenefitMax$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cp.opt = ifelse(i > 1, sqrt(CARTBenefitMax$cptable[i,1] * CARTBenefitMax$cptable[i-1,1]), 1)

# Plug optimal cp to prune model
CARTBenefitOptimal <- prune(CARTBenefitMax, cp = cp.opt)
printcp(CARTBenefitOptimal, digits = 3)

print(CARTBenefitOptimal)

rpart.plot(CARTBenefitOptimal, nn = T, main = "Pruned Tree") # Plotting optimal tree


## Variable Importance

CARTBenefitOptimal$variable.importance

# Scaling Variable Impt so as to rep as percentage impt 
ScaledVariableImpt <- round(100*CARTBenefitOptimal$variable.importance/sum(CARTBenefitOptimal$variable.importance))
ScaledVariableImpt[ScaledVariableImpt > 3]

summary(CARTBenefitOptimal)


## Testing
RMSE.train <- sqrt(mean(residuals(CARTBenefitOptimal)^2))  # RMSE on train set
RMSE.train
summary(abs(residuals(CARTBenefitOptimal)))  # Check Min Abs Error and Max Abs Error.

testset.prediction <- predict(CARTBenefitOptimal, newdata = testset)
testset.error <- testset$`Benefit per order` - testset.prediction

RMSE.test <- sqrt(mean(testset.error^2))  # RMSE on test set
RMSE.test
summary(abs(testset.error))

## Train set and test set accuracy of model
c(RMSE.train, RMSE.test)


## Calculation of R^2
# Relative error of CART is (1 - R^2). References:
# https://stats.stackexchange.com/questions/103018/difference-between-rel-error-and-xerror-in-rpart-regression-trees#:~:text=The%20%22rel%20error%22%20is%201,observations%20from%20cross%20validation%20data.
# https://stats.stackexchange.com/questions/5792/r-square-from-rpart-model
cptable.extract <- printcp(CARTBenefitOptimal)
cptable.extract[,c(3,4)]
rsquare.val <- 1-cptable.extract[,c(3,4)]
rsquare.val[nrow(rsquare.val)]



#### Data Modelling - Categorical Y Variable ####

#### >>Logistic Regression - Delivery Status ####

dfLogReg <- copy(dfsupplychain.cleaned)

## Dropping irrelevant variables (Based on reasons stipulated in the report)
## (Some columns capture similar information whereas others do not directly affect order profitability)
dfLogReg[, c("Type", 
        "Benefit per order", 
        "Sales per customer",
        "Category Id",
        "Customer State", 
        "Category Name",
        "Market", 
        "Order City",
        "Order Item Discount Rate",
        "Order Item Discount",
        "Order Item Product Price", 
        "Order Item Profit Ratio",
        "Sales", 
        "Order Item Total", 
        "Order Profit Per Order", 
        "Order Region", 
        "Order State",
        "Product Name",
        "Product Price",
        "Department Name",
        "Days for shipping (real)",
        "Order Status",
        "orderquarter"):=NULL] 


## Normalising the "Distance" column
dfLogReg$Distance <- dfLogReg$Distance/max(dfLogReg$Distance)

## Releveling baseline level from Advance Shipping to Shipping on time
levels(dfLogReg$`Delivery Status`)  # Default baseline is Status = Advance Shipping
dfLogReg$`Delivery Status` <- relevel(dfLogReg$`Delivery Status`, ref = "Shipping on time")
levels(dfLogReg$`Delivery Status`)

## Logistic regression involving Delivery Status as the Y variable will exclude Late_delivery_risk as an input variable


## Using backward elimination logistic regression on the full dataset to determine significant variables
## All the independent variables are entered into the equation first and each one is deleted one at a time if they do not contribute to the regression equation
multinom1 <- multinom(`Delivery Status` ~.- Late_delivery_risk, data = dfLogReg)
summary(multinom1)
multinom1.stepped <- step(multinom1)
summary(multinom1.stepped)

# Model equation obtained from backward elimination: `Delivery Status` ~ `Days for shipment (scheduled)` + `Customer Segment` + `Shipping Mode` + shippingquarter


## Train-test split using seed of 2004 for reproducability and consistency in train test splitting
set.seed(2004)
sample <- sample.int(n = nrow(dfLogReg), size = floor(.70*nrow(dfLogReg)), replace = F)
trainset <- dfLogReg[sample, ]
testset  <- dfLogReg[-sample, ]
summary(trainset$`Delivery Status`)
summary(testset$`Delivery Status`) 


## Developing the logistic regression model using identified significant variables and trainset as training data
multinomDeliveryStatus <- multinom(`Delivery Status` ~ `Days for shipment (scheduled)` + `Customer Segment` + `Shipping Mode` + shippingquarter, data = trainset)
summary(multinomDeliveryStatus)

# Odds ratio
OR.DelStatus <- exp(coef(multinomDeliveryStatus))
OR.DelStatus

# 95% Confidence interval
OR.DelStatus.CI <- exp(confint(multinomDeliveryStatus))
OR.DelStatus.CI

## Testing

# Trainset
train.logreg.predict <- predict(multinomDeliveryStatus, newdata = trainset, type = "class")
results <- data.frame(trainset$`Delivery Status`, train.logreg.predict)
trainset.accuracy <- mean(results$trainset..Delivery.Status == results$train.logreg.predict)
trainset.accuracy

# Testset
test.logreg.predict <- predict(multinomDeliveryStatus, newdata = testset, type = "class")
results <- data.frame(testset$`Delivery Status`, test.logreg.predict)
testset.accuracy <- mean(results$testset..Delivery.Status == results$test.logreg.predict)
testset.accuracy

## Train set and test set accuracy of model
c(trainset.accuracy, testset.accuracy)


## Confusion Matrix
LogReg.Prediction <- predict(multinomDeliveryStatus, newdata = testset, type = 'class')
LogReg.Confusion.matrix <- table(testset$`Delivery Status`, LogReg.Prediction, deparse.level = 2)
LogReg.Confusion.matrix

## Calculation of accuracy, precision, recall (WITH REGARDS TO OBSERVATIONS OF LATE DELIVERY)
LogReg.accuracy <- (LogReg.Confusion.matrix[1,1]+LogReg.Confusion.matrix[2,2]+
                      LogReg.Confusion.matrix[3,3]+LogReg.Confusion.matrix[4,4])/sum(LogReg.Confusion.matrix)
LogReg.precision <- LogReg.Confusion.matrix[3,3]/sum(LogReg.Confusion.matrix[c(1:4),3])
LogReg.recall <- LogReg.Confusion.matrix[3,3]/sum(LogReg.Confusion.matrix[3,c(1:4)])

c(LogReg.accuracy, LogReg.precision, LogReg.recall)



#### >>Logistic Regression - Late_delivery_risk ####

## Using the same dataset as the logistic regression for Delivery Status

## In this case, Logistic regression involving Late_delivery_risk as the Y variable will exclude Delivery Status as an input variable
## Using backward elimination logistic regression on the full dataset to determine significant variables
glm1 <- glm(`Late_delivery_risk` ~.-`Delivery Status`, family = binomial, data = dfLogReg)
summary(glm1)
glm1.stepped <- step(glm1)
summary(glm1.stepped)

## Model equation obtained from backward elimination: Late_delivery_risk ~ `Shipping Mode` + ordercountry + Distance
## From the summary() function, it can be seen that ordercountry is largely statistically insignificant, thus removed

## Train-test split using seed of 2004 for reproducability and consistency in train test splitting
set.seed(2004)
sample <- sample.int(n = nrow(dfLogReg), size = floor(.70*nrow(dfLogReg)), replace = F)
trainset <- dfLogReg[sample, ]
testset  <- dfLogReg[-sample, ]
summary(trainset$`Late_delivery_risk`)
summary(testset$`Late_delivery_risk`) 

## Developing the logistic regression model using identified significant variables and trainset as training data
glmLateDelivery <- glm(Late_delivery_risk ~ `Shipping Mode` + Distance, family = binomial, data = trainset)
summary(glmLateDelivery)

## Odds ratio
OR.LateDelivery <- exp(coef(glmLateDelivery))
OR.LateDelivery

## Confidence Interval
OR.LateDelivery.CI <- exp(confint(glmLateDelivery)) 
OR.LateDelivery.CI

## Testing

# train set
LogRegProbability <- predict(glmLateDelivery, newdata = trainset, type = 'response')
LogRegProbability
threshold <- 0.5
LogReg.Prediction <- ifelse(LogRegProbability > threshold, '1', '0')
table(trainset$`Late_delivery_risk`, LogReg.Prediction, deparse.level = 2)
trainset.accuracy <- mean(LogReg.Prediction == trainset$`Late_delivery_risk`)
trainset.accuracy

# test set
LogRegProbability <- predict(glmLateDelivery, newdata = testset, type = 'response')
LogRegProbability
threshold <- 0.5
LogReg.Prediction <- ifelse(LogRegProbability > threshold, '1', '0')
table(testset$`Late_delivery_risk`, LogReg.Prediction, deparse.level = 2)
testset.accuracy <- mean(LogReg.Prediction == testset$`Late_delivery_risk`)
testset.accuracy

## Train set and test set accuracy of model
c(trainset.accuracy, testset.accuracy)


## Confusion Matrix
LogRegProbability <- predict(glmLateDelivery, newdata = testset, type = 'response')
LogRegProbability
threshold <- 0.5
LogReg.Prediction <- ifelse(LogRegProbability > threshold, '1', '0')
LogReg.Confusion.matrix <- table(testset$`Late_delivery_risk`, LogReg.Prediction, deparse.level = 2)

LogReg.Confusion.matrix

## Calculation of accuracy, precision, recall
LogReg.accuracy <- (LogReg.Confusion.matrix[1,1]+LogReg.Confusion.matrix[2,2])/sum(LogReg.Confusion.matrix)
LogReg.precision <- LogReg.Confusion.matrix[2,2]/sum(LogReg.Confusion.matrix[c(1,2),2])
LogReg.recall <- LogReg.Confusion.matrix[2,2]/sum(LogReg.Confusion.matrix[2,c(1,2)])

c(LogReg.accuracy, LogReg.precision, LogReg.recall)



#### >>CART Methodlogy (Classification) - Late_delivery_risk ####

dfCatCART <- copy(dfsupplychain.cleaned)

## Dropping delivery status, days for shipping (real), days for shipping (scheduled)
## since they are different ways of presenting information captured in "Late_delivery_risk"
dfCatCART$`Delivery Status` <- NULL
dfCatCART$`Days for shipping (real)` <- NULL
dfCatCART$`Days for shipment (scheduled)` <- NULL
dfCatCART$`Order Status` <- NULL

## Dropping Order Profit Per Order, Order Item Total, sales per customer and sales, 
## since they are different ways of presenting information captured in "Benefit per Order"
dfCatCART$`Sales per customer` <- NULL
dfCatCART$Sales <- NULL
dfCatCART$`Order Profit Per Order` <- NULL
dfCatCART$`Order Item Total` <- NULL

## Department Name, Category Name and Product Price information are represented as Department Id, Category Id and Order Item Product Price
dfCatCART$`Department Name`<- NULL
dfCatCART$`Category Name`<- NULL
dfCatCART$`Product Price` <- NULL


## Train-test split using seed of 2004 for reproducability and consistency in train test splitting
set.seed(2004)
sample <- sample.int(n = nrow(dfCatCART), size = floor(.70*nrow(dfCatCART)), replace = F)
trainset <- dfCatCART[sample, ]
testset  <- dfCatCART[-sample, ]
summary(trainset$`Late_delivery_risk`)
summary(testset$`Late_delivery_risk`) 


# rpart() completes phrase 1 & 2 automatically.
# Change two default settings in rpart: minsplit and cp.
CARTLateDelMax <- rpart(`Late_delivery_risk` ~., data = trainset, method = 'class',control = rpart.control(minsplit = 5000, cp = 0))

# plots the maximal tree and results.
rpart.plot(CARTLateDelMax, nn= T, main = "Maximal Tree")

# prints the maximal tree CARTLateDelMax onto the console.
print(CARTLateDelMax)

# prints out the pruning sequence and 10-fold CV errors, as a table.
printcp(CARTLateDelMax)

# Display the pruning sequence and 10-fold CV errors, as a chart.
plotcp(CARTLateDelMax, main = "Subtrees in cleaned supply chain dataset")

## Automated geometric mean finding
CVerror.cap <- CARTLateDelMax$cptable[which.min(CARTLateDelMax$cptable[,"xerror"]),"xerror"] + CARTLateDelMax$cptable[which.min(CARTLateDelMax$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (CARTLateDelMax$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cp.opt = ifelse(i > 1, sqrt(CARTLateDelMax$cptable[i,1] * CARTLateDelMax$cptable[i-1,1]), 1)

# Plug optimal cp to prune model
CARTLateDelOptimal <- prune(CARTLateDelMax, cp = cp.opt)

printcp(CARTLateDelOptimal, digits = 3)
print(CARTLateDelOptimal)
rpart.plot(CARTLateDelOptimal, nn= T, main = "Pruned Tree")


## Variable Importance

CARTLateDelOptimal$variable.importance

# Scaling Variable Impt so as to rep as percentage impt 
ScaledVariableImpt <- round(100*CARTLateDelOptimal$variable.importance/sum(CARTLateDelOptimal$variable.importance))
ScaledVariableImpt[ScaledVariableImpt > 3]

summary(CARTLateDelOptimal)


## Accuracy of cart

# trainset
train.cart.predict <- predict(CARTLateDelOptimal, newdata = trainset, type = "class")
results <- data.frame(trainset$Late_delivery_risk, train.cart.predict)
trainset.accuracy <- mean(results$trainset.Late_delivery_risk == results$train.cart.predict)
trainset.accuracy

# testset
test.cart.predict <- predict(CARTLateDelOptimal, newdata = testset, type = "class")
results <- data.frame(testset$Late_delivery_risk, test.cart.predict)
testset.accuracy <- mean(results$testset.Late_delivery_risk == results$test.cart.predict)
testset.accuracy

c(trainset.accuracy, testset.accuracy)


## Confusion Matrix
CART.Prediction <- predict(CARTLateDelOptimal, newdata = testset, type = 'class')
CART.Confusion.matrix <- table(testset$`Late_delivery_risk`, CART.Prediction, deparse.level = 2)

CART.Confusion.matrix

## Calculation of accuracy, precision, recall
CART.accuracy <- (CART.Confusion.matrix[1,1]+CART.Confusion.matrix[2,2])/sum(CART.Confusion.matrix)
CART.precision <- CART.Confusion.matrix[2,2]/sum(CART.Confusion.matrix[c(1,2),2])
CART.recall <- CART.Confusion.matrix[2,2]/sum(CART.Confusion.matrix[2,c(1,2)])

c(CART.accuracy, CART.precision, CART.recall)



#### Data Modelling - Time Series Forecasting Models ####

## visualising the sales data against time to evaluate if train-test split is feasible
salesdata <- copy(dfsupplychain.cleaned)
salesdata <- salesdata[,c("orderdate","Sales")]
salesdata <- aggregate(Sales~orderdate, data = salesdata, FUN=sum)
ggplot(data = salesdata, mapping = aes(orderdate, Sales)) +
  geom_line()

# Huge variability in sales data towards the end of 2018.
# The only way for a train-test split to be carried out is to treat the sales from the later dates as test sets
# Given the huge variability towards the end of the time period captured by the dataset,
# it is likely that the test set would not provide an objective assessment of the models built in this section.
# Should a train-test split be carried out,
# the test set would contain sales data reflecting the huge variability, and on the other hand
# the train set would not be able to capture the trend that only surfaced towards the later periods in the dataset

# Given that the analysis of time series forecasting models are carried out with a purely exploratory objective,
# both prophet and exponential forecasting models will be generated with the entire dataset as the training data.
# While there is the risk of overfitting the time series based models without the use of test set data,
# It was decided that train-test split is not a feasible choice given the huge variability of sales data in the later years
# Since the team's intention to explore relevant models that can be adopted by SCMs,
# the team decided that the risk of overfitting the models in this case is 



#### >>Prophet - Sales ####

## Using only orderdate and sales variables
dfProphet <- copy(dfsupplychain.cleaned)
dfProphet <- dfProphet[,c("orderdate","Sales")] 

## Calculating the sum of sales for each day
dfProphetSummed <- aggregate(Sales~orderdate, data = dfProphet, FUN=sum)
dfProphetSummed$orderdate <- as.Date(dfProphetSummed$orderdate) # Changing type from POSIXct to Date

## Renaming orderdate to ds and daily sales to y for prophet algorithm
colnames(dfProphetSummed) <- c("ds","y")

## Visualizing Sales data
ggplot(data = dfProphetSummed, mapping = aes(ds, y)) +
  geom_line()

## Building the prophet model
ProphetModel <- prophet(dfProphetSummed)

## Forecasting the future (365 days)
future = make_future_dataframe(ProphetModel, periods = 365) ## 365 days / 1 year into the future
forecast = predict(ProphetModel, future) ## prediction of sales 

## Visualising prophet model
plot(ProphetModel, forecast, xlab = "Years", ylab = "Sales")
prophet_plot_components(ProphetModel, forecast) ## components

## Accuracy of model (Based on entire dataset)
ProphetPrediction <- data.frame(ProphetPredict = forecast[1:1127,]$yhat, ProphetActual = dfProphetSummed$y)
ProphetPrediction
Prophet.RMSE <- sqrt(mean((ProphetPrediction$ProphetActual-ProphetPrediction$ProphetPredict)^2))
Prophet.RMSE



#### >>Exponential Smoothing - Sales ####

## Using only orderdate and sales variables
dfExpSmooth <- copy(dfsupplychain.cleaned)
dfExpSmooth <- dfExpSmooth[,c("orderdate","Sales")]
dfExpSmooth$orderdate <- as.Date(dfExpSmooth$orderdate) # Changing type from POSIXct to Date

## Calculating the sum of sales for each day
dfExpSmooth$orderdate <- as.Date(dfExpSmooth$orderdate) # Changing type from POSIXct to Date
dfExpSmoothSummed <- aggregate(Sales~orderdate, data = dfExpSmooth, FUN=sum)

## Creating time series, frequency = 365 as data is collected for 365 days in one year
Y <- ts(dfExpSmoothSummed[,2],frequency=365)
ExpSmoothModel <- ses(Y, h=365) #Projecting latest forecast value over 365 days

## Visualising Model - Plotting Exponential Smoothing
autoplot(ExpSmoothModel) + ggtitle("Exponential Smoothing Sales Forecast")+ylab("Sales") + 
  scale_x_discrete(limits = c("2015","2016","2017","2018","2019"),expand = c(0,0.05))+scale_y_continuous(labels = comma)+
  theme_classic(base_size=15)

## Accuracy of model (Based on entire dataset)
summary(ExpSmoothModel)
#RMSE 6026.777
