#Getting Directory
getwd()

#cleaning environment
rm(list = ls())

#setting Directory
setwd('C:/Users/cabfareprediction')

#installing and loading libraries
x = c('ggplot2','corrgram','DMwR','caret','randomForest','unbalanced','C50','rpart','sampling')
lapply(x,require,character.only = TRUE)

data = read.csv('train_cab.csv')

data$fare_amount = as.numeric(as.character((data$fare_amount)))
View(data)

## converting pickup datetime to date time class and extracting different features.
data$pickup_datetime=as.character(data$pickup_datetime)
data$pickup_datetime<- as.POSIXct(data$pickup_datetime,format = "%Y-%m-%d %H:%M:%S",tz="")


data$date=as.numeric(format(data$pickup_datetime, format = "%d",  usetz = FALSE))
data$hour=as.numeric(format(data$pickup_datetime, format = "%H",  usetz = FALSE))
data$month=as.numeric(format(data$pickup_datetime, format = "%m",  usetz = FALSE))
data$year=as.numeric(format(data$pickup_datetime, format = "%Y",  usetz = FALSE))
data$dayofweek=as.numeric(format(data$pickup_datetime, format = "%u",  usetz = FALSE))
## Missing Value Analysis------------------------------------------------

missing_val = data.frame(apply(data,2,function(x){sum(is.na(x))}))
View(missing_val)

data = na.omit(data)

## Outlier Analysis------------------------------------------------------
dim(data)
numeric_index = sapply(data,is.numeric) #selecting only numeric
numeric_data = data[,numeric_index]
cnames = colnames(numeric_data)

# Get the Boxplots of various variables
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i])), data = subset(data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           ggtitle(paste("Box plot of ",cnames[i])))
}

gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)

## detect and delete data from outliers
cnames = c('fare_amount','pickup_latitude','pickup_longitude','dropoff_latitude','dropoff_longitude')
for(i in cnames){
  print(i)
  val = data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
  print(length(val))
  data = data[which(!data[,i] %in% val),]
}



data = subset(data,passenger_count<7 & passenger_count >=1)
data = subset(data, fare_amount > 0 )

#Feature Extraction
#Feature Extraction from existing available variables
install.packages('geosphere')
library(geosphere)
data$distance = distHaversine(cbind(data$pickup_longitude, data$pickup_latitude),
                                                cbind(data$dropoff_longitude,data$dropoff_latitude))

data$distance = data$distance/1000
summary(data$distance)
 
#contains 0 as minimum , but distance can not be zero

data = subset(data, data$distance != 0)

# plotting distance vs fare_amount scatter graph

plot(x = data$fare_amount, y = data$distance, main = 'Trip distance vs fare amount',xlab = 'fare amount', ylab = 'distance',pch = 19,col.sub="blue"  )
### we can see the  linear varation of fare amount with distance.

# Removing outliers from other features
data= subset(data, (pickup_longitude <180 & pickup_longitude > -180))
data= subset(data, (pickup_latitude<90 & pickup_latitude > -90))
data= subset(data, (dropoff_latitude<90 & dropoff_latitude > -90))
data= subset(data, (dropoff_longitude <180 & dropoff_longitude > -180))

data= subset(data, (pickup_longitude != 0.00000 & pickup_longitude != 0.00000))
data= subset(data, (dropoff_longitude != 0.00000 & dropoff_longitude != 0.00000))

summary(data)

#dropping pickup_datetime
## As subset function does not drop the POSIXT objects, so we have to first convert 
## it into the string class

data$pickup_datetime = toString(data$pickup_datetime)
class(data$pickup_datetime)
data = subset(data, select = -pickup_datetime)
View(data)

## plotting different graphs to visualize the data

## pickup in day of week

barplot(table(data$dayofweek),beside = TRUE, legend =FALSE,xlab = 'pickup day ',
        ylab = 'number of trips',col='salmon')

## pickup year 

barplot(table(data$year),beside = TRUE, legend =FALSE,xlab = 'pickup year ',
        ylab = 'number of trips',col='salmon')
## pickup month

barplot(table(data$month),beside = TRUE, legend =FALSE,xlab = 'pickup month',
        ylab = 'number of trips',col='salmon')

## pickup hour

barplot(table(data$hour),beside = TRUE, legend =FALSE,xlab = 'pickup hour',
        ylab = 'number of trips',col='salmon')

## passenger count

barplot(table(data$passenger_count),beside = TRUE, legend =FALSE,xlab = 'passeger count ',
        ylab = 'number of trips',col='salmon')

## pickup day

barplot(table(data$date),beside = TRUE, legend =FALSE,xlab = 'pickup day ',
        ylab = 'number of trips',col='salmon')

## correlation between variables
corrgram(data[,numeric_index],order = F, upper.panel = panel.pie,text.panel = panel.txt, main = 'correlation plot')


# checking multicollinearity
library(usdm)
vif(data[,-1])
vifcor(data[,-1], th = 0.9)


## MODELLING----------------------------------------------------------------------
# 25% validatoin set
set.seed(123)
train_index = sample(1:nrow(data),0.75*nrow(data))
train = data[train_index,]
test = data[-train_index,]

# error metric

MAPE = function(y, yhat){
  mean(abs((y - yhat)/y)*100)
}

## MODEL DEVELOPMENT---------------------------------------------------------------



##Random Forest----------------------

library(randomForest)
model_RF =randomForest(fare_amount ~ ., data = train, importance = TRUE)
summary(model_RF)
predictions_RF = predict(model_RF, test[,-1])
#accuracy----- 82.81472%
100-MAPE(test[,1],predictions_RF)
library(DMwR)
#error metric
regr.eval(test[,1], predictions_RF,stats=c("mape","rmse"))
importance(model_RF)




##Decision tree-----------------------

library(rpart)
model_DT = rpart(fare_amount ~ ., data = train, method = "anova")
summary(model_DT)
predictions_DT = predict(model_DT, test[,-1])
#accuracy-----79.44429%
100-MAPE(test[,1], predictions_DT)
#error metric
regr.eval(test[,1], predictions_RF,stats=c("mape","rmse"))





## Linear Regression ------------------------------

model_LR = lm(fare_amount ~., data = train)
summary(model_LR)
predictions_LR = predict(model_LR, test[,2:12])
#accuracy---------81.77481
100-MAPE(test[,1], predictions_LR)
#Error Metric
regr.eval(test[,1], predictions_LR,stats=c("mape","rmse"))




## Since the accuracy of random forest is highest, hence fixing the random forest model
## for test data



##  Predicting in test data--------------------------


## cleaning test data -----------------------

test_data = read.csv('test.csv')
View(test_data)
test_data$pickup_datetime=as.character(test_data$pickup_datetime)
test_data$pickup_datetime<- as.POSIXct(test_data$pickup_datetime,format = "%Y-%m-%d %H:%M:%S",tz="")


test_data$date=as.numeric(format(test_data$pickup_datetime, format = "%d",  usetz = FALSE))
test_data$hour=as.numeric(format(test_data$pickup_datetime, format = "%H",  usetz = FALSE))
test_data$month=as.numeric(format(test_data$pickup_datetime, format = "%m",  usetz = FALSE))
test_data$year=as.numeric(format(test_data$pickup_datetime, format = "%Y",  usetz = FALSE))
test_data$dayofweek=as.numeric(format(test_data$pickup_datetime, format = "%u",  usetz = FALSE))

# checking for missing values
missing_val_test = data.frame(apply(test_data,2,function(x){sum(is.na(x))}))
View(missing_val_test) #no missing values

test_data$pickup_datetime = toString(test_data$pickup_datetime)
class(test_data$pickup_datetime)
test_data = subset(test_data, select = -pickup_datetime)
View(test_data)

# adding distance feature in test data
test_data$distance = distHaversine(cbind(test_data$pickup_longitude, test_data$pickup_latitude),
                              cbind(test_data$dropoff_longitude,test_data$dropoff_latitude))

test_data$distance = test_data$distance/1000
summary(test_data$distance)

View(test_data)

## Predicting data-----------------------------
predicted_fare_amount = predict(model_RF,test_data[,1:11])
test_data$predicted_fare_amount = predicted_fare_amount
write.csv(test_data,'cab_fare_predictionsR.csv',row.names = FALSE)
View(test_data)

