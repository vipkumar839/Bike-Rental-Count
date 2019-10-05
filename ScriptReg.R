#Including required libraries
library("ggplot2")
library("scales")
library(usdm)
library(e1071)
library(car)
library(kernlab)
library(caret)

#Reading the given data into train_data
train_data=read.csv("day.csv",header=TRUE,sep = ",")
 
head(train_data)
#After analysing the provided data we can conclude that
#The columns "instant","dteday" and "yr" are irrelevant for predicting the final count.
#Also the division of count of users into registered and casual is irrelevant as we are predicting the total count.
#Therefore we will remove these columns to get our train data.
train=train_data[,-c(1,2,4,14,15)]
str(train)
col<- c('season','mnth','holiday','weekday','workingday','weathersit')
train[,col] <- lapply(train[,col], factor)
str(train)

#Check if any value is missing in the data.
missinig_value= data.frame(apply(train,2,function(x){sum(is.na(x))}))
#No missing value is found.

#OUTLIER ANALYSIS
#Only continuos variables are checked for outliers.
#Saving the names of continuos variables in cnames.
cnames= c('temp','atemp','hum','windspeed')

#Box plot
for (i in 1:4)
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), data = subset(train))+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=3, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="cnt")+
           ggtitle(paste("Box plot for",cnames[i])))
}

## Plotting plots together
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,ncol=4)

#from the box plots we can clearly conclude that only windspeed and humidity have some outliers.
#Removing outliers of attributes windspeed and humidity.

for(i in c('hum','windspeed')){
  
  val = train[,i][train[,i] %in% boxplot.stats(train[,i])$out]
  train = train[which(!train[,i] %in% val),]
}

#COORELATION ANALYSIS
VIfDf=data.frame(vif(train[,-11]))
vifcor(train[,-11], th = 0.9)

#atemp variable have collinearity problem.
#Removing the variable
train=train[,-8]

#Scaling Data
#Normality check
hist(train$temp)

#Normalisation
for(i in c('temp','hum','windspeed')){
  print(i)
  train[,i] = (train[,i] - min(train[,i]))/
    (max(train[,i] - min(train[,i])))
}

#Dividing Data into training and testing data
train_index = sample(1:nrow(train), 0.8 * nrow(train))
training = train[train_index,]
testing = train[-train_index,]

#MODELLING
#Regression model
model1 = lm(cnt ~., data = training)
summary(model1)
predictions_LR = predict(model1, testing[,1:9])

#MAPE
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))*100
}

MAPE(testing[,10], predictions_LR)
#Error Rate=27.86


#Support Vector Regression(eps-regression)
fitepslinear<-svm(cnt~., data=training,type="eps-regression",kernel="linear",cross=573)
fitepspoly<-svm(cnt~., data=training,type="eps-regression",kernel="polynomial",cross=573)
fitepssigm<-svm(cnt~., data=training,type="eps-regression",kernel="sigmoid",cross=573)
fitepsrad<-svm(cnt~., data=training,type="eps-regression",kernel="radial",cross=573)

summary(fitepslinear)
# Total Mean Squared Error: 1820838 
#Squared Correlation Coefficient: 0.5217222

summary(fitepspoly)
# Total Mean Squared Error: 2711760 
#Squared Correlation Coefficient: 0.4814777 

summary(fitepssigm)
# Total Mean Squared Error: 1814138 
#Squared Correlation Coefficient: 0.5282726 

summary(fitepsrad)
#Total Mean Squared Error: 1607429 
#Squared Correlation Coefficient: 0.5752761

#Choosing the best algorithm to predict the results from test data
predepsrad<-predict(fitepsrad,testing[,1:9])

MAPE(testing[,10], predepsrad)
#Error Rate 24.81

#Support Vector Regression (nu-regression)
fitnulinear<-svm(cnt~., data=training,type="nu-regression",kernel="linear",cross=573)
fitnupoly<-svm(cnt~., data=training,type="nu-regression",kernel="polynomial",cross=573)
fitnusigm<-svm(cnt~., data=training,type="nu-regression",kernel="sigmoid",cross=573)
fitnurad<-svm(cnt~., data=training,type="nu-regression",kernel="radial",cross=573)

summary(fitnulinear)
# Total Mean Squared Error: 1667930 
#Squared Correlation Coefficient: 0.5523692 

summary(fitnupoly)
# Total Mean Squared Error: 2776408 
#Squared Correlation Coefficient: 0.4674513

summary(fitnusigm)
# Total Mean Squared Error: 1779773 
#Squared Correlation Coefficient: 0.5248146

summary(fitnurad)
# Total Mean Squared Error: 1467320 
#Squared Correlation Coefficient: 0.6093441

#Choosing the best algorithm to predict the results from test data
prednurad<-predict(fitnurad,testing[,1:9])

MAPE(testing[,10], prednurad)
#Error Rate 26.72

#BoundCostrain SVM


fitbsvrlinear<-ksvm(cnt~., data=training,type="eps-bsvr",kernel="vanilladot",cross=573)
fitbsvrpoly<-ksvm(cnt~., data=training,type="eps-bsvr",kernel="polydot",cross=573)
fitbsvrrad<-ksvm(cnt~., data=training,type="eps-bsvr",kernel="rbfdot",cross=573)
fitbsvrhyptan<-ksvm(cnt~., data=training,type="eps-bsvr",kernel="tanhdot",cross=573)
fitbsvrlapl<-ksvm(cnt~., data=training,type="eps-bsvr",kernel="laplacedot",cross=573)
fitbsvrbessl<-ksvm(cnt~., data=training,type="eps-bsvr",kernel="besseldot",cross=573)
fitbsvranova<-ksvm(cnt~., data=training,type="eps-bsvr",kernel="anovadot",cross=573)

fitbsvrlinear
# Training error : 0.526382 
#Cross validation error : 2194991 

fitbsvrpoly
# Training error : 0.433751 
#Cross validation error : 1821507 

fitbsvrrad
# Training error : 0.262491 
#Cross validation error : 1575051

fitbsvrhyptan
#Training error : 3907.613559 
#Cross validation error : 14253844725 

fitbsvrlapl
# Training error : 0.263272 
#Cross validation error : 15869199 

fitbsvrbessl
# Training error : 0.266261 
#Cross validation error : 1596046

fitbsvranova
# Training error : 259293.976123 
#Cross validation error : 352047518889 

##Bessel,laplace and radial kernels are giving least errors .Hence, we'll continue predicting for these three models

predsvrrad<-predict(fitbsvrrad,testing[,1:9])
predsvrlapl<-predict(fitbsvrlapl,testing[,1:9])
predsvrbessl<-predict(fitbsvrbessl,testing[,1:9])

MAPE(testing[,10], predsvrrad)
#Error Rate=25.12

MAPE(testing[,10], predsvrlapl)
#Error Rate=25.02

MAPE(testing[,10], predsvrbessl)
#Error Rate=25.97




##KNN Implementation
for( j in 1:15){
  KNN_model = knnreg(training[,1:9],training$cnt, k = j)
  KNN_Predictions= predict(KNN_model,testing[,1:9])
  print(MAPE(testing[,10],KNN_Predictions))
  }
  
##In different algorithms used SVM, eps-regression with radial kernel is giving us least error.
#So, we'll further tune it to decrease the error 

#TUNNING PARAMETERS OF BEST MODEL

tune1=tune(svm,cnt~. , data=training, type="eps-regression",kernel="radial",ranges = list(cost=c(1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9)))
summary(tune1)
#BestTune cost=1.6
tune2=tune(svm,cnt~. , data=training, type="eps-regression",kernel="radial",ranges = list(gamma=c(0.001,0.01,0.1,0.2,0.3,0.4,0.5)))
summary(tune2)
#best tune gamma=0.1(default value)
tune5=tune(svm,cnt~. , data=training, type="eps-regression",kernel="radial",ranges = list(epsilon=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)))
summary(tune5)
#best tune epsilon=0.6

fitepsradtune<-svm(cnt~., data=training,type="eps-regression",kernel="radial",cost=1.6,gamma=0.1,epsilon=0.6,cross=573)
summary(fitepsradtune)
# Total Mean Squared Error: 1372250 
#Squared Correlation Coefficient: 0.6370695
predepsradtune<-predict(fitepsradtune,testing[,1:9])
MAPE(testing[,10],predepsradtune)
#26.86

# fitepsrad<-svm(cnt~., data=training,type="eps-regression",kernel="radial",cost=1.6,cross=573)
# summary(fitepsrad)
# # Total Mean Squared Error: 1603492 
# # Squared Correlation Coefficient: 0.5743909
# 
# fitepsrad<-svm(cnt~., data=training,type="eps-regression",kernel="radial",epsilon=0.6,cross=573)
# summary(fitepsrad)
# # Total Mean Squared Error: 1445166 
# # Squared Correlation Coefficient: 0.6170122

#Best tuned model is also giving more error than our initial minimum,so we'll fix, eps-regression with radial kernel
#and default parameters as our best model for now with 24.81 error rate.

#Taking Sample predictions from our sample output
test_sample=read.csv("SampleData.csv",header=TRUE,sep = ",")
str(test_sample)

col<- c('season','mnth','holiday','weekday','workingday','weathersit')
test_sample[,col] <- lapply(test_sample[,col], factor)

predepsample<-predict(fitepsrad,test_sample[1:9])
print(predepsample)
