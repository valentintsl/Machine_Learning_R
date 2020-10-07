setwd('D:\\Apprentissage avec R\\StatisticsWithR')
getwd()

library(data.table) #Use of data.table
library(ggthemes)
library(corrplot)
library(GGally) # ggally (data viz)
library(usdm)
require(glmnet)
require(caret)
require(ggplot2)

#Create a RSS function with Y features (real data) and Yhat (prediction) that restitue the mean of the residual squared
RSS <- function(y, yhat) sum((yhat-y)^2)/length(y)

#Create a R2 function with Y feature (real data) and Yhat (prediction) giving the R2 of the model
R2 <- function(y, yhat) 1-(sum(yhat-y)^2)/sum((y-mean(y))^2)

#Example of high R2
set.seed(222)
Nlig <- 100
Ncol <- 95
X <- matrix(rnorm(Nlig*Ncol), ncol=Ncol)
Y <- rnorm(Nlig)
summary(lm(Y~X))

#R2 normally ranges between 0 and 1, here the R2 is 0.984 : A very high R2 signify a high % of the variance explained
#R2 is a measure of how well the regression predictions approximate the real data poins. An R2 of 1 indicates that the regression predictions perfectly fit the data

#Acquisition Boston
DT <- fread("data\\boston.csv") #Load file in data.table
valid <- sample(1:dim(DT)[1], 200) #Choose a validation set
Y <- log(DT[valid]$medv) #Log of the validation set target

#Visual exploration
summary(DT)
str(DT)
corrplot(cor(DT)) #Identify the direct correlation
ggpairs(DT) #Dataviz

#Explore the distribution of medv (Median value of owner occupied homes in 1000's $)with ggplot and geom histogram
ggplot(DT,aes(x=medv))+geom_histogram(bins=50, fill="blue")+theme_classic()
#Most of the medv is around 35 000$

#Manipulation of data.tables (review)
DT[nox>0.5] #Filter with nox > 0.5
DT[,.(zn, chas,indus)] #Filter columns
DT[,.(dis_moyenne=mean(dis)),by=.(zn==0)]

#Calcul median age by nox (Nitric oxides concentration) for zn = 0 (Proportion of residential land zoned for lots over 25 000sq.ft)
noxBin = bin(nox, nbins = 10)
DT[,.(median_age=median(age)), by=.(nox)]

#Modelisations

