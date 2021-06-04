setwd("C:/Users/galyech/OneDrive - Microsoft/Personal/MBA/R") # change this directory to your working directory

############################################## Installing packages

#install.packages('party')
#install.packages('forecast')
#install.packages('caret') # this is the relevant ml package
#install.packages('e1071', dependencies=TRUE) 
#install.packages('gbm') 
library(caret)
library('e1071')
library(forecast)
library(party)
library(gbm)

############################################## READ DB

mydfTest <-  read.csv("Cereal.csv") 

############################################## Preprocessing columns 

mydfTest$DisplayA = factor(mydfTest$DisplayA)
mydfTest$DisplayB = factor(mydfTest$DisplayB)
mydfTest$Brand.Preference = factor(mydfTest$Brand.Preference)

############################################## Adding new column 

ChooseA <- as.numeric(mydfTest$Brand.Preference == "A")
ChooseB <- as.numeric(mydfTest$Brand.Preference == "B")

ANotDisplayed <-as.numeric(mydfTest$DisplayA == "0")
BNotDisplayed <- as.numeric(mydfTest$DisplayB == "0")

mydfTest$ChooseANotDisplayedA <- factor(ANotDisplayed * ChooseA)
mydfTest$chooseBNotDisplayedB <- factor(BNotDisplayed * ChooseB)

mydfTest$DiscountBRelToA= mydfTest$DiscountB/mydfTest$DiscountA
mydfTest$DiscountARelToB=mydfTest$DiscountA/mydfTest$DiscountB

DiscountAPercent=as.numeric(format(round(mydfTest$DiscountA/(mydfTest$DiscountA+mydfTest$PriceA)*100, 3)))
DiscountBPercent=as.numeric(format(round(mydfTest$DiscountB/(mydfTest$DiscountB+mydfTest$PriceB)*100, 3)))

mydfTest$DiscountAPercentOverBPercent <-DiscountAPercent/DiscountBPercent
mydfTest$DiscountBPercentOverAPercent <- DiscountBPercent/DiscountAPercent

############################################## Plotting - TODO


############################################## partitioning 

set.seed(1001)
partition <- createDataPartition(mydfTest[['Brand.Preference']], p = 0.7, list=FALSE ) # returns the indexes of the train data set.  
training <- mydfTest[partition,]
validation <- mydfTest[-partition,]

############################################## Training KNN 


knnFit <- train(Brand.Preference~., 
                data = training, 
                method="knn", preProcess=c("scale","center"),
                tuneGrid   = expand.grid(k = c(1,3, 5,10,20))) 


knnFit

# Predict KNN
knnPred <- predict(knnFit, newdata = validation)

confusionMatrix(knnPred, validation$Brand.Preference)


############################################## gbm    

gbm <- train(Brand.Preference~., 
             data = training, 
             method="gbm") 

summary(gbm)

adaboostPred <- predict(gbm, newdata = validation)
adaboostPred

confusionMatrix(adaboostPred, validation$Brand.Preference)


