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
mydfTest$Household <- NULL 

############################################## Remove unwanted rows 

ConditionA <- (mydfTest$DisplayA == mydfTest$DisplayB) & (mydfTest$PriceA <= mydfTest$PriceB) & (mydfTest$DiscountA >= mydfTest$DiscountB) &  (mydfTest$LoyaltyA >= mydfTest$LoyaltyB) & (mydfTest$Brand.Preference == "B")
ConditionB <- (mydfTest$DisplayA == mydfTest$DisplayB) & (mydfTest$PriceB <= mydfTest$PriceA) & (mydfTest$DiscountB >= mydfTest$DiscountA) &  (mydfTest$LoyaltyB >= mydfTest$LoyaltyA) & (mydfTest$Brand.Preference == "A")

mydfTest <- mydfTest[ !(ConditionA == TRUE | ConditionB == TRUE), ]

############################################## Adding new columns

mydfTest$DiscountAPercent <- as.numeric(format(round(mydfTest$DiscountA/(mydfTest$DiscountA+mydfTest$PriceA)*100, 3)))
mydfTest$DiscountBPercent <- as.numeric(format(round(mydfTest$DiscountB/(mydfTest$DiscountB+mydfTest$PriceB)*100, 3)))

mydfTest$DiscountAPercentOverBPercent <- mydfTest$DiscountAPercent/mydfTest$DiscountBPercent
mydfTest$DiscountBPercentOverAPercent <- mydfTest$DiscountBPercent/mydfTest$DiscountAPercent

############################################## partitioning 

set.seed(3003)
partition <- createDataPartition(mydfTest[['Brand.Preference']], p = 0.8, list=FALSE ) # returns the indexes of the train data set.  
training <- mydfTest[partition,]
validation <- mydfTest[-partition,]

############################################## Training KNN 

knnFit <- train(Brand.Preference~., 
                data = training, 
                method="knn", preProcess=c("scale","center"),
                tuneGrid   = expand.grid(k = c(10,20,25))) 

knnFit

knnPred <- predict(knnFit, newdata = validation)

confusionMatrix(knnPred, validation$Brand.Preference)

############################################## Gradient Boosting Machines    

gbm <- train(Brand.Preference~., 
             data = training,
             method="gbm") 

summary(gbm)

adaboostPred <- predict(gbm, newdata = validation)
adaboostPred

confusionMatrix(adaboostPred, validation$Brand.Preference)

######################### Training classification tree  

ctreeFit <- train(Brand.Preference ~ ., 
                  data = training, 
                  method="ctree") 

ctreeFit

plot(ctreeFit$finalModel, type = "simple")

ctreePred <- predict(ctreeFit, newdata = validation)
ctreePred

confusionMatrix(ctreePred, validation$Brand.Preference)

######################### Training classification forest  

ctreeForest <- train(Brand.Preference ~ ., 
                     data = training, 
                     ntree=50,
                     method="rf") 

cforestPred <- predict(ctreeForest, newdata = validation)
cforestPred

confusionMatrix(cforestPred, validation$Brand.Preference)

############################################## Plotting 

plot(PriceA/PriceB ~ Brand.Preference, data = mydfTest)

plot(DiscountAPercentOverBPercent ~ Brand.Preference, data = mydfTest)
plot(DiscountBPercentOverAPercent ~ Brand.Preference, data = mydfTest)

plot(DiscountAPercent ~ Brand.Preference, data = mydfTest)
plot(DiscountBPercent ~ Brand.Preference, data = mydfTest)

plot(mydfTest$DiscountA ~ mydfTest$LoyaltyA )
plot(mydfTest$DiscountB ~ mydfTest$LoyaltyB )

plot(mydfTest$PriceA/mydfTest$PriceB ~ mydfTest$LoyaltyA )
plot(mydfTest$PriceB/mydfTest$PriceA ~ mydfTest$LoyaltyB )

plot( mydfTest$LoyaltyB ~ mydfTest$Brand.Preference )
plot( mydfTest$LoyaltyA ~ mydfTest$Brand.Preference )


