setwd("C:/Users/galyech/OneDrive - Microsoft/Personal/MBA/R") # change this directory to your working directory

#Installing packages

#install.packages('party')
#install.packages('forecast')
#install.packages('caret') # this is the relevant ml package
#install.packages('e1071', dependencies=TRUE) 
library(caret)
library('e1071')
library(forecast)
library(party)

# READ DB
mydf <-  read.csv("Cereal.csv") 

# preprocess , thoughts and stuff:
# DisplayA and DisplayB should be categorials / factor because they are boolean
# maybe we want to know how much the discount percentage is meaningful for the customer. 
# ie: PriceA+DiscountA is the real price. DiscountA/(PriceA+DiscountA) for example is 10% but for B is 20% 
# so the customer prefers B.

# factoring DisplayA and DisplayB
mydf$DisplayA = factor(mydf$DisplayA)
mydf$DisplayB = factor(mydf$DisplayB)
mydf$Brand.Preference = factor(mydf$Brand.Preference)

# Adding new column for how much the discount percentage
mydf$DiscountAPercent=as.numeric(format(round(mydf$DiscountA/(mydf$DiscountA+mydf$PriceA)*100, 3)))
mydf$DiscountBPercent=as.numeric(format(round(mydf$DiscountB/(mydf$DiscountB+mydf$PriceB)*100, 3)))

# Checking relation between DiscountAPercent and Brand.Preference
# we want to understand if theres a connection between the discount and the preference
plot(DiscountAPercent ~ Brand.Preference , data = mydf)
plot(DiscountBPercent ~ Brand.Preference , data = mydf)
discountBMeanPerc <- mean(mydf$DiscountBPercent)
discountAMeanPerc <- mean(mydf$DiscountAPercent)
discountBMean <- mean(mydf$DiscountB)
discountAMean <- mean(mydf$DiscountA)
# conclusion - the discounts are quite the same
# conclusion - when the discount percentage for B is high - customers buy more B. 
# conclusion - the discount percentage of A has no affect on customers pref

# Checking the discount for A relatively to B.
plot(DiscountA/DiscountB ~ Brand.Preference, data = mydf)
plot(DiscountB/DiscountA ~ Brand.Preference, data = mydf)
# conclusion - when the discount for A is higher than B, customers buy more A, and vice versa, people looooove higher discounts

# The prices are quite similar so does the preference 
plot(PriceA/PriceB ~ Brand.Preference, data = mydf)
plot(PriceA ~ Brand.Preference, data = mydf)
plot(PriceB ~ Brand.Preference, data = mydf)
# conclusion - the final price doesnt affect the pref

# Checking the affect that the price has on the display on the sales shelf
plot(PriceA ~ DisplayA, data = mydf)
plot(PriceB ~ DisplayB, data = mydf)
# conclusion - The final price doesn`t affect much of the display in sales shelf

# checking how the discount percentage of A affects the display on the sales shelf
plot(DiscountAPercent ~ DisplayA, data = mydf)
plot(DiscountBPercent ~ DisplayB, data = mydf)
# conclusion - The discount percentage of B a little bit affect the display on the sales shelf, but for A it's not a thing

# Loyalty affects preferences.. walla
plot(LoyaltyA ~ Brand.Preference, data = mydf)
plot(LoyaltyB ~ Brand.Preference, data = mydf)

#How many times only A was on display and they chose A
countAOnDisplayA <- as.numeric(nrow(mydf[mydf$Brand.Preference == "A" & mydf$DisplayA == 1 & mydf$DisplayB == 0,]))
#How many times only B was on display and they chose A
countBOnDisplayA <- as.numeric(nrow(mydf[mydf$Brand.Preference == "A" & mydf$DisplayB == 1 & mydf$DisplayA == 0,]))
#How many times both were on display and they chose A 
countBothOnDisplayA <- as.numeric(nrow(mydf[mydf$Brand.Preference == "A" & mydf$DisplayA == 1 & mydf$DisplayB == 1,]))
#How many times none were on display and they chose A 
countNoneOnDisplayA <- as.numeric(nrow(mydf[mydf$Brand.Preference == "A" & mydf$DisplayA == 0 & mydf$DisplayB == 0,]))

#How many times only A was on display and they chose B 
countAOnDisplayB <- as.numeric(nrow(mydf[mydf$Brand.Preference == "B" & mydf$DisplayA == 1 & mydf$DisplayB == 0,]))
#How many times only B was on display and they chose B 
countBOnDisplayB <- as.numeric(nrow(mydf[mydf$Brand.Preference == "B" & mydf$DisplayB == 1 & mydf$DisplayA == 0,]))
#How many times both were on display and they chose B 
countBothOnDisplayB <- as.numeric(nrow(mydf[mydf$Brand.Preference == "B" & mydf$DisplayA == 1 & mydf$DisplayB == 1,]))
#How many times none were on display and they chose B 
countNoneOnDisplayB <- as.numeric(nrow(mydf[mydf$Brand.Preference == "B" & mydf$DisplayA == 0 & mydf$DisplayB == 0,]))

colors = c("blue","red")
whichOnDisplay <-  c("Only A","Only B", "Both", "None") 
preferences <-c("% Choose A","% Choose B")
# Create the matrix of the values.
Values <- matrix(c(countAOnDisplayA/(countAOnDisplayA+countAOnDisplayB)*100,
                   countBOnDisplayA/(countBOnDisplayA+countBOnDisplayB)*100,
                   countBothOnDisplayA/(countBothOnDisplayA+countBothOnDisplayB)*100,
                   countNoneOnDisplayA/(countNoneOnDisplayA+countNoneOnDisplayB)*100,
                   countAOnDisplayB/(countAOnDisplayA+countAOnDisplayB)*100,
                   countBOnDisplayB/(countBOnDisplayA+countBOnDisplayB)*100,
                   countBothOnDisplayB/(countBothOnDisplayA+countBothOnDisplayB)*100,
                   countNoneOnDisplayB/(countNoneOnDisplayA+countNoneOnDisplayB)*100)
                 , nrow = 2, ncol = 4, byrow = TRUE)
# Create the bar chart
barplot(Values, main = "Pref for display", names.arg = whichOnDisplay, xlab = "WhichOnDisplay", ylab = "customer pref", col = colors)
# Add the legend to the chart
legend("topleft", preferences, cex = 1.3, fill = colors)
# conclusion - seems like A is getting the same attention when displaying alone or when nothing is displayed.
# when none is on the sales shelf - A gets bigger attention. 
# B is much more dominant when displaying alone

# All conclusions:
# conclusion - the discounts are quite the same
# conclusion - when the discount percentage for B is high - customers buy more B. 
# conclusion - the discount percentage of A has no affect on customers pref
# conclusion - when the discount for A is higher *than* B, customers buy more A, and vice versa, people looooove higher discounts
# conclusion - the final price doesnt affect the pref
# conclusion - The final price doesn`t affect much of the display in sales shelf
# conclusion - The discount percentage of B a little bit affects the display on the sales shelf, but for A it's not a thing
# conclusion - seems like A is getting the same attention when displaying alone or when nothing is displayed.
# conclusion - when none is on the sales shelf - A gets bigger attention. 


# which models can be relevant here? 
# according to the graphs we saw that the following columns are important:
#   1. Loyalty
#   2. discount percentage
# we can try linear regression as a start, with loyalty, price and discount

#########################  partitioning 

set.seed(1)
partition <- createDataPartition(mydf[['Brand.Preference']], p = 0.7, list=FALSE ) # returns the indexes of the train data set.  
training <- mydf[partition,]
validation <- mydf[-partition,]

######################### Training classification tree  

ctreeFit <- train(Brand.Preference ~ ., 
                  data = training, 
                  method="ctree") 

ctreeFit$finalModel

plot(ctreeFit$finalModel, type = "simple")

# Predict regression tree

ctreePred <- predict(ctreeFit, newdata = validation)
ctreePred

confusionMatrix(ctreePred, validation$Brand.Preference)

###################################### Training KNN 

knnFit <- train(Brand.Preference~., 
                data = training, 
                method="knn", preProcess=c("scale","center"),
                tuneGrid   = expand.grid(k = c(1,5,10))) 


knnFit

# Predict KNN
knnPred <- predict(knnFit, newdata = validation)

confusionMatrix(knnPred, validation$Brand.Preference)


# TODO - play with the numbers of validation percentages, posibble KNN values, add variables 
