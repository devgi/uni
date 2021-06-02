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
mydf$DisplayA= factor(mydf$DisplayA)
mydf$DisplayB= factor(mydf$DisplayB)
mydf$Brand.Preference=   factor(mydf$Brand.Preference)

# adding new column for how much the discount was meaningful 
mydf$DiscountAPercent=as.numeric(format(round(mydf$DiscountA/(mydf$DiscountA+mydf$PriceA)*100, 3)))
mydf$DiscountBPercent=as.numeric(format(round(mydf$DiscountB/(mydf$DiscountB+mydf$PriceB)*100, 3)))


## relation between DiscountAPercent and Brand.Preference
# we want to understand if theres a connection between the discount and the preference
plot(DiscountAPercent ~ Brand.Preference , data = mydf)

plot(DiscountBPercent ~ Brand.Preference , data = mydf)
#conclusion - when the discount for B is higher- customer buy more B. 

mydf$DiscountABRel=as.numeric(format(mydf$DiscountAPercent/mydf$DiscountBPercent))
plot(DiscountABRel ~ Brand.Preference , data = mydf)

# The prices are quite similar so does the preference 
plot(PriceA/PriceB ~ Brand.Preference, data = mydf)

# The more the discount of A is larger than B, then Customers prefer A over B
plot(DiscountA/DiscountB ~ Brand.Preference, data = mydf)
# And vice versa
plot(DiscountB/DiscountA ~ Brand.Preference, data = mydf)

# How is the sales shelf display affect preference?
plot(mydf$DisplayA , mydf$Brand.Preference )


plot(DisplayB ~ Brand.Preference, data = mydf)


# The price doesn`t affect much of the display in sales shelf
plot(PriceA ~ DisplayA, data = mydf)

# The price doesn`t affect much of the display in sales shelf
plot(PriceB ~ DisplayB, data = mydf)

# The discount percentage of A doesnt affect the displayA
plot(DiscountAPercent ~ DisplayA, data = mydf)

# The discount percentage of B a little bit affect the display B
plot(DiscountBPercent ~ DisplayB, data = mydf)


# The price doesn`t affect much of the display in sales shelf
plot(DisplayB ~ DisplayA, data = mydf)

# Loyalty affects preferences.. no shit
plot(LoyaltyA ~ Brand.Preference, data = mydf)
plot(LoyaltyB ~ Brand.Preference, data = mydf)


mydf$Brand.Preference<- ifelse(mydf$Brand.Preference == "A" , 1,0)
plot(DisplayA ~ Brand.Preference, data = mydf)

