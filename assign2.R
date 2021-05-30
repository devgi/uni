######################################
# 0. Read data
######################################

setwd("c:/workspace/uni")
cerealDf = read.csv("./cereal.csv")


######################################
# 1. Explore & Visualize Raw Data
######################################
dim(cerealDf)
summary(cerealDf)
View(cerealDf)

# In the following visualizations limegreen would represent success - meaning 
# customer preferred product A 

plot(cerealDf$LoyaltyB ~ cerealDf$LoyaltyA, data=cerealDf,
     col=ifelse(cerealDf$Brand.Preference == "A", "limegreen", "red"))
# NOTE: good distinction, by loyalty alone

plot(cerealDf$PriceA ~ cerealDf$PriceB, data=cerealDf,
     col=ifelse(cerealDf$Brand.Preference == "A", "limegreen", "red"))
# NOTE: bad distinction, by price alone

plot(cerealDf$DiscountA ~ cerealDf$DiscountB, data=cerealDf,
     col=ifelse(cerealDf$Brand.Preference == "A", "limegreen", "red"))
# NOTE: good distinction, by discount alone

plot(cerealDf$DisplayA ~ cerealDf$DisplayB, data=cerealDf, 
     col=ifelse(cerealDf$Brand.Preference == "A", "limegreen", "red"))
# NOTE: **very good**  distinction, by display alone 

plot(cerealDf$PriceA ~ cerealDf$DiscountA, data=cerealDf,
     col=ifelse(cerealDf$Brand.Preference == "A", "limegreen", "red"))
# NOTE: bad distinction, by price A with Discount A

plot(cerealDf$PriceB ~ cerealDf$DiscountB, data=cerealDf, 
     col=ifelse(cerealDf$Brand.Preference == "A", "limegreen", "red"))
# NOTE: good distinction, by price B with Discount B



########################################
# 2. Prepare Data
########################################
