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

plot(cerealDf$DiscountA, data=cerealDf,
     col=ifelse(cerealDf$Brand.Preference == "A", "limegreen", "red"))
# NOTE: **bad** distinction by discount A alone

plot(cerealDf$DiscountB, data=cerealDf,
     col=ifelse(cerealDf$Brand.Preference == "A", "limegreen", "red"))
# NOTE: good distinction by discount B alone

plot(cerealDf$DiscountA ~ cerealDf$DiscountB, data=cerealDf,
     col=ifelse(cerealDf$Brand.Preference == "A", "limegreen", "red"))
# NOTE: good distinction, by discount A vs discount B

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

cerealDf$normLoyality <- as.numeric(cerealDf$LoyaltyA / (cerealDf$LoyaltyA + cerealDf$LoyaltyB))
cerealDf$isPreferredA <- as.numeric(ifelse(cerealDf$Brand.Preference == "A", 1, 0))

plot(cerealDf$normLoyality, 
     col=ifelse(cerealDf$isPreferredA, "limegreen", "red"))
# NOTE: good 


cerealDf$fullPriceA <- cerealDf$PriceA + cerealDf$DiscountA
cerealDf$fullPriceB <- cerealDf$PriceB + cerealDf$DiscountB
cerealDf$fullPriceAratio <- cerealDf$fullPriceA / cerealDf$fullPriceB
plot(cerealDf$fullPriceA ~ cerealDf$fullPriceB, 
     col=ifelse(cerealDf$isPreferredA, "limegreen", "red"))
plot(cerealDf$fullPriceAratio, 
     col=ifelse(cerealDf$isPreferredA, "limegreen", "red"))
# NOTE: bad


cerealDf$discountRatioA <- cerealDf$DiscountA / cerealDf$PriceA
cerealDf$discountRatioB <- cerealDf$DiscountB / cerealDf$PriceB
plot(cerealDf$discountRatioA ~ cerealDf$discountRatioB, 
     col=ifelse(cerealDf$isPreferredA, "limegreen", "red"))
# NOTE: pretty good

View(cerealDf)


########################################
# 3. Model
########################################

reg1 <- lm(cerealDf$isPreferredA ~ cerealDf$DisplayA + cerealDf$DisplayB + cerealDf$normLoyality, data=cerealDf)
summary(reg1)


reg2 <- lm(cerealDf$isPreferredA ~ cerealDf$DisplayA + cerealDf$DisplayB + cerealDf$normLoyality+ 
            cerealDf$PriceA  + cerealDf$PriceB + cerealDf$DiscountA + cerealDf$DiscountB
          , data=cerealDf)
summary(reg2)


reg3 <- lm(cerealDf$isPreferredA ~ cerealDf$PriceA  + cerealDf$PriceB + cerealDf$DiscountA + cerealDf$DiscountB, 
           data=cerealDf)
summary(reg3)



reg4 <- lm(cerealDf$isPreferredA ~ cerealDf$DisplayA + cerealDf$DisplayB + cerealDf$normLoyality+ 
             cerealDf$discountRatioA + cerealDf$discountRatioB
           , data=cerealDf)
summary(reg4)
