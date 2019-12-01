# Machine Learning

install.packages("factoextra")
install.packages("rpart") #to make decision trees
install.packages("rpart.plot") #to visualize decision trees
install.package("rattle")
install.packages(RColorBrewer) #nice colors


library(tidyverse)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(class)
library(factoextra)

# ---- Regression  ----
# The goal of regression is to learn a pattern and then situate a point within the pattern. 
# Regression requires a large amount of pre-labeled data to learn the pattern.

# Linear Regress
data("txhousing")
# Information about the 2000 - 2015 housing market in Texas provided by the TAMU real estate cente.
?txhousing

summary(txhousing)
head(txhousing)

# Create linear model between median sales price based on number of listings
# median sales price by listings
lm_housing <- lm(median ~ listings, data=txhousing)

# if there are 800 houses for sale how much will my house be
# create this dataset

unseen <- data.frame(listings = 800)
predict(lm_housing, unseen)
# 1
# 127009
# I could see my house for 127k if there are 800 listings

summary(lm_housing)
# Look at Multiple R-squared:  0.06007
# This is a terrible model becauase it is nowhere near 1

lm_housing1 <- lm(median ~ sales, data=txhousing)
summary(lm_housing1)

# volume gives Multiple R-squared:  0.1667
# inventory gives Multiple R-squared:  0.02022
# sales gives Multiple R-squared:  0.119

# None are great :\

# Multiple Regression
street <- read_csv("../DataSets/streeteasy.csv")
summary(street)

street_select6 <- street %>% select(rent, bedrooms,bathrooms, size_sqft, min_to_subway,floor,building_age_yrs)
# build model with rent based on all of the other variables
lm_street_select6 <- lm(rent ~ ., data=street_select6)
summary(lm_street_select6)
# Multiple R-squared:  0.7296

# Square Footage Only

lm_footage <- lm(rent ~ size_sqft, data=street_select6)
summary(lm_footage)                                    

street_select16 <- street[,1:17]
#inspect your data
str(street_select16)    

