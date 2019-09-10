library(tidyverse)
library(dplyr)
library(lubridate)

# Skipped first three rows because we aren't use the data, specified that the first row is the header, and do not check the formatting of names
meals <- read.csv("../DataSets/mealplan.csv", skip=3, header=TRUE, check.names = FALSE)
# Check to make sure that it is being read as a dataframe
class(meals)
# Check the size of the dataset
dim(meals)
# What are the variable names?
names(meals)
# Overview of dataset
str(meals)
# A better overview...
glimpse(meals)
# Statistical summary
summary(meals)
# Check beginning and end
head(meals)
tail(meals)

# Data Issues
# All variables were identified as factors
# Meal and Location should be characters
# Entree name and price are as one value
# Dates should be recognized as dates
# The headers have information such the type of meal and ocation

# Create a new dataset using tidyverse gather function to pivot the header information into values 
meals1 <- gather(meals, date, value, -Meal, -Location)
head(meals1)

# Break apart entree name and price into columns
meals1 <- separate(meals1, value, c("entree", "price"), sep=',')
str(meals1)

# Fix the dates
meals1$date <- mdy(meals1$date)
head(meals1)

# Remove the dollar sign
meals1$price <- gsub("\\$","",meals1$price)
head(meals1)

# Change the prices to number values
meals1$price <- as.numeric(meals1$price)

# histogram
hist(meals1$price)

#boxplot
boxplot(meals1$price)

# Check distribution of values
unique(meals1$price)
meals1$price[meals1$price==400] <- 4.00
hist(meals1$price)
summary(meals1)

# Remove whitespace
meals1$Meal <- str_trim(meals1$Meal)
meals1$entree <- str_trim(meals1$entree)
meals1$Location <- str_trim(meals1$Location)

# Remove NA
meals1$entree[meals1$entree==""] <- "NA"

summary(meals1)

# Convert Meals and Location to characters
meals1$Meal <- as.character(meals1$Meal)
meals1$Location <- as.character(meals1$Location)

# Save another version to continue manipulation/standardization
meals2 <- meals1[,]

# to lowercase
names(meals2) <- tolower(names(meals2))
head(meals2)

meals2$entree <- tolower(meals2$entree)

# identify NAs and do not include them in mean
mean(meals2$price, na.rm = TRUE)

# find maximum price
maxprice <- max(meals2$price, na.rm=TRUE)
print(maxprice)

# create a new dataframe with the maxprice replaced missing values
meals2$price_imputed <- meals2$price
meals2$price_imputed[is.na(meals2$price_imputed)]<- maxprice

# add up the price column
sum(meals2$price_imputed)
