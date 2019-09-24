# Descriptive Data Analysis
# Urban Park Ranger Animal Condition Response

library(tidyverse)
library(dplyr)
library(lubridate)

# Specified that the first row is the header, and do not check the formatting of names
rangerRescues <- read.csv("../DataSets/Urban_Park_Ranger_Animal_Condition_Response.csv", header=TRUE, check.names = FALSE)

# Shows that it is being read as a dataframe
class(rangerRescues)
# The dataframe has 982 objects with 22 variables
dim(rangerRescues)
# List all the variable names
names(rangerRescues)
# Overview of dataset shows that date and time are being read as a factor
str(rangerRescues)
# A better overview...
glimpse(rangerRescues)
# Statistical summary 
# Most calls are in Manhattan, are made by conservancies (animal interest groups), in parks, mostly Central Park, and are about small mammals, particulary racoons.
# Rarely are the police involved.
summary(rangerRescues)
# Check beginning and end
head(rangerRescues)
# Starts with a hawk, goose, parrot, chicken and a Red-Eared Slider, which happens to be invasive reptile
tail(rangerRescues)
# Ends with a hawk and a group of racoons.

?lubridate
