install.packages("lubridate")
install.packages("dplyr")

library(lubridate)
library(dplyr)
library(tidyverse)

meals <- read.csv("DataSets/mealplan.csv", header = TRUE)

class(meals)
dim(meals)
names(meals)
str(meals)

#not standard with R
glimpse(meals)
