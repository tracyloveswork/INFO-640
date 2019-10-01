# Descriptive Data Analysis
# Urban Park Ranger Animal Condition Response from Open Data

library(tidyverse)
library(dplyr)
library(lubridate)
library(ggthemes)

# Specified that the first row is the header, and check the formatting of names
rangerRescues <- read.csv("../DataSets/Urban_Park_Ranger_Animal_Condition_Response.csv", header=TRUE, check.names = TRUE)

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
# The larger parks see the most wildlife requests. Manhattan's Central Park and Brooklyn's Prospect Park get the majority of the calls.
# Most calls are in Manhattan, are made by conservancies (animal interest groups), in parks, mostly Central Park, and are about small mammals, particulary racoons.
# Rarely are the police involved. The longest time spent on a response was 21 hours but most calls seem to take around an hour.
# The majority of calls are for Small Mammals on the Rabies Vector Scale and Raccoons make up nearly all of these requests.
summary(rangerRescues)

# Check beginning and end
head(rangerRescues)
# Starts with a hawk, goose, parrot, chicken and a Red-Eared Slider, which happens to be invasive reptile
tail(rangerRescues)
# Ends with a hawk and a group of racoons.

sum(is.na(rangerRescues))
# There are 870 missing values in the dataset. Why? Some fields not always applicable.

# Create new dataset to manipulate
rangerRescues1 <- rangerRescues[,]
summary(rangerRescues1)

# Convert to characters
rangerRescues1$Date.and.Time.of.initial.call <- as.character(rangerRescues1$Date.and.Time.of.initial.call)
rangerRescues1$Date.and.time.of.Ranger.response <- as.character(rangerRescues1$Date.and.time.of.Ranger.response)
rangerRescues1$Species.Description <- as.character(rangerRescues1$Species.Description)

# Fix inconsistencies in species case
rangerRescues1$Species.Description <- tolower(rangerRescues1$Species.Description)
# Check it
table(rangerRescues1$Species.Description)
head(subset(rangerRescues1, select = 'Species.Description'))
# rd-tailed hawk, red tailed hawk, red tail, red-tailed hawk (correct), red tailed-hawk
# racoon, raccoon (correct)
# deer, white tailed deer, white-tailed deer (correct)
# unknown (correct), unknown species,n/a
# rabbit (correct), pet rabbit
# Wildcard selection in R?!!
?grep
rangerRescues1$Species.Description[rangerRescues1$Species.Description=="red tailed hawk"] <- "red-tailed hawk"
rangerRescues1$Species.Description[rangerRescues1$Species.Description=="racoon"] <- "raccoon"
rangerRescues1$Species.Description[rangerRescues1$Species.Description=="deer"] <- "white-tailed deer"
rangerRescues1$Species.Description[rangerRescues1$Species.Description=="unknown species"] <- "unknown"
rangerRescues1$Species.Description[rangerRescues1$Species.Description=="pet rabbit"] <- "rabbit"
rangerRescues1$Species.Description[rangerRescues1$Species.Description=="turtle/ unspecified species"] <- "turtle"

# Animal.Class
table(rangerRescues1$Animal.Class)

# Find typical response time - need to convert data
# Format Dates | Test one value
x <- "06/10/2019 01:00:00 PM"
parse_date_time(x, '%m/%d/%Y %I:%M:%S %p', tz = "EST")
x <- parse_date_time(x, '%m/%d/%Y %I:%M:%S %p', tz = "EST")
x

# Convert to Dates 
rangerRescues1$Date.and.Time.of.initial.call <- parse_date_time(rangerRescues1$Date.and.Time.of.initial.call, '%m/%d/%Y %I:%M:%S %p', tz = "EST")
rangerRescues1$Date.and.time.of.Ranger.response <- parse_date_time(rangerRescues1$Date.and.time.of.Ranger.response, '%m/%d/%Y %I:%M:%S %p', tz = "EST")
head(rangerRescues1)

# Create new dataset with times and a new column with response time (borough)
?dplyr
rescuesResponse <- select(rangerRescues1, Date.and.Time.of.initial.call, Date.and.time.of.Ranger.response, Borough)
head(rescuesResponse)

# create by Borough dataframe
rescuesResponseTime_byBorough <- rescuesResponse %>%
  group_by (Borough) %>%
  summarize(AverageResponseTimeHours =(mean(Date.and.time.of.Ranger.response - Date.and.Time.of.initial.call)/3600) )

# create plot chart
ggplot(rescuesResponseTime_byBorough, aes(x = Borough, y = AverageResponseTimeHours)) +
  geom_point() +
  labs(title = "Average Response Time by Borough") +
  theme_tufte()

# Create set just for Manhattan
rescuesManhattan <- rangerRescues1 %>% filter(Borough=="Manhattan")
head(rescuesManhattan, 10)

# Create set just for Raccoons in Prospect Park, Brooklyn
rescuesProspectRaccoons <- rangerRescues1 %>% filter(Borough=="Brooklyn", Property=="Prospect Park", Species.Description=="raccoon")
summary(rescuesProspectRaccoons)
rescuesProspectRaccoons <- select(rescuesProspectRaccoons, -Borough, -Property, -Species.Description, -Species.Status, -Animal.Class)
head(rescuesProspectRaccoons)

# Change empty Animal.Condition fields to N/A
rescuesProspectRaccoons$Animal.Condition[rescuesProspectRaccoons$Animal.Condition==""] <- "N/A"
write.csv(rescuesProspectRaccoons, "../DataSets/rescuesProspectRaccoons.csv")

# Arrange by Condition
rescuesProspectRaccoons %>% arrange(Animal.Condition)

# View duration of responses
table(rescuesProspectRaccoons$Animal.Condition, rescuesProspectRaccoons$Duration.of.Response)
# Raccoons that are unhealthy usually take 1-2 hours, most unhealthy calls are 2 hours

# Average duration of response by condition
rescuesProspectRaccoonsDuration <- rescuesProspectRaccoons %>%
  group_by(Animal.Condition) %>%
  summarize(AverageTime = mean(Duration.of.Response), MaxTime = max(Duration.of.Response), MinTime = min(Duration.of.Response))
rescuesProspectRaccoonsDuration

# Plot average duration (min, max) of response by condition
ggplot(rescuesProspectRaccoonsDuration, aes(x = Animal.Condition, y = AverageTime)) +
  geom_point(shape=20, size=5, fill="#00000080")+
  geom_point(data=rescuesProspectRaccoonsDuration, aes(y=MaxTime),shape=21, size=3, fill="#FF0000FF") +
  geom_point(data=rescuesProspectRaccoonsDuration, aes(y=MinTime),shape=21, size=3, fill="#00FF00FF") 
  #labs(title = "Prospect Park Racoons Response Average Duration with Max and Min") 
 # Outlier for 21 hours for an unhealthy racoon.


# Frequency by animal - find most common and outliers
rescuesAnimal <- select(rangerRescues1, "Species.Description", "Species.Status")
head(rescuesAnimal)

# Frequency by age of animal by Borough
table(rangerRescues1$Age, rangerRescues1$Borough)

# Frequency by borough and final action
table(rangerRescues$Final.Ranger.Action, rangerRescues1$Borough)
rescuesBoroughs <- as.data.frame (table(rangerRescues$Final.Ranger.Action, rangerRescues1$Borough))
glimpse(rescuesBoroughs)

# Frequency by Animal Condition, Rehabilitation
table(rangerRescues1$Animal.Condition) # The largest reported group was unhealthy. 106 
table(rangerRescues1$Rehabilitator) #The overwhelming majority do not go to a rehabilitation center

# False Alarms
falseAlarms <- rangerRescues1 %>% filter(Final.Ranger.Action=="Unfounded")
glimpse(falseAlarms)
# Average time of False Alarm Call by Borough
falseAlarms %>%
  group_by(Borough) %>%
  summarize(AverageTime = mean(Duration.of.Response))

# create by Borough dataframe
falseAlarms_byBorough <- falseAlarms %>%
  group_by (Borough) %>%
  summarize(AverageTime = mean(Duration.of.Response), TotalTime=sum(Duration.of.Response))

# create plot chart
ggplot(falseAlarms_byBorough, aes(x = Borough, y = AverageTime)) +
  geom_point() +
  labs(title = "Average Time Spent on False Alarms by Borough") +
  theme_tufte()
# Brooklyn unfounded calls take the longest on average

ggplot(falseAlarms_byBorough, aes(x = Borough, y = TotalTime)) +
  geom_point() +
  #geom_hline(data=falseAlarms_byBorough, aes(yintercept=AverageTime)) #not worth looking at together
  labs(title = "Total Time Spent on False Alarms by Borough") +
  theme_tufte()
# Manhattan spends the most total time on unfounded calls and this makes sense because it has the highest call volume

