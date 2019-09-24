install.packages("gapminder")

library(tidyverse)
library(dplyr)
library(gapminder) #the dataset

?gapminder
# Excerpt of the Gapminder data on life expectancy, GDP per capita, and population by country.

summary(gapminder)
# Gives the range of years of data 1952-2007 and median life-expectency is 60.71 years

glimpse(gapminder)
# 6 variables and 1,704 observations

head(gapminder)
# top of dataset
tail(gapminder)
# bottom of dataset

sum(is.na(gapminder))
# the number of missing values and there aren't any

# create a new dataset that just has country and life expectency
gp_cnt_life <- select(gapminder, country, lifeExp)
head(gp_cnt_life)

# create a new dataset without the population variable
gp_no_pop <- select(gapminder, -pop)
head(gp_no_pop)

# create a dataset and filter for a specific year or country
gp_1957 <- gapminder %>% filter(year == 1957)
glimpse(gp_1957)
head(gp_1957, n=10)

#filter for the United States
gp_us <- gapminder %>% filter(country == "United States")
head(gp_us, n=15)

# filter for two variables, Asia and 1957
gp_1957_Asia <- gapminder %>% filter(year==1957, continent=="Asia")
head(gp_1957_Asia, 8)

# save into a new dataset
write.csv(gp_1957_Asia, 'gapminder1957Asia.csv')

# Reorder by population
gapminder %>% arrange(pop)
head(gapminder)
gapminder %>% arrange(desc(pop))
head(gapminder)

# look at just 1957 and arrange by gdp
gapminder %>%
  filter(year == 1957) %>%
  arrange(desc(gdpPercap))

# see population in millions by changing the value of the pop variable
gapminder %>% mutate(pop = pop/1000000)

# mutate can also be used to add a variable, gdp is created by multiplying two current variables
gapminder %>% mutate(gdp = gdpPercap* pop)

# We created a new variable, gdp, and then listed them in descending order within 1957 but nothing was saved outside of the function
gapminder %>%
  mutate(gdp = gdpPercap * pop) %>%
  filter(year == 1957) %>%
  arrange(desc(gdp))
# Check
head(gapminder)

# To save the dataframe we need to assign it a new variable, gap_gdp_1957 
gap_gdp_1957 <- gapminder %>%
  mutate(gdp = gdpPercap * pop) %>%
  filter(year == 1957) %>%
  arrange(desc(gdp))

# Life expectency median since 1957
gapminder %>%
  summarize(meanLifeExp = mean(lifeExp))

# 1957 only
gapminder %>%
  filter(year==1957) %>%
  summarize(meanLifeExp = mean(lifeExp))

# 1957 and total population
gapminder %>%
  filter(year==1957) %>%
  summarize(meanLifeExp = mean(lifeExp),
            totalPop = sum(as.numeric(pop)))

# instead of filtering by year, give results for each year using group_by()
gapminder %>%
  group_by(year) %>%
  summarize(meanLifeExp = mean(lifeExp),
            totalPop = sum(as.numeric(pop)))

# group by continent as well
gapminder %>%
  filter(year == 1957) %>%
  group_by(continent) %>%
  summarize(meanLifeExp = mean(lifeExp),
            totalPop = sum(as.numeric(pop)))


# Find the median life expectancy and gdp of United States (or another country) in the year 2000
gapminder %>%
  filter(year == 2000, country == "United States")
# there are no observations for 2000, so switched to 2002
gapminder %>%
  filter(year == 2002, country == "United States") %>%
  mutate(gdp = gdpPercap* pop) %>%
  summarize(meanLifeExp = (lifeExp),
            gdp = gdp)

# Find the mean gdp for every year
gapminder %>%
  group_by(year) %>%
  mutate(gdp = gdpPercap* pop) %>%
  summarize(meanGdp = mean(gdp))

# --- VISUALIZATIONS ---

# create by_year dataframe
by_year <- gapminder %>%
  group_by(year) %>%
  summarize(totalPop = sum(as.numeric(pop)),
            meanLifeExp = mean(lifeExp))

# create plot chart
ggplot(by_year, aes(x = year, y = totalPop)) +
  geom_point()

# start y at 0
ggplot(by_year, aes(x = year, y = totalPop)) +
  geom_point() +
  expand_limits(y=0)

# broken down by continent
by_year_continent <- gapminder %>%
  group_by(year, continent) %>%
  summarize(totalPop = sum(as.numeric(pop)),
            meanLifeExp = mean(lifeExp))
by_year_continent
# chart it 
ggplot(by_year_continent, aes(x = year, y = totalPop, color = continent)) +
  geom_point() +
  expand_limits(y = 0)

head(by_year_continent)

# Visualize the mean gdpPerCap of each continent as a function of time
by_year_continent_gdpPerCap <- gapminder %>%
  group_by(year, continent) %>%
  summarize(gdpPerCap = mean(gdpPercap))

by_year_continent_gdpPerCap

# chart it 
ggplot(by_year_continent_gdpPerCap, aes(x = year, y = gdpPerCap, color = continent)) +
  geom_point() +
  expand_limits(y = 0)


# --- Our Descriptive Analysis ---

#1 [gives us 12 observations for each country, the earliest was 1952 adn the latest 2007, the lowest life expectency was 23.6 and hightest was 82.6]
summary(gapminder)
  
#2 create the dataframe from 2007 which is the last observation
gap2007 <- gapminder %>% filter(year=="2007")
  
gap2007[which.min(gap2007$lifeExp),]
# Swaziland had the lowest life expectency
gap2007[which.min(gap2007$pop),]
# Sao Tome and Principe had the lowest population
gap2007[which.min(gap2007$gdpPercap),]
# Congo, Dem. Rep. had the lowest gdp per capita
gap2007[which.max(gap2007$lifeExp),]
# Japan had the highest life expectency
gap2007[which.max(gap2007$pop),]
# China had the highest population
gap2007[which.max(gap2007$gdpPercap),]
# Norway had the highest gross domestic product per capita
  

#3
# create a variable for start year from the lowest value for year in gapminder dataset
start_year <- min(gapminder['year'])
# create a variable for end year from the highest value for year in gapminder dataset
end_year <- max(gapminder['year'])
# create a variable for lowest population value in gapminder dataset
start_pop <- min(gapminder['pop'])
# create a variable for highest population value in gapminder dataset
end_pop <- max(gapminder['pop'])
# create a variable for growth rate
pop_growth_rate <- (end_pop-start_pop)/(end_year-start_year)
# How much did the population increase between 1952 and 2007?
pop_growth_rate


#4 Create a dataframe with the avg life expectency, gdpPerCap and the total population broken down by year and continent
gap_grouped <- gapminder %>%
  group_by(continent, year) %>%
  summarize(meanLifeExp = mean(lifeExp),
            totalPop = sum(as.numeric(pop)),
            meanGdpPercap = mean(gdpPercap))
summary(gap_grouped)


#4  Visualize
# growth of life expectency of each continent by year
ggplot(gap_grouped, aes(x = year, y = meanLifeExp, color=continent)) +
  geom_line()
# growth of total population of each continent by year
ggplot(gap_grouped, aes(x = year, y = totalPop, color=continent)) +
  geom_line()
# growth of gdp per capita of each continent by year
ggplot(gap_grouped, aes(x = year, y = meanGdpPercap, color=continent)) +
  geom_line()

# First you can filter each continent, make a new dataframe for each continent, and summarize each. Good with only five.
gp_Asia <- gapminder %>% 
  filter(continent=="Asia") %>%
  summarize(maxYear = max(year),
          minYear = min(year),
          maxLifeExp = max(lifeExp),
          minLifeExp = min(lifeExp),
          meanLifeExp = mean(lifeExp),
          maxPop = max(pop),
          minPop = min(pop),
          meanPop = mean(pop),
          maxgdpPercap = max(gdpPercap),
          mingdpPercap = min(gdpPercap),
          meangdpPercap = mean(gdpPercap))
gp_Asia
# And then do the other four...

# Second, you can group by continent and specify the metrics you want. Good if you only have a few metrics.
gapminder %>%
  group_by(continent) %>%
  summarize(maxYear = max(year),
            minYear = min(year),
            maxLifeExp = max(lifeExp),
            minLifeExp = min(lifeExp),
            meanLifeExp = mean(lifeExp),
            maxPop = max(pop),
            minPop = min(pop),
            meanPop = mean(pop),
            maxgdpPercap = max(gdpPercap),
            mingdpPercap = min(gdpPercap),
            meangdpPercap = mean(gdpPercap))
#repeat for the remaining measures

# Finally, you can use what’s called lambda calculus.
# First, we’ll make a list (a 1 dimensional vector)
my_continents <- c("Africa", "Americas", "Asia", "Europe", "Oceania")
# then specify that we are using a function with each item in that list.
sapply(my_continents, 
       # Then define the function.
       function(cont){gapminder %>% 
           filter(continent==cont) %>% 
           summary()})
summary(gapminder)

# From 1952 through 20017, we have seen the greatest growth in population in Asia. 
# There has been a steady rise in population in the Americas and Africa wheras Oceania has remained flat.
# GDP growth has been the highest in Europe and Oceania in this same time period. Norway had the highest GDP per capita in 2007.
# GDP per capita has steadly risen in the Americas and Asia but stayed relatively level for African countries during this time.
# Median life expectency has grown overall globally. Africa has consistently had the lowest values with one record at just 23.6 years.
