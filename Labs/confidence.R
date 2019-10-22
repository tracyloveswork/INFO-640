# Confidence
# CI (confidence interval)

install.packages("gmodels")

library(gmodels) #gmodels has the ci function to calculate confidence interval of a normally 
library(tidyverse)

bodytemp <- rnorm(10000, mean=97.82, sd=.69)
glimpse(bodytemp)
#num [1:10000] 98 97.7 98.9 97.7 97.4 ...
hist(bodytemp)

set.seed(1234)

bodysample <- sample(bodytemp, 10)
mean(bodysample)
# [1] 98.00471

bodysample <- sample(bodytemp, 100)
mean(bodysample)
# [1] 97.78966

bodysample <- sample(bodytemp, 1000)
mean(bodysample)
# [1] 97.8522

our_sample <- numeric(10000)
for(i in 1:10000) {a_sample <- sample(bodytemp, 50) our_sample[i] <- mean(a_sample)}

hist(our_sample, breaks = 50)


