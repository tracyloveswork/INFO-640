# Regression & Correlation


install.packages("broom")
install.packages("GGally")

library(tidyverse)
library(lubridate)
library(broom) #to help clean things up and remerge our dataframes
library(GGally) #to help run multiple pair-wise correlations

?broom

?trees

data(trees)
glimpse(trees)

# Viewing relationship between the girth of the tree and it's height there does not seem to be a strong correlation.
ggplot(trees, aes(x=trees$Girth, y=trees$Height)) + geom_point()+
  labs(title="Tree height by girth")

# Add line of best fit
ggplot(trees, aes(x=trees$Girth, y=trees$Height)) + geom_point()+
  stat_smooth(method="lm", se=FALSE)+
  labs(title="Tree height by girth")

lm_trees <- lm(Height ~ Girth, data=trees)
lm_trees

summary(lm_trees)

coef(lm_trees)
#Coefficients:
#  (Intercept)        Girth  
#   62.031            1.054  

# For every inch wider a tree is, it is 1.054 feet taller, assuming a base height of 62.031 feet.

fitted_trees <- fitted.values(lm_trees)
fitted_trees

residual_trees <- residuals(lm_trees)
residual_trees


lm_matrix_trees <- broom::augment(lm_trees)
head(lm_matrix_trees)

lm_matrix_trees %>%
  arrange(desc(.resid)) %>%
  head()

lm_matrix_trees$.resid_abs <- abs(lm_matrix_trees$.resid)
lm_matrix_trees %>%
  arrange(desc(.resid_abs)) %>%
  head()

trees %>%
  filter(Girth == 13.8)

# A black cherry tree with a diameter of 19 inches

head(lm_trees)
new_trees <- data.frame("Girth" = 19)

predict(lm_trees, newdata=new_trees)

mytree <- broom::augment(lm_trees, newdata=new_trees)
mytree
  
ggplot(data = trees, aes(x = Girth, y = Height)) +
  geom_point() + geom_smooth(method = "lm") +
  geom_point(data = mytree, aes(y = .fitted), size = 3, color = "red")+
  labs(title="Tree height by girth")

tree_null <- lm(Height ~ 1, data = trees)

mean_height <- mean(trees$Height)

ggplot(data = trees, aes(x = Girth, y = Height)) +
  geom_point() +
  geom_hline(yintercept=mean_height) +
  labs(title="Tree height null model")

summary(lm_trees)

ggpairs(data = trees, columns=1:3)
