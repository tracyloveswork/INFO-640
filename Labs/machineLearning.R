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
# The goal of regression is to learn a pattern and then situate a point within the pattern... 
# Predict outcomes based on ones that we already know
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
# This is a terrible model becauase it is nowhere near 1, far too narrow

lm_housing1 <- lm(median ~ sales, data=txhousing)
summary(lm_housing1)

# volume gives Multiple R-squared:  0.1667
# inventory gives Multiple R-squared:  0.02022
# sales gives Multiple R-squared:  0.119

# None are great :\ We need more variables to predict price so we need a multiple regression

# Multiple Regression
street <- read_csv("../DataSets/streeteasy.csv")
summary(street)

# create dataset from only values you're interested in to predict rent 
street_select6 <- street %>% select(rent, bedrooms,bathrooms, size_sqft, min_to_subway,floor,building_age_yrs)

# build model with rent based on all of the other variables
lm_street_select6 <- lm(rent ~ ., data=street_select6)
summary(lm_street_select6)
# All are significant and have three astericks
# Multiple R-squared:  0.7296

# Square Footage Only

lm_footage <- lm(rent ~ size_sqft, data=street_select6)
summary(lm_footage)                                    
# Multiple R-squared:  0.6541

street_select16 <- street[,1:17]
#inspect your data
str(street_select16) 
lm_street_select16 <- lm(rent ~ ., data=street_select16)
summary(lm_street_select16)
# Multiple R-squared:  0.7337
# Adjusted R-squared:  0.7329
# The more variables the better the prediction
# Not significant: building_id, no_fee, has_roofdeck, has_doorman, has_dishwasher, has_patio, has_gym

# Decision Trees (Classification)
# rpart needs a seed to produce randomness needed for decision trees
set.seed(1234)
# Load data (https://www.kaggle.com/c/titanic/data)
train <- read.csv("../DataSets/titanic/train.csv")
test <- read.csv("../DataSets/titanic/test.csv")
str(train)
str(test)
# create data sets with only selected variables
train <- train %>% select(Pclass, Sex, Age, Survived)
test <- test %>% select(Pclass, Sex, Age, Survived)

tree <- rpart(Survived ~Pclass + Age + Sex, train, method="class") # Use class because the variables are factors
fancyRpartPlot(tree)
# Consider a female over 39 in 3rd class
# The dominant class is at the top and for all is "0" which means that they did not survive
# The number at the bottom is the amount of data that goes through that node
# 92% of survival with 1% of the data going through that node

# Let's see how good the prediction is// calculate the quality
pred <- predict(tree, test, type = "class")
pred
# Confusion Matrix
conf <- table(test$Survived, pred)
conf

#   pred
#   0   1
# 0 258   8
# 1   5 147
# The table shows that 258 people that were predicted to die, died; 147 were predicted to survive, did; 

TP <- conf[2,2]
FP <- conf[1,2]
TN <- conf[1,1]
FN <- conf[2,1]

acc <- sum(TP, TN)/sum(TP, TN, FP, FN)
acc
# [1] 0.9688995 // 97% accuracy is pretty good

#There is a shortcut to calculate accuracy:
  acc <- sum(diag(conf))/sum(conf)
acc

# Calculate and print out the precision: prec
prec <- TP/sum(TP,FP)
prec
# [1] 0.9483871
# Calculate and print out the recall: rec
rec <- TP/sum(TP,FN)
rec
# [1] 0.9671053

# This seems very accurate but we know the results. 

# Overfitting
# We could be overfitting and it worksfor the instances it was trained on but not transtlate to a larger scale. 
# Overfitting is a common problem in machine learning. The results are highly interpretable but unreliable.
# Example
overfit_tree <- rpart(Survived ~ Pclass + Sex + Age , train, method = "class", control = rpart.control(xval=10))
fancyRpartPlot(overfit_tree)

pruned <- prune(tree, cp=.01)
fancyRpartPlot(pruned)

# Classification with K Nearest Neighbors
# Create vector lists of Survived from our train and test datasets after removing NAs
train <- drop_na(train)
test <- drop_na(test)
train_labels <- train$Survived
test_labels <- test$Survived

# Create copies.

knn_train <- train
knn_test <- test

# Convert Sex to numeric value
knn_train$Sex <- as.factor(knn_train$Sex)
knn_test$Sex <- as.factor(knn_test$Sex)

knn_train$Sex <- as.factor(gsub("male", "1", knn_train$Sex))
knn_train$Sex <- as.factor(gsub("female", "0", knn_train$Sex))

knn_test$Sex <- as.factor(gsub("male", "1", knn_test$Sex))
knn_test$Sex <- as.factor(gsub("female", "0", knn_test$Sex))

knn_train$Sex <- as.numeric(knn_train$Sex)
knn_test$Sex <- as.numeric(knn_test$Sex)

# Remove the answers (‘Survived’) from our new sets since we are trying to predict who survives, 
# not train on who survives.
knn_train$Survived <- NULL
knn_test$Survived <- NULL

# The point of kNN is to situate each datapoint on a multi-dimensional plane and
# figure out the shortest distance to all the other points. For this to work, all of
# the variables must be on the same scale.

# Normalize Pclass
min_class <- min(knn_train$Pclass)
max_class <- max(knn_train$Pclass)
knn_train$Pclass <- (knn_train$Pclass - min_class) / (max_class - min_class)
knn_test$Pclass <- (knn_test$Pclass - min_class) / (max_class - min_class)
min_class
max_class

# Normalize Age
min_age <- min(knn_train$Age)
max_age <- max(knn_train$Age)
knn_train$Age <- (knn_train$Age - min_age) / (max_age - min_age)
knn_test$Age <- (knn_test$Age - min_age)/(max_age - min_age)

head(knn_train)

# We’ll make a new variable, ‘pred’ and call the knn function on it. The knn
# function requires: training set (knn_train) testing set (knn_test) vector of true
# classifications of the training set (cl) number of neighbors to consider (k)

k_pred <- knn(train=knn_train, test=knn_test, cl=train_labels, k=5)
k_pred
# Confusion Table
conf <- table(test_labels, k_pred)
conf

my_titanic <- data.frame("Age" = .6, "Pclass" = .5, "Sex" = 0)
new_k_pred <- knn(train=knn_train, test=my_titanic, cl=train_labels, k=5)
new_k_pred
# my_titanic person is predicted to have survived

# Clustering
# Basic Clustering - Unsupervised
set.seed(1234)
data(iris)
my_iris <- iris[,1:4]
species <- iris$Species

kmeans_iris <- kmeans(my_iris, centers=3, nstart=10)
kmeans_iris

table(species, kmeans_iris$cluster)

plot(Petal.Length ~ Petal.Width, data = my_iris, col = kmeans_iris$cluster)

fviz_cluster(kmeans_iris, data = my_iris)

# Clustering Unsupervised Unknown Clusters
# Initialize total within sum of squares error: wss
wss_tot <- 0
for (i in 1:15) {
  km_out <- kmeans(my_iris, centers = i, nstart = 10)
  # Save total within sum of squares to wss variable
  wss_tot[i] <- km_out$tot.withinss/km_out$totss
}
plot(1:15, wss_tot, type = "b",
     xlab = "Number of Clusters",
     ylab = "WSS/TSS")

# US Arrests dataset
data("USArrests")
str(USArrests)

#1
wss_tot <- 0
for (i in 1:15) {
  km_out <- kmeans(USArrests, centers = i, nstart = 10)
  # Save total within sum of squares to wss variable
  wss_tot[i] <- km_out$tot.withinss/km_out$totss
}
#2
plot(1:15, wss_tot, type = "b",
     xlab = "Number of Clusters",
     ylab = "WSS/TSS")
#3
kmeans_arrests <- kmeans(USArrests, centers= i, nstart=10)
kmeans_arrests
#4
plot(Murder ~ Assault, data = USArrests, col = kmeans_arrests$Murder)
fviz_cluster(kmeans_arrests, data = km_out)