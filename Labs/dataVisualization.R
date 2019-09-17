install.packages("ggthemes")

library(ggthemes)
library(tidyverse)
library(dplyr)
library(lubridate)

# ---- Flowers Visualization ----

?iris
# Edgar Anderson's Iris Data

# Load data and inspect it
data(iris)
summary(iris)
glimpse(iris)

# Check formatting
head(iris)

# Look for null variables
sum(is.na(iris))
# the sum of null variables is none :D

# Create a plot graph of width over length
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point()

# Change the geometry form a point to jitter and set transparency to 60%
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_jitter(alpha=.6)
# You can see where clusters are darker more points are layered over each other. You can also guess that the cluster at the top left may be one particular species.

# Add color variable to species
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
  geom_jitter(alpha=.6)

# Add title to plot graph
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
  geom_jitter(alpha=.6) +
  labs(title = "3 Species: Sepal Length by Sepal Width in Iris Dataset")

# Faceting is breaking something into small multiples
# Facet this grid by the variable Species
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
  geom_jitter(alpha=.6) +
  facet_grid(.~Species) +
  labs(title = "3 Species: Sepal Length by Sepal Width in Iris Dataset")

# Look for an overall trend or line of best fit, use stat
# The method is linear, standard error is false and the color is red (or Hex code)
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
  geom_jitter(alpha=.6) +
  facet_grid(.~Species) +
  stat_smooth(method="lm", se=FALSE, col="#666666") +
  labs(title = "3 Species: Sepal Length by Sepal Width in Iris Dataset")

# Create an object that you can use to adjust geometrics and colors
iris_plot <- ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species))

# Every visualization needs a dataset, aesthetic and geometrics
# We are saving the dataset and aesthetic to an object to reuse with different geometrics

iris_plot + geom_point()
iris_plot + geom_jitter()
iris_plot + geom_line()

# Make jitter consistant across all plots
posn <- position_jitter(width = 0.1)
iris_plot + geom_point(position = posn)


# Find the midpoint for each species to show an average sepal
# Create a function that calculates this value for each species
iris_summary <- aggregate(iris[1:4], list(iris$Species), mean)
names(iris_summary)[1] <- "Species"
iris_summary

# add these values to plot by creating a chart within a chart
iris_plot + geom_point(position = posn) +
  geom_point(data=iris_summary, shape=21, size=5, fill="#00000080")

# add line to show average
iris_plot + geom_point(position = posn) +
  geom_vline(data=iris_summary, aes(xintercept=Sepal.Length))

# add y axis 
iris_plot + geom_point(position =posn, alpha=.6) +
  geom_vline(data = iris_summary, aes(xintercept = Sepal.Length)) +
  geom_hline(data = iris_summary, aes(yintercept = Sepal.Width)) +
  facet_grid(.~Species)

# adjust linetypes
iris_plot + geom_point(position =posn, alpha=.6) +
  geom_vline(data = iris_summary,linetype = 2, aes(xintercept = Sepal.Length)) +
  geom_hline(data = iris_summary, linetype = 3, aes(yintercept = Sepal.Width)) +
  facet_grid(.~Species)

# Begin Final Visualization
# limits set the range of values on axis, breaks say when to begin and ends with the value of breaks, expand keeps points from going off of map
iris_plot +
  geom_point(position = posn, alpha = 0.5) +
  facet_grid(.~Species) +
  stat_smooth(method = "lm", se= FALSE, col = "#999999") +
  scale_y_continuous("Sepal Width (cm)",
                     limits = c(1,5),
                     expand = c(0,0)) +
  scale_x_continuous("Sepal length (cm)",
                     limits = c(4,8),
                     breaks = seq(2, 8, 2),
                     expand = c(0,0)) +
  coord_equal() +
  labs(title = "3 Species from Iris Dataset: Sepal Length by Sepal Width (cm)")

# Add theme
iris_plot +
  geom_point(position = posn, alpha = 0.5) +
  facet_grid(.~Species) +
  stat_smooth(method = "lm", se= FALSE, col = "#999999") +
  scale_y_continuous("Sepal Width (cm)",
                     limits = c(1,5),
                     expand = c(0,0)) +
  scale_x_continuous("Sepal length (cm)",
                     limits = c(4,8),
                     breaks = seq(2, 8, 2),
                     expand = c(0,0)) +
  coord_equal() +
  labs(title = "3 Species from Iris Dataset: Sepal Length by Sepal Width (cm)") +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        strip.background = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text = element_blank(),
        panel.spacing = unit(1, "lines")
  )

iris_theme <- theme(panel.background = element_blank(),
                    plot.background = element_blank(),
                    legend.background = element_blank(),
                    legend.key = element_blank(),
                    strip.background = element_blank(),
                    axis.text = element_text(colour = "black"),
                    axis.ticks = element_line(colour = "black"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    axis.line = element_line(colour = "black"),
                    strip.text = element_blank(),
                    panel.spacing = unit(1, "lines")
)

iris_plot + 
  geom_point(position=posn) + 
  labs(title = "3 Species from Iris Dataset: Sepal Length by Sepal Width (cm)") +
  iris_theme

??ggthemes

iris_plot + 
  geom_jitter() + 
  theme_tufte()




