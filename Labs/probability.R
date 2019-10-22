# Probability Lab

# Coinflips
# of times, possibilities, chance
rbinom(1, 1, .5)
# [1] 0

rbinom(10, 1, .5)
# [1] 0 1 1 0 1 1 0 0 1 0

d1 <- rbinom(10, 1, .5)
d1
# [1] 1 1 0 0 1 1 0 1 0 0
sum(d1)
# [1] 5

rbinom(1, 10, .5)
# [1] 4
# How many times it came up heads

rbinom(10, 10, .8)
rbinom(10, 10, .2)

flips <- rbinom(100000, 10, .5)
hist(flips)

flips == 5
# TRUE equals 1, FALSE equals 0

mean(flips == 5)

pbinom(5, 10, .5)

# Women heights
rnorm(10000, 65, 3.5)

heights <- rnorm(10000, 65, 3.5)
hist(heights)

# Function
f <- function(x){ dnorm(x, mean=65, sd=3.5) }
integrate(f, 70, Inf)
# 0.07656373 with absolute error < 2.2e-06

pnorm(70, 65, 3.5)
1-pnorm(70, 65, 3.5)



