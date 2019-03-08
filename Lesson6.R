setwd("~/Desktop/eda-course-materials/lesson6/solutions")
getwd()

library(ggplot2)
data(diamonds)
summary(diamonds)

ggplot(data = diamonds, aes(x = carat, y = price)) + geom_point()

# It seems that there is an exponential relationship between price and carat. 
# There are some outliers in the sample.

summary(diamonds$carat)
summary(diamonds$price)
quantile(diamonds$carat)
quantile(diamonds$price)
quantile(diamonds$carat, c(.9))
quantile(diamonds$price, c(.9))

# 90% of the diamonds have a carat below 1.51 and price under 9821.

ggplot(data = diamonds, aes(x = carat, y = price)) + 
  geom_point() + scale_x_continuous(lim = c(.2, quantile(diamonds$carat, .95))) +
  scale_y_continuous(lim = c(326, quantile(diamonds$price, .95)))

# Correlation

with(diamonds , cor.test(price, x))
with(diamonds , cor.test(price, y))
with(diamonds , cor.test(price, z))

# Create a simple scatter plot of price vs depth.

ggplot(data = diamonds, aes(x = depth, y = price)) + 
  geom_point(alpha=0.01) + scale_x_continuous(lim = c(55,70), breaks = seq(50,70,2))

# Correlation between depth vs price

with(diamonds , cor.test(depth, price))

# Create a scatterplot of price vs carat
# and omit the top 1% of price and carat
# values.

?coord_cartesian
summary(diamonds$carat)
summary(diamonds$price)

ggplot(data = diamonds, aes(y = price, x= carat)) + geom_point(alpha=0.05) +
 coord_cartesian(ylim = (c(326, quantile(diamonds$price, .99))), xlim = (c(.2, quantile(diamonds$carat, .99))))

# Create a scatterplot of price vs. volume (x * y * z).
# This is a very rough approximation for a diamond's volume.

# Create a new variable for volume in the diamonds data frame.
# This will be useful in a later exercise.

# Don't make any adjustments to the plot just yet.

diamonds$volume <- with(diamonds, x*y*z)

summary(diamonds$volume)

ggplot(data = diamonds, aes(x=volume, y=price)) + geom_point()

