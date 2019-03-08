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

# Observations of the scatterplot
# Some outliers, with price and volume not looking to have correlation.

# What's the correlation bewtween price and volume? Exclude diamonds that have a volume
# of 0 or that are greater than 800

diamondsVolumeFiltered <- subset(diamonds, volume>0 & volume <= 800)
with(diamondsVolumeFiltered, cor(price, volume))

# Subset the data to exclude diamonds with a volume
# greater than or equal to 800. Also, exclude diamonds
# with a volume of 0. Adjust the transparency of the
# points and add a linear model to the plot. (See the
# Instructor Notes or look up the documentation of
# geom_smooth() for more details about smoothers.)

ggplot(data = diamondsVolumeFiltered, aes(x=volume, y=price)) + geom_point(alpha=0.01) +
  stat_smooth(method = "lm")


# Do you think this would be a useful model to estimate
# the price of diamonds? Why or why not?

# The relationship seems to not be linear, so this won't be useful to estimate the price.

ggplot(data = diamondsVolumeFiltered, aes(x=volume, y=price)) + geom_point(alpha=0.01) +
  geom_smooth()

# Use the function dplyr package
# to create a new data frame containing
# info on diamonds by clarity.

# Name the data frame diamondsByClarity
# The data frame should contain the following
# variables in this order.
#       (1) mean_price
#       (2) median_price
#       (3) min_price
#       (4) max_price
#       (5) n
# where n is the number of diamonds in each
# level of clarity.

library(dplyr)

clarity_groups <- group_by(diamonds, clarity)
diamondsByClarity <- summarise(clarity_groups,
                          mean_price = mean(price),
                          median_price = median(price),
                          min_price = min(price),
                          max_price = max(price),
                          n = n())

diamondsByClarity <- arrange(diamondsByClarity, clarity)
head(diamondsByClarity)

# We’ve created summary data frames with the mean price
# by clarity and color. You can run the code in R to
# verify what data is in the variables diamonds_mp_by_clarity
# and diamonds_mp_by_color.

# Your task is to write additional code to create two bar plots
# on one output image using the grid.arrange() function from the package
# gridExtra.

diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

library(gridExtra)

p1 <- ggplot(data=diamonds_mp_by_clarity, aes(x=clarity, y=mean_price)) + geom_bar(stat = "identity")
p2 <- ggplot(data=diamonds_mp_by_color, aes(x=color, y=mean_price)) + geom_bar(stat = "identity")
grid.arrange(p1,p2,ncol=1)

## What to do you think about the results?

## It's weird, but mean price tends to decrease while both clarity and color improve. This
## trends go against intuition. 