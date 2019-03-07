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
