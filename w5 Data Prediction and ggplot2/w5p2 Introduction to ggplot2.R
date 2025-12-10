library(tidyverse)
library(MASS)
install.packages("rgl")
library(rgl)
data("hills")
nrow(hills)

hills_train <- hills[1:30,]
hills_test <- hills[31:35,]

hills_train[1:10,]

summary(hills)

ggplot(
  data=hills_train,
  aes(x=dist, y=time)
) + geom_point()

ggplot(
  data=hills_train,
  aes(x=climb, y=time)
  ) + geom_point()

cor.test(
  hills_train$dist,
  hills_train$time
)

mod_dist <- lm(
  formula=time~dist,
  data=hills_train
)
summary(mod_dist)

coef(mod_dist)

coefs_dist <- coef(mod_dist)
ggplot(
  data=hills_train,
  aes(x=dist, y=time)
) +
  geom_point() +
  geom_abline(mapping=aes(
    slope=coefs_dist["dist"],
    intercept=coefs_dist["(Intercept)"]
  ), color='red')

hills_resid <- hills_train
hills_resid$predicted <-predict(mod_dist)
hills_resid$residuals <- residuals(mod_dist)
hills_resid[1:10,]

ggplot (
  data=hills_resid,
  aes(x=dist,y=time)
) +
  geom_point(size=3) +
  geom_point(size=2, aes(y=predicted), shape=1) +
  geom_segment(aes(xend=dist, yend=predicted), alpha=0.9, color='red') +
  geom_abline(mapping=aes(
    slope=coefs_dist["dist"],
    intercept=coefs_dist["(intercept)"]
  ),color='gray')

plot(
  mod_dist,
  which=1
)

predict(
  mod_dist,
  newdata=hills_test
)
predict(
  mod_dist,
  newdata=hills_test,
  interval='confidence'
)

hills_dist_test <- hills_test 
hills_dist_test$predicted <- predict(mod_dist, newdata=hills_dist_test)
hills_dist_test$residuals <- hills_dist_test$predicted - hills_dist_test$time
hills_dist_test

ggplot(
  data=hills_dist_test,
  aes(x=dist,y=time)
) +
  geom_point(size=3) + 
  geom_point(size=2, aes(y=predicted), shape=1) + 
geom_segment(aes(xend=dist, yend=predicted), alpha=0.9, color='red') +
  geom_abline(mapping=aes(
    slope=coefs_dist["dist"],
    intercept=coefs_dist["(Intercept)"]
  ), color='gray')

sse_dist <- sum(hills_dist_test$residuals**2)
sse_dist

ggplot(
  data=hills_train,
  aes(x=dist, y=climb)
) + geom_point()

cor.test(hills_train$dist, hills_train$climb)

library(rgl)
plot3d(
  x=hills_train$dist,
  y=hills_train$climb,
  z=hills_train$time
)
rglwidget()

mod_hills <- lm(
  formula=time~climb+dist, # predicting time using both climb AND dist
  data=hills_train
)

plot3d(
  x=hills_train$dist, 
  y=hills_train$climb, 
  z=hills_train$time,
  type='s', size=2, col='blue'
)
coefs <- coef(mod_hills)
planes3d(a=coefs["dist"],b=coefs["climb"],c=-1,d=coefs["(Intercept)"], col='gray') 
rglwidget()

layout(matrix(1:3, ncol=3))
plot(mod_dist, which=1) 
plot(mod_climb, which=1) 
plot(mod_hills, which=1) 
#execise 1
mod_climb <- lm(
  formula = time ~ climb,
  data = hills_train
)
summary(mod_climb)
#Execise 2
mod_climb<- lm(
  formula=time~climb,
  data=hills_train
)
coefs_climb <- coef(mod_climb)
ggplot(
  data = hills_train,
  aes(x = climb, y = time)
) +
  geom_point() +
  geom_abline(mapping = aes(
    slope = coefs_climb["climb"],
    intercept = coefs_climb["(Intercept)"]
  ), color = 'red')

plot(
  mod_climb,
  which = 1
)
#Execise 3
# Make a copy of the test data
hills_climb_test <- hills_test

# Use the climb model to predict times on the test data
hills_climb_test$predicted <- predict(mod_climb, newdata = hills_climb_test)

# Calculate the residuals (predicted - actual)
hills_climb_test$residuals <- hills_climb_test$predicted - hills_climb_test$time

# Display the results
hills_climb_test

sse_climb <- sum(hills_climb_test$residuals**2)
sse_climb
