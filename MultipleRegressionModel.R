install.packages(c('car', 'gvlma', 'MASS', 'leaps')) 

auto <- read.csv("~/Desktop/auto-mpg.csv")
str(auto)

auto <- auto[,-9]
auto <- auto[,-4]
# I needed to drop name and horsepower columns which are  qualitative variables.
str(auto)

plot(auto, col="navy", main="Matrix Scatterplot")
# From the plot we can see that mpg seems to have the same pattern with displacement and weight

auto_1 <- lm(mpg ~ cylinders + displacement  + weight + acceleration + model.year,
          data = auto)
summary(auto_1)

#From the model output we found that:
#The p value of cylinders, displacement, and acceleration shows that there is no significant relation with mpg.
#The RSE is 3.44 where p-value is very small.
#The matrix scatterplot above shows that there is the high correlation between The intercept(mpg), weight, and model.year.
# The mpg is also strongly correlated to weight, and model.year. We can know that by looking to the t-value for each variable.
# F tells us the strength of the overall model. The F value in the model is 331.4
# RAS = 3.44
# F tells us the strength of the overall model.  If we are improving the model,
# F should go up
# R^2 tells us the percent variation in the dependant variable (mpg) that can
# be explained by the predictor variables (or the regression equation) which is equal to 0.809

# As the cylinders, displacement, and acceleration don’t show any significant relation in the first model with mpg I will exclude them.

auto_1 <- lm(mpg ~ weight  + model.year,
             data = auto)
summary(auto_1)

# In this model, we find all predictors p-value is highly significant. 
# After excluding the collinear variables the F- statistic improved from 331.4 to 830.4 which is a great improvement.
# But there is no improvement on RSE and adjusted R squared value. 
plot(auto_1, which =1)
#The plot shows some of the outliers  far away from the middle of the plot
# There are four approaches to dealing with violations of regression assumptions and I will use one of them (log)
auto_2 <- lm(log(mpg) ~ weight  + model.year,
             data = auto)
summary(auto_2)
#After some changes in the model, the performance of the model is increased.
#The Adjusted R-squared raised to 0.8713 and F-statistic is increased to 1337.
#The RSE is 0.87 has improved which is good because it's near to 0
#The p-value of the predictors is significant.

auto_3 <- lm(log(mpg) ~ log(weight)  + model.year,
             data = auto)
summary(auto_3)

# The output of this model shows that the F-statistics is increased to 1472 and the Adjusted R-squared is also increased.
# The predictor has highly significant p values. 
# This model is better than previous models.

# VIF values are provided by the vif() function in the car package.
# As a general rule, a sqrt >2 indicates a multicollinearity problem.
library(car)
vif(auto_3)
sqrt(vif(auto_3)) > 2
# No multicolinearity here

library(gvlma)
gvmodel <- gvlma(auto_3)
summary(gvmodel)
# Based on the result of (p = 1.055, from global stat) the data meet only two of the statisical assumptions 

library(MASS)
auto_4<- lm(mpg ~ cylinders + displacement  + weight + acceleration + model.year,
            data = auto)
stepAIC(auto_4, direction = "backward")
# We start with all five predictors in the model. 
# For each step, the AIC column provides the model AIC resulting from the deletion 
# of the variable listed in that row. 
#
# In the first step, cylinders is removed, decreasing the AIC from 989.48 to 988.11. 
# In the second step, displacement is removed,decreasing the AIC to 986.55.
# In the third step, acceleration is removed,decreasing the AIC to 985.19.
# Deleting any more variables would increase the AIC, so the process stops.

# My manual step by step improvement for my model is better than the backwards stepwise using the automatic function from the results.


