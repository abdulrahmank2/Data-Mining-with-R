# Import the dataset
Abdul <- read.csv("~/Desktop/adult-training.csv")

# Installing the backages to rename one of the variable to make it clear and change the income levels.
install.packages("reshape")
require(reshape)
install.packages("plyr")
require(plyr)

# Rename the variable
Abdul <- rename(Abdul, c(X..50K = "Income"))
Abdul <- rename(Abdul, c(White = "Race"))
Abdul <- rename(Abdul, c(Male = "Gender"))


levels(Abdul$Income) 

# We do this with the revalue function.  Chane the  income <=50K to low and >50K to High.
# the customer is classified high or  low category
Abdul$Income <- revalue(Abdul$Income, c(" <=50K"= "Low", " >50K"= "High"))


# View the new change:
str(Abdul)

# Data cleaning
Abdul1 <- Abdul

str(Abdul1)

# A few variables that I believe might be irrelevant with regards to this model should be removed. 
Abdul1 <- Abdul1[, c(-1,-2,-3,-4,-5,-7,-11,-12,-13,-14)]

# View the new change:
str(Abdul1)


# Naive Bayes Classifier Model
# Installing the libraries,


install.packages('e1071')
install.packages('caret')



library(e1071)
library(caret)

# Set the seed of R‘s random number generator
set.seed(2)

# Divide the dataset into training and testing sets, keeping the ratio as 6:4,

random1 <- sample(2, nrow(Abdul1), prob = c(0.6, 0.4), replace = T)

Abdul_train <- Abdul1[random1 == 1, ]
Abdul_test <- Abdul1[random1 == 2, ]

# Running the naiveBayes() function. Keeping "Income" as the dependent 
# variable and considering all the other 4 variables as independent variables 
Abdul_nb <- naiveBayes(Income ~ . , data = Abdul_train)

Abdul_nb

# Insights
# 1. For Never married variable, customers who are  "Divorced", "Married-spouse-absent", "Never-married ", "Separated", and "Widowed" has probabilities 
# of low income. Those who are  "Married-AF-spouse", "Married-civ-spouse" has the opposite result. 
# 
# 2. Not.in.family, customers who are  "Husband" and "Wife"has probabilities of income being high than being low. 
# But those who are "Not-in-family","Other-relative", "Own-child", "Unmarried" shows the opposite insight.
# 
# 3. For Race, customers who are "Amer-Indian-Eskimo", "Black", "Other" have low income, but those who are "Asian-Pac-Islander" and "White" have high income
# 
# 4. Gender, customers who are Male have high income not like the female customers.


# Now we run the model on the test data and get the predictions.

Real_nb <- predict(Abdul_nb, Abdul_test)

#create a confusion matrix out of Real_nb

confusionMatrix(table(Real_nb, Abdul_test$Income))

# The accuracy is 71%.
# 
# Validation Observations
# We can see that there are much 
# lower wrong predictions (515 + 3211) compared to the correct predictions (6704 + 2619).
# 
# Accuracy per cent is high (71%) which is a good indication.
# P-value is 1.
# 
# Kappa statistic is (around 40%).
# 
# Sensitivity is 0.67, but Specificity is 0.83
# 'Positive' Class = Low 

