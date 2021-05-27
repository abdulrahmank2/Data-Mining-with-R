# Import the dataset
Abdul <- read.csv("~/Desktop/dataset-11424.csv")

# List the dataset structure
str(Abdul)

# To answer my question, I will add a variable to change the sales from numeric to a binary variable. If it equal or above 7.5, we will consider it as high.
High <- ifelse(Abdul$Sales<=7.5, "No", "Yes")
Abdul <- data.frame(Abdul, High)


#Split the data
set.seed(123)

# random sample 300 out of the 400 observations
Abdu_train <- sample(1:nrow(Abdul), 300)

# List the dataset structure
str(Abdu_train)

# split the data frames
train <- Abdul[Abdu_train, ]
test  <- Abdul[-Abdu_train, ]  


# We want to make sure we have proportional Hight or not in our training and test samples. 
prop.table(table(train$High))   
prop.table(table(test$High))   

# Create our model using rpart
install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

Abdul_fit <- rpart(High ~ Price+Age, data = train, method = 'class')

# In rpart it is easier to plot the tree
rpart.plot(Abdul_fit)

#From the plot we see that (starting with the root at the top),
# 49% considered to be not high. Of those that were the price was less than 106, there are 71%  high. If the age bigger or equal 69 ,
# there are 77% chance of being  high. We can look at the other branches to find feature that determine the probability of being high, bit I interpret one side in the plot to clarify that I understand.

# Now I'm going to predict it on the test set
predict_unseen <-predict(Abdul_fit, test, type = 'class')

# And we can output it:
table_test <- table(test$High, predict_unseen)
table_test

# Our accuracy can be calculated by summing the numbers on the diagonal divided by the sum of values in the table
accuracy_Test <- sum(diag(table_test)) / sum(table_test)

print(paste('Accuracy for test', accuracy_Test))
# Our accuracy is 71%
