# Breakfast Cereals
# The objective is to predict rating of the cereal dataset variables such as calories, proteins, fat etc.
# I used rating as the dependent variable and calories, proteins, fat, sodium and fiber as the independent variables.
# I used a subset of cereal dataset: https://www.kaggle.com/crawford/80-cereals

Abdul <- read.csv("~/Desktop/eBooks/Data mining/Assignment5/cereals.csv")
str(Abdul)

# I used min-max normalization to scale the data
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# apply normalization to entire data frame
Abdul_norm <- as.data.frame(lapply(Abdul, normalize))

# confirm that the range is now between zero and one
summary(Abdul_norm$rating)

# compared to the original minimum and maximum
summary(Abdul$rating)

# create training and test data
Abdul_train <- Abdul_norm[1:50, ]
Abdul_test <- Abdul_norm[51:75, ]

# install the neuralnet model
install.packages("neuralnet")
library(neuralnet)

# simple ANN with only a single hidden neuron
Abdul_model <- neuralnet(formula = rating ~ calories + protein +
                           fat + sodium + fiber,
                            data = Abdul_train)

# visualize the network topology
plot(Abdul_model)

# obtain model results
model_results <- compute(Abdul_model, Abdul_test[1:5])

# obtain predicted rating values

predicted_rating <- model_results$net.result

# This is a numeric prediction, so we cannot use a confusion matrix to
# look at accuracy.  We must measure the corellation between the 
# predicted  rating and the true value.
# examine the correlation between predicted and actual values

cor(predicted_rating, Abdul_test$rating)

# Improving model performance by using 3 hidden neurons

set.seed(12345) 
Abdul_model2 <- neuralnet(rating ~ calories + protein + fat + sodium + fiber,
                             data = Abdul_train, hidden = 3)

# plot the network

plot(Abdul_model2)

# My model has 3 neurons in its hidden layer. The black lines show the connections with weights.
# The weights are calculated using the back propagation algorithm. 
# The blue line is the displays the bias term.

# evaluate the results as we did before

model_results2 <- compute(Abdul_model2, Abdul_test[1:5])
predicted_rating2 <- model_results2$net.result
cor(predicted_rating2, Abdul_test$rating)

# The accuracy has improved.
