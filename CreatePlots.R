data()
data(HairEyeColor)
train.data = read.csv("~/Desktop/HairEyeColor.csv")
str(train.data)
sum(is.na(train.data$Hair) == TRUE)
sum(is.na(train.data$Eye) == TRUE)
sum(is.na(train.data$Sex) == TRUE)


barplot(table(train.data$Sex), main="Student Gender")

barplot(table(train.data$Hair), main="Hair Color")

barplot(table(train.data$Eye), main="Eye Color")



counts <- table( train.data$Hair, train.data$Sex)

counts

barplot(counts,  col=c("darkblue","red","Yellow","Green"), legend = c("Black", "Brown","Red","Blond"), main = "Hair color by Gender")

counts <- table( train.data$Eye, train.data$Sex)

counts

barplot(counts,  col=c("blue","red","Yellow","Gray"), legend = c("Blue", "Brown","Green","Hazel"), main = "Eye color by Gender")

hist(train.data$Freq, main="Students Frequency", xlab = "Frequency")

hist(train.data$Freq[which(train.data$Eye == "Brown")], 
     main= "Green's eye color by frequency", xlab="Frequency", ylab="Count", col ="Brown")

hist(train.data$Freq[which(train.data$Eye == "Green")], 
     main= "Green's eye color by frequency", xlab="Frequency", ylab="Count", col ="Green")

boxplot(train.data$Freq ~ train.data$Sex, 
        main="Student gender by Frequency",
        xlab="Sex", ylab="Frequency")

install.packages("Rcpp")
library(Rcpp)

install.packages('tidyverse')
library(tidyverse)


install.packages("ggplot2")
library (ggplot2)

ggplot(data = train.data) + 
  geom_point(mapping = aes(x = Sex, y = Freq))


ggplot(data = train.data) + 
  geom_point(mapping = aes(x = Sex, y = Freq, color = Hair))

ggplot(data = train.data) + 
  geom_point(mapping = aes(x = Sex, y = Freq, size = Eye))

ggplot(data = train.data) + 
  geom_point(mapping = aes(x = Sex, y = Freq, shape = Eye))

ggplot(data = train.data) + 
  geom_point(mapping = aes(x = Sex, y = Freq, color = "Red"))


ggplot(data = train.data) + 
  geom_point(mapping = aes(x = Hair, y = Freq, color = Hair))

ggplot(data = train.data) + 
  geom_point(mapping = aes(x = Eye, y = Sex, size = Eye))


ggplot(data = train.data) +
  geom_smooth(mapping = aes(x = X, y = Freq, color = Hair,
                            show.legend = FALSE))



