
## Exploring and preparing the data.
arrests <- read.csv("~/Desktop/eBooks/Data mining/Assignment6/USArrests.csv")
str(arrests)

# look at missing data for states variable

table(arrests$X)

# We must add another argument if we want to know NAs

table(arrests$X, useNA = "ifany")

# We don't have any missing value


# look at missing data for the rest variables

summary(arrests$Murder)
summary(arrests$Assault)
summary(arrests$UrbanPop)
summary(arrests$Rape)

# Also, We don't have any missing value

# finding the mean for the variables by cohort
mean(arrests$Murder)
mean(arrests$Assault)
mean(arrests$UrbanPop)
mean(arrests$Rape)

## --- Training a model on the data ----
# Lets start by looking at the 36 features that represent the number of times
# various interest words appeared in the teen profiles

arrests_abdul <- arrests[2:5]

# We can normalize the data using the z score by applying the scale()and 
# lapply() functions. lapply returns a matrix, so we must change it to a data
# frame:


arrests_abdul1 <- as.data.frame(lapply(arrests_abdul, scale))
# create the model, here we are creating 2 clusters:

set.seed(2345)
arrests_clusters <- kmeans(arrests_abdul1, 2)

str(arrests_clusters)

## --- Evaluating model performance ----
# look at the size of the clusters

arrests_clusters$size

arrests_clusters$centers
# We see the cluster centers (means) for the two groups across the four variables (Murder, Assault, UrbanPop, Rape)
# look at the cluster centers. These values are z score standardized
# positive values are above the mean level for all arrests and negative
# values are below the mean. The first row, for example has a high value
# for murder and assault more than the Rape. 


# Obviously cluster 1 has assrests more than cluster 2.

arrests_clusters$withins
# Within cluster sum of squares by cluster: [1] 54762.30 41636.73 (between_SS / total_SS =  72.9 %)
# This shows how diverse or similar each cluster is.
# High numbers indicate higher dissimilarity, low numbers indicate those in the specific cluster are more similar. 

arrests_clusters

# Now we apply the clusters back into the full dataset to see if we can
# gain more insight from the data

arrests$cluster <- arrests_clusters$cluster

# look at the records from record 7 to record 14

arrests[7:14, c("X","cluster", "Murder", "Assault", "UrbanPop","Rape")]

# we can look at mean rape by cluster
aggregate(data = arrests, Rape ~ cluster, mean)

# we can look at mean marder by cluster
aggregate(data = arrests, Murder ~ cluster, mean)

# we can look at mean Assault by cluster
aggregate(data = arrests, Assault ~ cluster, mean)

# I can execute the same process for 4 clusters and I will see the result.
set.seed(2345)
arrests_clusters <- kmeans(arrests_abdul1, 4)

str(arrests_clusters)

## --- Evaluating model performance ----
# look at the size of the clusters

arrests_clusters$size

arrests_clusters$centers
# We see the cluster centers (means) for the two groups across the four variables (Murder, Assault, UrbanPop, Rape)
# look at the cluster centers. These values are z score standardized
# positive values are above the mean level for all arrests and negative
# values are below the mean. The second row, for example has a high value
# for murder, and the first row has a high value for rape and assault. 


# Obviously cluster 1 has assault and rape more than the rest clusters, but cluster 2 has murders more than the rest.

arrests_clusters

# Now we apply the clusters back into the full dataset to see if we can
# gain more insight from the data

arrests$cluster <- arrests_clusters$cluster

# look at the records from record 1 to record 14

arrests[1:14, c("X","cluster", "Murder", "Assault", "UrbanPop","Rape")]

# We can see each state belongs to which cluster.

# we can look at mean rape by cluster
aggregate(data = arrests, Rape ~ cluster, mean)

# we can look at mean marder by cluster
aggregate(data = arrests, Murder ~ cluster, mean)

# we can look at mean Assault by cluster
aggregate(data = arrests, Assault ~ cluster, mean)

