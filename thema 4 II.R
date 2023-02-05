
setwd("C:/Users/Aggeliki/Documents/MSc/Διαχείριση Μεγάλων Δεδομένων/3hErgasia2022-2023")
# Read csv file
# The dataset  europe is  a csv file 
europe <- read.csv("europe.csv", header=TRUE, sep=",")

# Some descriptive statistics of the data to see
# if our data looks fine for hierarchical clustering 
summary(europe)

# Attributes/Variables have different scales. Since# we will be using
# Euclidean distance in the distance matrix, this may introduce bias. 
# Hence, try to normalize each value of attribute to a scale from 0 to 1.

# We will use min-max normalization. It's easy and works (for most cases).
# Define the function norm that will normalize a value using the min-max
# method

norm <- function(x){ return( (x-min(x)) / (max(x)-min(x)) ) }

# Each attribute of the dataset through the norm function

# This will normalize our non string variables: GDP, Inflation, Life.expect, 
# Military, Pop.growth, Unemployment.
europe[,"GDP"]<- norm(europe$GDP)
europe[,"Inflation"]<- norm(europe$Inflation)
europe[,"Life.expect"]<- norm(europe$Life.expect)
europe[,"Military"]<- norm(europe$Military)
europe[,"Pop.growth"]<- norm(europe$Pop.growth)
europe[,"Unemployment"]<- norm(europe$Unemployment)

# Now normalized data
summary(europe)

# Now, we will calculate the initial distance matrix for all data points,
# by removing first two variables that indicate the country and the area code
# 1st and 2nd variables 

# We will use the R function dist() to calculate the entire distance matrix based on 
#the Euclidean distance. To tell R to take into consideration all attributes except
# the first and the second.

distanceMatrix <- dist(europe[,3:8])

# Distance matrix calculated. We can now proceed to execute 
# Agglomerate hierarchical clustering using the hclust function

# The hclust() function executes hierarchical clustering.
# hclust() takes a shitload of arguments, but the important ones
# are two: 1) the distance matrix that we already computed above and 
# 2) the distance are two: 1) the distance matrix that we already computed 
# above and 2) the distance measure for clusters. First argument of hclust
# is the distanceMatrix that has been calculated previously. 
# If no argument for the distance measure of clusters is given 
# (parameter method), the "Complete Linkage" measure is assumed. 
# Here we use the complete linkage by explicitly specifying it.

europeHClustering <-hclust(distanceMatrix, method="complete")

# Hierarchical clustering finished. Plot the dendrogram using the
# plot() function. Second parameter labels= tells R to display labels
# (in our case the Country) on the horizontal axis.
#
plot(europeHClustering, labels=europe$Country)
