#THEMA 4

#install.packages("amap")
#install.packages("MASS")
#install.packages("klaR")
#install.packages("plyr")

graphics.off() ; rm(list = ls(all = TRUE)) ; cat("\014");
library(dplyr)
library(amap)
library(MASS)
library(klaR)
library(plyr)

setwd("C:/Users/Aggeliki/Documents/MSc/Διαχείριση Μεγάλων Δεδομένων/3hErgasia2022-2023")

#############     1     ######################################################## 
# Read csv file "movies"
movies <- read.csv("movies.csv", header=TRUE, sep=",")

#############     2     ######################################################## 
# Initialize random number generator. We need this because we'll be be using the nstart
# parameter in the kmeans() function that randomly initializes the process
set.seed(40)

# Initialize a vector wr where we will store our metric. 
# In this example we will use the mean (i.e. avg) of withinss . Since we will execute K-means 19 times we will
# use a vector with dimension 20. The idea is that position i of vector will store the mean withinss of K-means 
# with i centers.
# NOTE: we will ignore wr[1] as we won't execute K-means with 1 center.
wr <-rep(0, 20)


# clustering for K=2,3,4 etc until 100 each time executing K-means.We start with 2 
#clusters (starting with K=1 does not make really sense)
for(i in 2:100) {
  
  # Cluster the data with i centers
  moviesCluster<-kmeans(movies[,3:22], centers=i, nstart=20, iter.max=20)
  
  #Clustering done. Now store our selected metric. 
  wr[i] <- mean(moviesCluster$withinss) 
}

# Now plot the mean withinss values.  Try to see where the elbow is! (elbow method)
plot(2:20, wr[2:20], type="b", xlab="Number of Clusters",
     ylab="Ratio betweenss / totss",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)


#############     3     ######################################################## 
# Cluster the data with 7 centers
moviesCluster<-kmeans(movies[,3:22], centers=7, nstart=20, iter.max=20)

# Create a new column/variable in data.frame movies and assign to each movie, 
# the cluster it belongs to. We do this in order to be able
# to lookup later the cluster for movies the user rated.

movies$clusterId <- moviesCluster$cluster


#############     4     ######################################################## 
# Read csv file "ratings"
ratings <- read.csv("ratings.csv", header=TRUE, sep=",")

# Mean of ratings for each movie in ratings.csv
print("Average rating for each movie ID:")
aver_ratings<-as.data.frame (tapply(ratings$rating, ratings$movieId, mean))


#############     5     ######################################################## 
print("Please choose a user to proceed. (default user: 198)")
#choosing user 198 
user198<-subset(ratings,ratings$userId=='198')
user198
# Now, we do the following: we lookup each movie rated by user 198 from
# movies (containing ALL movies) and return from the matching line the clusterId the
# movie has been assigned to. This value is stored in a new column.
# This can be done using the match() function.

# So, after executing next line, user198 data.frame should have a new column 
# named clusterId containing the assigned cluster id of the movies rated by user 198
user198$clusterId <- movies[ match(user198[,"movieId"], movies[,"movieId"]), "clusterId" ]

# At this point dataframe user198 has a new column with the cluster ids of 
# the movies 198 rated.


#############     6     ########################################################
# Mean of ratings for user 198 by grouped cluster id 
mean_ratings_user_by_cluster <-aggregate(user198$rating, list(user198$clusterId), FUN=mean) 
names(mean_ratings_user_by_cluster) = c("clusterId", "mean_ratings")


#############     7     ########################################################
#remove clusters from mean_ratings_user_by_cluster with a mean rating of < 3.5
highlyRatedClusters <- mean_ratings_user_by_cluster[ -which(mean_ratings_user_by_cluster$mean_ratings < 3.5), ]

#############     8     ########################################################
# Show message if user's mean cluster ratings are all < 3.5
if ( all( highlyRatedClusters$mean_ratings <3.5)){
  print ("Sorry , no recommendations for you!")
}

#############     9     ########################################################
#Order the highly ranked clusters with descending ratings 
highlyRatedClusters<-highlyRatedClusters[ order(highlyRatedClusters$mean_ratings, decreasing=TRUE), ]

# merging movies and ratings
ratings_movies_clusters <- merge(movies, ratings, by.movies = movieId, by.ratings = movieId)
# Keeping only movieId,title,clusterId,userId,rating columns
ratings_movies_clusters = subset(ratings_movies_clusters, select = c(movieId,title,clusterId,userId,rating))

n<-highlyRatedClusters$clusterId
#filter ratings_movies_clusters and keep clusters that exist in vector n which has clusters from highlyRatedClusters
ratings_movies_clusters<-filter(ratings_movies_clusters, clusterId %in% n)

#Removing user 198 from df to recommend movies the user has not already seen
ratings_movies_clusters<-ratings_movies_clusters[!(ratings_movies_clusters$userId=="198"),]

#recommend top 2 movies from each highly rated cluster of user198
d <-data.frame ()

if ( all( highlyRatedClusters$mean_ratings <3.5)){
    # Show message if user's mean cluster ratings are all < 3.5
  print ("Sorry , no recommendations for you!")
    #Otherwise recommend movies for each cluster with rating >=3.5
} else if (any( highlyRatedClusters $mean_ratings >=3.5)){
  print ("You may also like the following movies")
    #Merge the df that have user's cluster ratings and df with movieId,title,clusterId,userId,rating
  d <-merge ( highlyRatedClusters , ratings_movies_clusters , by="clusterId")
    #Order df with higher rating first
  d <- d[ order (d$rating, decreasing = TRUE ),]
    #for each cluster in n (that has the highly rated clusters of user198)
  for (i in n) {
     #Go and show cluster and top 2 movies of said cluster
    if ( any( highlyRatedClusters $ clusterId ==i )){
      m <-d[ which (d$clusterId ==i),]
      print (i)
      for (j in 1:2) {
        print (m[j ,4])
      }
    }
   }
}

