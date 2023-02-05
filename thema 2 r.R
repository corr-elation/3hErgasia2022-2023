setwd("C:/metaptyxiako/diaxeirhsh megalwn dedomenwn/ergasia3/erwthma2")
getwd()
library(tibble)
library(e1071)
library(stringr)
library(tm)
library(SnowballC)
library(caret)
library(caTools)
imdbdata = read.csv("C:/metaptyxiako/diaxeirhsh megalwn dedomenwn/ergasia3/erwthma2/IMDBDataset.csv",header=TRUE)

#We use the command 'head' to see the first rows of observations
head(imdbdata)

####Removing special characters....####erwthmatika teleies klp
imdbdata$review= str_replace_all(imdbdata$review, "[^[:alnum:]]", " ")

#### Lower case####ta kanei ola mikra
imdbdata$review = tolower(imdbdata$review)
imdbdata$sentiment = tolower(imdbdata$sentiment)

#### stop words####afairei tis klassikes lexeis px arthra
imdbdata$review=removeWords(imdbdata$review,stopwords("english"))

#### stemming ####
# to see the available languages
getStemLanguages()
wordStem(imdbdata$review, language = "english")
###Here we make the stemming
stemDocument(imdbdata$review)
imdbdata$review = stemDocument(imdbdata$review)


#erwthma 3

dtm <- DocumentTermMatrix(imdbdata$review)
inspect(dtm)


#erwthma 4
set.seed(1234)

#use 80% of dataset as training set and 20% as test set
indexset <- sample(2, nrow(imdbdata), replace = TRUE, prob = c(0.8,0.2))
trainingset  <- imdbdata[indexset==1,]
testset <- imdbdata[indexset==2,]

indexset

#erwthma5

# Fitting Naive Bayes Model to training dataset
NaiveBayesModel <- naiveBayes (sentiment~ ., data = trainingset)
NaiveBayesModel

### predicy sentiment for the test set
NBMpredict = predict(NaiveBayesModel, newdata= testset)
NBMpredict

# Confusion Matrix
testingDataConfusionTable = table(NBMpredict,testset$sentiment)
print(testingDataConfusionTable)

#### Accuracy##
modelAccuracy = sum( diag(testingDataConfusionTable)/sum(testingDataConfusionTable))

sprintf("Model accuracy: %f", modelAccuracy)
