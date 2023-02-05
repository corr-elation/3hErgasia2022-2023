
#First of all,we download the dataset from the link below:
#https://archive.ics.uci.edu/ml/datasets/Mushroom and after this ,in the R environment we  
#create a dataset with this data. We renamed the variables, and we separate the sample in training 
#sample and test sample. Then,we create the decision tree,we make a  prediction for  the class 
#attribute (High) for the testing dataset and try to evaluate how well our testing data was 
#classified by calculating the  Confusion Matrix. After this we calculate the accuracy of our model.

library(rpart)
library(rpart.plot)

data<- read.csv("", header=FALSE)
cls = data$V1
data = data[,-1]
colnames(data) <- c("cap-shape", "cap-surface", "cap-color", "bruises",
                    "odor","gill-attachment", "gill-spacing","gill-size",
                    "gill-color","stalk-shape","stalk-root","stalk-surface-above-ring",
                    "stalk-surface-below-ring","stalk-color-above-ring","stalk-color-below-ring","veil-type",
                    "veil-color","ring-number","ring-type","spore-print-color",
                    "population","habitat")
as.vector(cls)
data = cbind.data.frame(data, cls)
set.seed(2)
train = sample( 1:nrow(data), nrow(data)/1.25)
test = -train 
training_data = data[train,]
tree_model=rpart(cls~.,training_data)
rpart.plot(tree_model)
testing_data = data[test,]
head(testing_data)
tree_predict = predict(tree_model, testing_data, type="class")
testingDataConfusionTable = table(tree_predict, testing_data$cls)
print(testingDataConfusionTable)
modelAccuracy = sum( diag(testingDataConfusionTable)/sum(testingDataConfusionTable))
sprintf("Model accuracy: %f", modelAccuracy)


#We calculate for the first 30 observations of the dataset, the Entropy gain of the
#attribute "habitat", if the mushroom is poisonous or edible. The variable
#"habitat" has four values, "g" for grasses, "m" for meadows, "u" whether the
#habitat is urban and "d" whether the habitat is woods. The categorization
#attribute is the variable p, whether the mushroom is edible or poisonous.

entropy_entire= - (21/30)*log2(21/30) - (9/30)*log2(9/30) 
Entropy_g = - (7/11)*log2(7/11) - (4/11)*log2(4/11) 
Entropy_m = - (11/11)*log2(11/11) - 0 
Entropy_u = - (2/7)*log2(2/7) - (5/7)*log2(5/7) 
Entropy_d = - (1/1)*log2(1/1) - 0
Entropy_split<- 11/30*Entropy_g + 12/30*Entropy_m + 6/30*Entropy_u + 1/30*Entropy_d 
Entropy_gain = entropy_entire - Entropy_split 
