
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier
from sklearn import preprocessing
from sklearn.metrics import accuracy_score
from sklearn.metrics import confusion_matrix, classification_report
from sklearn import tree

# import required library for visualizing decision tree
from matplotlib import pyplot as plt

#import data and names#
data = pd.read_csv("agaricus-lepiota.data", sep=",",  header=None)
data.columns = ["cls", "cap-shape", "cap-surface", "cap-color", "bruises",
                "odor","gill-attachment", "gill-spacing","gill-size",
                "gill-color","stalk-shape","stalk-root","stalk-surface-above-ring",
                "stalk-surface-below-ring","stalk-color-above-ring","stalk-color-below-ring","veil-type",
                "veil-color","ring-number","ring-type","spore-print-color",
                "population","habitat"]


print("Variables of DataFrame data:")
print(data.columns)

# One-Hot encoding for categorical variables.
print("\n\nPreprocessing dataset\n")

# First, get a list of the names of variables in our dataset that are categorical.
categoricalVariables=data.select_dtypes([object]).columns

# CAVEAT: We only avoid the class attribute! The class attribute, that takes binary values 'p', 'e'
# will not be One-hot encoded. We use a different way to encode the class attribute to 0 and 1.
for var in categoricalVariables:
  
  print("\tOne-Hot-Encoding variable ", var, " .....", sep="", end="")
# Is this variable our class attribute? If so, DO NOT One-hot encode it i.e. ignore it
if var == "cls":
  print("Ignored")  # yes it is the class attribute. Ignore it for now,
# as we will handle it differently
continue  # don't execute the code below. Go on to next attribute

# If we reach this point during the for-loop, the variable var IS NOT the class attribute
# and catefffgorical. Hence, One-hot encode it.

# First, make the variable we are going to One-hot encode explicitly categorical
data[var] = pd.Categorical(data[var])
# Now, this does the actual work of One-hot encoding of variable var.
# varDummies will be a pandas DataFrame with new variables, one for each
# possible value of variable var and the proper value for each of these variables
# for the respective entry in the original dataset.
varDummies = pd.get_dummies(data[var], prefix=var)
# One-hot encoding of variable done.
# Now, add the new variables to our original dataset.
data = pd.concat([data, varDummies], axis=1)
# And remove the original variable from our dataset. We don't need it anymore
# since we have One-hot encoded it and added the new variables to the DataFrame.
data = data.drop([var], axis=1)
print("Done")
# Finished. Go to next categorical variable in our dataset and do the same.


# If we reach this point, all categorical variables have been One-hot encoded. We print out
# the variables of the DataFrame just to get a look at how many new variables were added
print("\n\tVariables of DataFrame bankData after One-hot encoding:")
print("\t", data.columns)

# We left out the class attribute during One-hot encoding.
# Since the class attribute (attribute/variable cls in our dataset) already takes two values ('p', 'e') and hence is in
# essence already a binary variable, we just do different encoding of these values: we will transform the value 'p' to the number 0
# and the value 'e' to the value 1.
# We do this encoding and add a new column to the end of DataFrame  named newY, containing these new values of 0 and 1.
# Once we have done this, the original column/variable cls is not needed anymore and can be dropped.
data['newcls'] = ( data['cls'].map( {'p':0, 'e':1}) )

# Drop the column y. Not needed anymore since we have added the column newY
data=data.drop(['cls'], axis=1)
print("\n\nPreprocessing done.")

# These are our features i.e. the variable values that we will use to predict the class attribute.
# Remember that our class attribute is called newcls and is the LAST one in our DataFrame. Hence
# we select the features with :-1 which means all columns except lst one (which is the class attribute).
features = data.iloc[:, :-1]

# Get the values of the class variable i.e. the newY variable/column
classVariable = data["newcls"]

# Split the DataFrame into a training and testing set.
# We use here the special function train_test_split which does the work for us.
# By saying test_size=0.2 this means that 20% of rows in the original dataset will be used for testing.
# Which rows will be selected is determined randomly. The random_state argument specifies exactly
# this (random selection of the testing set).
# The other 80% of the dataset will be used for training the model
trainingSetFeatures, testingSetFeatures, trainingSetClass, testingSetClass = train_test_split(features, classVariable, test_size=0.2, random_state = 100)

#We are now ready to train our Decision Tree (aka the model) with our training set
print("\nTraining the model (decision tree)......", sep='', end='')

# We use entropy as our impurity measure and there set the argument criterion="entropy".
# We could also use the gini measure and in this case we would specify the argument criterion="gini"
model = DecisionTreeClassifier(criterion="entropy", max_depth= 3)


# Train now the model. The next line will create the Decision Tree using the entropy measure
# with which we are able to predict the value of the class variable newY.
model_fit = model.fit(trainingSetFeatures, trainingSetClass)
print("done.")

# Visualizing the Decision Tree using a text representation
text_representation = tree.export_text(model)
print(text_representation)

#Displaying the Decision Tree
#
# The graphics
fig = plt.figure(figsize=(20,10))

_ = tree.plot_tree(model,
                   max_depth = 3,
                   feature_names = data.columns,
                   proportion = True,
                   filled=True)

# Set the title
plt.title("Decision tree generated from training data (visualized only at depth=3, not fully)",
          fontsize=16,  color= 'red', fontweight='bold',loc='center')
plt.show()

# We use the .predict() method of the model on the testing set
print("\nUsing testing set to predict class attribute......", sep='', end='')
predictions = model.predict(testingSetFeatures)
print("done.")

# calculate the cinfusion matrix
print("\nCalculating confusion matrix on the testing set......", sep='', end='')
confm = confusion_matrix(testingSetClass, predictions)

# We just tranform the confusion matrix into a DataFrame in order to make
# its display much more convenient i.e. it's done for purely esthetic reasons.
confm = pd.DataFrame(confm)
print("done.")
# Print/show the confusion matrix
print("Confusion matrix:")
print(confm)


# calculate the accuracy of the model
result=model.score(testingSetFeatures, testingSetClass)
print ("\nModel's predictive accuracy is: %.2f%%" % (accuracy_score(testingSetClass,predictions)*100))

