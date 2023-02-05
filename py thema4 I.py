# Import some required libraries
import pickle
import unittest
import pandas as pd
import numpy as np
from scipy import stats
#from sklearn.utils._testing import assert_equal
from kmodes.kmodes import KModes
from kmodes.util.dissim import ng_dissim
from matplotlib import pyplot as plt

# Read the data

movies = pd.read_csv("/Users/vasileioskogkos/Documents/MSc ASssignments/Big Data/3rdAssignBigData/3hErgasia2022-2023/movies.csv", sep=",", header=0)
ratings = pd.read_csv("/Users/vasileioskogkos/Documents/MSc ASssignments/Big Data/3rdAssignBigData/3hErgasia2022-2023/ratings.csv", sep=",", header=0)

rating=ratings.iloc[:,2]
names=movies.iloc[:,1]
movies2 = movies.iloc[:,2:22]
cost = []
for k in range(2, 100):
   kmodes= KModes(n_clusters=k, n_init=1, verbose=1)
   #movies["clusters"] = kmodes.labels_
   #print(movies["clusters"])
   kmodes.fit_predict(movies2)
   cost.append(kmodes.cost_)
   print(k)
   #print(cost.append(kmodes.cost_))
   
km = KModes(n_clusters=10, n_init=1, verbose=1)
clusters = km.fit_predict(movies2)
cl=km.cluster_centroids_
#Print the cluster centroids
print(km.cluster_centroids_)
   
df=rating.groupby(names).agg(pd.Series.mean)
df = df.to_frame().reset_index()
df = df.rename(columns= {0: 'list'})
df.index.name = 'index'
df_col=['title','mean']
df.columns=df_col
movies['clusters']=clusters
merged = pd.merge(movies,ratings, how="left", on="movieId")
userID=merged.loc[merged.userId==5]
merged2 = pd.merge(userID,df, how ="left", on = "title")
usrate2=merged.loc[:,['clusters','rating']]
usrate2.groupby('clusters')['rating'].mean()
usrate3=usrate2.groupby('clusters')['rating'].mean()
usrate3 = usrate3.to_frame().reset_index()
usrate3 = usrate3.rename(columns= {0: 'list'})
usrate3.index.name = 'index'
usrate3_col=['clusters','rating']
usrate3.columns=usrate3_col
print(usrate3)
usrate=usrate3.loc[usrate3.rating>=3.5]
print(usrate)
merge6 = pd.merge(merged2,usrate, how ="left", on = "clusters")
print(merge6)
usrate=merge6.loc[merge6.rating_y>=3.5]
prt2=usrate.loc[:,['clusters']]
rec = pd.merge(merged,prt2, how="left", on="clusters")
rec=merged.loc[merged.userId!=5]
userID2=merged.loc[merged.userId!=5]
merged69 = pd.merge(merged,df, how ="left", on = "title")
userID69= pd.merge(df,merged, how="left", on="title")
userID69=userID69.loc[:,['title','mean','clusters','rating']]
test=userID69.groupby('title').aggregate('mean')
if all(usrate3.rating < 3.5) :
  print("Sorry, no recommendations for you!")
else:
  m=pd.merge(test,usrate,how="inner",on="clusters")
  m8=m.groupby('title').agg(pd.Series.mean)
  m8.groupby(['clusters']).apply(lambda m8: m8.nlargest(2, columns=['mean_y']))    
  recomm=m8.groupby(['clusters']).apply(lambda m8: m8.nlargest(2, columns=['mean_y']))      
  reccomandtions=recomm.iloc[:,1]    
  print("You may also like the following movies!",reccomandtions) 
