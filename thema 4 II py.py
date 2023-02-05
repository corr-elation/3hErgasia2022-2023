
import pandas as pd
import sklearn
from sklearn import preprocessing
from sklearn.cluster import AgglomerativeClustering
import numpy as np
import matplotlib.pyplot as plt 
from scipy.cluster.hierarchy import dendrogram, linkage
from scipy.spatial.distance import pdist
from scipy.cluster.hierarchy import linkage, dendrogram
from scipy.cluster.hierarchy import fcluster
import scipy.cluster.hierarchy as shc
from scipy.cluster import hierarchy

euro=pd.read_csv("\europe.csv",sep = ',')
print(euro)
summary = euro.describe()
print(summary)
def normalize(df):
    result = df.copy()
    for feature_name in df.columns:
        max_value = df[feature_name].max()
        min_value = df[feature_name].min()
        result[feature_name] = (df[feature_name] - min_value) / (max_value - min_value)
    return result

euroNum = euro.drop(['Country'], axis=1)
euroNum=normalize(euroNum)
euroNum['Country'] =euro['Country']
euroNum = euroNum[['Country', 'Area', 'GDP', 'Inflation', 'Life.expect', 'Military', 'Pop.growth', 'Unemployment']]
summary = euroNum.describe()
print(euroNum)
Z = hierarchy.linkage(euroNum.iloc[:,1:8], 'complete')
f, dax = plt.subplots(figsize=(14, 14))
dax.set_xlabel('MyData')
dax.set_ylabel('Distance')
countries = euroNum['Country']

# Create dendrogram
dn = hierarchy.dendrogram(Z,  ax=dax, leaf_label_func=lambda i: countries[i], orientation='top' )

# Display dendrogram
plt.show()
