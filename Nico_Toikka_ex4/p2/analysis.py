import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

#This code was varied according to the required plot

#Reading the files
data=pd.read_csv('data_distr2.txt', delim_whitespace=True, header=None)
data.columns=["Count", "x"]

#Plotting the distributions
sns.set_theme()

sns.relplot(data=data,x="x", y="Count", kind="line")
plt.xlim(2,4)
plt.savefig("distr2_H.jpeg")
