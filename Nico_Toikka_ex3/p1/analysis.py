import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

#Reading the files
data=pd.read_csv('data.txt', delim_whitespace=True, header=None)
data.columns=["Value", "Method"]

#Plotting the distributions
sns.set_theme()

sns.displot(data, x="Value", hue="Method", kde=True)
plt.savefig("distr.jpeg")
