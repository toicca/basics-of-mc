import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

#Reading the files
data=pd.read_csv('data_vdc.txt', delim_whitespace=True, header=None)
data.columns=["I", "N-power", "Method"]

#Plotting the distributions
sns.set_theme()

sns.lineplot(data=data, x="N-power",y="delI", hue="Method")
plt.savefig("distr.jpeg")
