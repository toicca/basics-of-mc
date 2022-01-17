import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

#This code was changed as needed

#Reading the files
#data=pd.read_csv('data_max.txt', delim_whitespace=True, header=None)
#data.columns=["RMS", "E_MAX"]

#Plotting the distributions
#sns.set_theme()

#sns.lineplot(data=data,x="E_MAX", y="RMS")
#plt.savefig("distr1.jpeg")



data=pd.read_csv('data_points.txt', delim_whitespace=True, header=None)
data.columns=["RMS", "N_POWER"]

fig, ax = plt.subplots()
ax.set(xscale="log", yscale="log")

#Plotting the distributions
sns.set_theme()

sns.regplot(data=data, x="N_POWER", y="RMS", ax=ax, robust=True, ci=None, fit_reg=False)
plt.ylim(97e-4,11e-3)
plt.savefig("distr2.jpeg")