import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

#Reading the files
data=pd.read_csv('statistics.dat', delim_whitespace=True)
data=data.loc[~(data["ntopple"]==0)]
#data=data.loc[~(data["ntopple"]==1)]

print(data)

#Plotting the distributions
sns.set_theme()

fig, ax = plt.subplots()
ax.set(xscale="log", yscale="log")

#Scuffed DIY fit of a*s^(-3/5)
x=np.arange(20,10000)
y=2.8*10**4*x**(-3/5)

#Plotting the distributions

sns.histplot(data=data, x="ntopple", ax=ax)
plt.plot(x,y)
plt.savefig("n_s.jpeg")