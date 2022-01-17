import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

#Reading the files and combining the data for plotting
lcg_data=pd.read_csv('data_lcg.txt', delim_whitespace=True, header=None)
lcg_data.columns=["k", "C(k)"]
lcg_data=lcg_data.assign(RNG="LCG")

pm_data=pd.read_csv('data_pm.txt', delim_whitespace=True, header=None)
pm_data.columns=["k", "C(k)"]
pm_data=pm_data.assign(RNG="Park-Miller")

shuffle_data=pd.read_csv('data_lcg_shuffle.txt', delim_whitespace=True, header=None)
shuffle_data.columns=["k", "C(k)"]
shuffle_data=shuffle_data.assign(RNG="LCG with shuffle")

data = pd.concat([lcg_data, pm_data, shuffle_data])

#Plotting
sns.set_theme(style="darkgrid")
sns.relplot(data=data, x="k", y="C(k)", col="RNG", kind="line")
plt.savefig("fig.png")

sns.relplot(data=data, x="k", y="C(k)", col="RNG", kind="line")
plt.xlim(1000,5000)
plt.savefig("fig_lim.png")

#Show for quickly checking results
#plt.show()