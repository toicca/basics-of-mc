import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

#Reading the files
lcg_data=pd.read_csv('data_lcg.txt',header=None)
lcg_data.columns=["Chi-squared LCG"]

pm_data=pd.read_csv('data_pm.txt',header=None)
pm_data.columns=["Chi-squared Park-Miller"]

mt_data=pd.read_csv('data_mt.txt',header=None)
mt_data.columns=["Chi-squared Mersenne Twister"]

qcg_data=pd.read_csv('data_qcg.txt',header=None)
qcg_data.columns=["Chi-squared QCG"]

#Print the stats
print(f"LCG mean: {float(lcg_data.mean(axis=0))}, median {float(lcg_data.median(axis=0))}")
print()
print(f"PM mean: {float(pm_data.mean(axis=0))}, median {float(pm_data.median(axis=0))}")
print()
print(f"MT mean: {float(mt_data.mean(axis=0))}, median {float(mt_data.median(axis=0))}")
print()
print(f"QCG mean: {float(qcg_data.mean(axis=0))}, median {float(qcg_data.median(axis=0))}")
print()

#Plotting the distributions
sns.set_theme()

sns.displot(lcg_data, kde=True)
plt.savefig("lcg_fig.png")

sns.displot(pm_data, kde=True)
plt.savefig("pm_fig.png")

sns.displot(mt_data, kde=True)
plt.savefig("mt_fig.png")

sns.displot(qcg_data)
plt.savefig("qcg_fig.png")
