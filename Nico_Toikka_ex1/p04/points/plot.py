import numpy as np
import matplotlib.pyplot as plt
#100 points
lcg_x, lcg_y = [], []
for line in open("data_lcg100.txt","r"):
    data=[float(i) for i in line.split()]
    lcg_x.append(data[0])
    lcg_y.append(data[1])

pm_x, pm_y = [], []
for line in open("data_pm100.txt","r"):
    data=[float(i) for i in line.split()]
    pm_x.append(data[0])
    pm_y.append(data[1])

mt_x, mt_y = [], []
for line in open("data_mt100.txt","r"):
    data=[float(i) for i in line.split()]
    mt_x.append(data[0])
    mt_y.append(data[1])

#Plots
fig, (ax1,ax2,ax3) = plt.subplots(1, 3)
fig.suptitle('RNG 100 points')
ax1.scatter(lcg_x,lcg_y)
ax1.set_title('LCG')
ax2.scatter(pm_x,pm_y)
ax2.set_title('Park-Miller')
ax3.scatter(mt_x,mt_y)
ax3.set_title('Mersenne twister')
plt.show()
fig.savefig("100points.png")

#10000 points
lcg_x, lcg_y = [], []
for line in open("data_lcg10000.txt","r"):
    data=[float(i) for i in line.split()]
    lcg_x.append(data[0])
    lcg_y.append(data[1])

pm_x, pm_y = [], []
for line in open("data_pm10000.txt","r"):
    data=[float(i) for i in line.split()]
    pm_x.append(data[0])
    pm_y.append(data[1])

mt_x, mt_y = [], []
for line in open("data_mt10000.txt","r"):
    data=[float(i) for i in line.split()]
    mt_x.append(data[0])
    mt_y.append(data[1])


fig, (ax1,ax2,ax3) = plt.subplots(1, 3)
fig.suptitle('RNG 10000 points')
ax1.scatter(lcg_x,lcg_y)
ax1.set_title('LCG')
ax2.scatter(pm_x,pm_y)
ax2.set_title('Park-Miller')
ax3.scatter(mt_x,mt_y)
ax3.set_title('Mersenne twister')
plt.show()
fig.savefig("10000points.png")

#Limited region, seed 1
lcg_x, lcg_y = [], []
for line in open("data_lcg_region1.txt","r"):
    data=[float(i) for i in line.split()]
    lcg_x.append(data[0])
    lcg_y.append(data[1])

pm_x, pm_y = [], []
for line in open("data_pm_region1.txt","r"):
    data=[float(i) for i in line.split()]
    pm_x.append(data[0])
    pm_y.append(data[1])

mt_x, mt_y = [], []
for line in open("data_mt_region1.txt","r"):
    data=[float(i) for i in line.split()]
    mt_x.append(data[0])
    mt_y.append(data[1])


fig, (ax1,ax2,ax3) = plt.subplots(1, 3)
fig.suptitle('RNG 1000 points between -0.01 and 0.01. Seed 1')
ax1.scatter(lcg_x,lcg_y)
ax1.set_title('LCG')
ax2.scatter(pm_x,pm_y)
ax2.set_title('Park-Miller')
ax3.scatter(mt_x,mt_y)
ax3.set_title('Mersenne twister')
plt.show()
fig.savefig("1000points_region1.png")

#Limited region, seed 2
lcg_x, lcg_y = [], []
for line in open("data_lcg_region2.txt","r"):
    data=[float(i) for i in line.split()]
    lcg_x.append(data[0])
    lcg_y.append(data[1])

pm_x, pm_y = [], []
for line in open("data_pm_region2.txt","r"):
    data=[float(i) for i in line.split()]
    pm_x.append(data[0])
    pm_y.append(data[1])

mt_x, mt_y = [], []
for line in open("data_mt_region2.txt","r"):
    data=[float(i) for i in line.split()]
    mt_x.append(data[0])
    mt_y.append(data[1])


fig, (ax1,ax2,ax3) = plt.subplots(1, 3)
fig.suptitle('RNG 1000 points between -0.01 and 0.01. Seed 2')
ax1.scatter(lcg_x,lcg_y)
ax1.set_title('LCG')
ax2.scatter(pm_x,pm_y)
ax2.set_title('Park-Miller')
ax3.scatter(mt_x,mt_y)
ax3.set_title('Mersenne twister')
plt.show()
fig.savefig("1000points_region2.png")