import numpy as np
import matplotlib.pyplot as plt

x, y = [], []
for line in open("data.txt", "r"):
    data = [float(i) for i in line.split()]
    x.append(10**data[0])
    y.append(data[1])

plt.semilogx(x, y, label="Average error")
plt.plot(np.linspace(0, 10**7), np.array([1E-4 for i in range(50)]), label="0.0001")
plt.xlabel("Number of needles")
plt.ylabel("Error")
plt.legend()
plt.savefig("errors.png")
plt.show()