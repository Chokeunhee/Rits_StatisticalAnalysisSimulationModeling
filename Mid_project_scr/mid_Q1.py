#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Nov  6 17:07:33 2020

@author: Chokeunhee
"""

import pandas as pd
import numpy as np
import seaborn as sns
import statsmodels.stats.api as sm
import matplotlib.pyplot as plt
import math
from scipy import stats


df = pd.read_csv (r'weatherdata.csv')

D1 = df['201908jp']
D2 = df['201908kr']

print(df.describe())

x = np.array(df['201908jp'])
y = np.array(df['201908kr']) 


X = np.array(x)
Y = np.array(y)

sns.distplot(X,hist=True,bins=5,label='Kusatsu')
sns.distplot(Y,hist=True,bins=5,label='Seoul')
plt.title('Histogram of Average Temperature in August 2019')
plt.ylabel("Percentage")
plt.xlabel("Temperature")
plt.legend()

#print(sm.ztest(df['201908jp'], x2=df['201908kr']))
# for the above formula when two data set it is considered independent

#independent 2 sample z test

alpha = 0.025

N1 = len(D1)
N2 = len(D2)

M1 = D1.mean()
M2 = D2.mean()

SE1 = stats.sem(D1)
SE2 = stats.sem(D2)

STD1 = D1.std()
STD2 = D2.std()

SEdiff = math.sqrt(SE1**2 + SE2**2)

z = (M2-M1) / SEdiff

print('n1:', N1)
print('n2:', N2)
print('Xhibar 1:', M1)
print('Xhibar 2:', M2)
print('Standard Deviation 1:',STD1)
print('Standard Deviation 2:',STD2)
print('Standard Error Difference:',SEdiff)
print('z:', z)
print('Z0.025:', stats.norm.ppf(1-(alpha)))
print('p-value :',2 * (1 - stats.norm.cdf(abs(z))))











