#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Nov 13 06:06:53 2020

@author: Chokeunhee
"""

import pandas as pd
import numpy as np
import seaborn as sns
import statsmodels.stats.api as sm
import matplotlib.pyplot as plt
from scipy import stats
import statistics as st
import math



df = pd.read_csv (r'weatherdata.csv')

D1 = df['201810kr']
D2 = df['202010kr']

#x = (df['201810kr']-df['201810kr'].mean())/df['201810kr'].std()
#y = (df['202010kr']-df['202010kr'].mean())/df['202010kr'].std()

x = np.array(df['201810kr'])
y = np.array(df['202010kr']) 

X = np.array(x)
Y = np.array(y)

#sns.distplot(X,hist=True,bins=5,label='2018')
#sns.distplot(Y,hist=True,bins=5,label='2020')
#plt.title('Histogram of Average Temperature of October in Seoul')
#plt.ylabel("Percentage")
#plt.xlabel("Temperature")
plt.legend()

#print(sm.ztest(df['202010kr'], x2=df['201810kr'],alternative='larger'))

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

z = (M1-M2) / SEdiff

print('n1:', N1)
print('n2:', N2)
print('Xhibar 1:', M1)
print('Xhibar 2:', M2)
print('Standard Deviation 1:',STD1)
print('Standard Deviation 2:',STD2)
print('Standard Error Difference:',SEdiff)
print('z:', z)
print('Z0.025:', stats.norm.ppf(1-(2*alpha)))
print('p-value :',(1 - stats.norm.cdf(abs(z))))

