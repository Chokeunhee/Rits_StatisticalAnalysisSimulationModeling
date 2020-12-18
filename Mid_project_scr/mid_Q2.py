#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Nov  8 00:37:48 2020

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

sns.distplot(X,hist=True,bins=5,label='2018')
sns.distplot(Y,hist=True,bins=5,label='2020')
plt.title('Histogram of Average Temperature of October in Seoul')
plt.ylabel("Percentage")
plt.xlabel("Temperature")
plt.legend()

#print(sm.ztest(df['202010kr'], x2=df['201810kr'],alternative='larger'))

N = len(D1)
D = D1-D2
Dbar = D.mean()
STD = st.stdev(D)
alpha = 0.025
SEmean = STD / math.sqrt(N)
z = Dbar / SEmean

#sns.distplot(D,hist=True,label='D')

print('n:', N)
print('d bar:', Dbar)
print('Standard Deviation:',STD)
print('Standard Error mean:',SEmean)
print('z:', z)
print('Z0.05:',stats.norm.ppf(1-(2*alpha)))
print('Z0.025:',stats.norm.ppf(1-(alpha)))
print('p-value:', 2 * (1 - stats.norm.cdf(abs(z))))
print('p-value:', (1 - stats.norm.cdf(abs(z))))




