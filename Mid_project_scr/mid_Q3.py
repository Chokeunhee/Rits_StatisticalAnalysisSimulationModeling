#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Nov  8 22:08:47 2020

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

D1 = df['201812jp']
D2 = df['201912jp']

#x = (df['201812jp']-df['201812jp'].mean())/df['201812jp'].std()
#y = (df['201912jp']-df['201912jp'].mean())/df['201912jp'].std()

x = np.array(df['201812jp'])
y = np.array(df['201912jp']) 

X = np.array(x)
Y = np.array(y)

#sns.distplot(X,hist=True,bins=5,label='2018')
#sns.distplot(Y,hist=True,bins=5,label='2019')
#plt.title('Histogram of Average Temperature of December in Kusatsu')
#plt.ylabel("Percentage")
#plt.xlabel("Temperature")
#plt.legend()

print(sm.ztest(df['201812jp'], x2=df['201912jp'], value=0,alternative='two-sided'))

N = len(D1)
D = D2-D1
Dbar = D.mean()
STD = st.stdev(D)
alpha = 0.025
SEmean = STD / math.sqrt(N)
z = Dbar / SEmean

print('n:', N)
print('d bar:', Dbar)
print('Standard Deviation:',STD)
print('Standard Error mean:',SEmean)
print('z:', z)
print('Z0.025:',stats.norm.ppf(1-(alpha)))
print('p-value:', 2 * (1 - stats.norm.cdf(abs(z))))