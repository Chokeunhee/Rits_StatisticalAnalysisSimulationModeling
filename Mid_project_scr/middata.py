#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Nov 12 15:43:40 2020

@author: Chokeunhee
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt



df = pd.read_csv (r'weatherdata.csv')
y1 = df['201908jp']
y2 = df['201908kr']

y3 = df['201810kr']
y4 = df['202010kr']

y5 = df['201812jp']
y6 = df['201912jp']

x = np.arange(1,len(df)+1)

plt.plot(x,y1)
plt.plot(x,y2)

#plt.plot(x,y3)
#plt.plot(x,y4)

#plt.plot(x,y5)
#plt.plot(x,y6)

plt.xlabel("days")
plt.ylabel("temp")
