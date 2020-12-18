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
x = np.arange(1,len(df)+1)

y1 = df['201908jp']
y2 = df['201908kr']
a1 = 0*x + y1.mean()
a2 = 0*x + y2.mean()

y3 = df['201810kr']
y4 = df['202010kr']
a3 = 0*x + y3.mean()
a4 = 0*x + y4.mean()

y5 = df['201812jp']
y6 = df['201912jp']
a5 = 0*x + y5.mean()
a6 = 0*x + y6.mean()


plt.plot(x,y1,label='Kusatsu',color='green')
plt.plot(x,y2,label='Seoul',color='orange')
plt.plot(x,a1,color='green')
plt.plot(x,a2,color='orange')
plt.title('Average Temperature in August 2019')


#plt.plot(x,y3,label='2018',color='green')
#plt.plot(x,y4,label='2020',color='orange')
#plt.plot(x,a3,color='green')
#plt.plot(x,a4,color='orange')
#plt.title('Average Temperature of October in Seoul')

#plt.plot(x,y5,label='2018',color='green')
#plt.plot(x,y6,label='2019',color='orange')
#plt.plot(x,a5,color='green')
#plt.plot(x,a6,color='orange')
#plt.title('Average Temperature of December in Kusatsu')

plt.grid()
plt.legend()
plt.xlabel("days")
plt.ylabel("temp")
