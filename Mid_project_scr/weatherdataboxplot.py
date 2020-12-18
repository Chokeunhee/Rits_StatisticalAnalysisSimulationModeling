#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Nov  8 20:47:29 2020

@author: Chokeunhee
"""
import random
import matplotlib.pyplot as plt
import numpy as np

df = pd.read_csv (r'weatherdata.csv')


Data1 = [df['201908jp'],df['201908kr']]

Data2 = [df['202010kr'],df['201810kr']]

Data3 = [df['201812jp'],df['201912jp']]


fig1 = plt.figure(figsize =(10, 7)) 
ax1 = fig1.add_subplot(311) 
bp1 = ax1.boxplot(Data1, vert = 0) 
ax1.set_yticklabels(['2019/08/Kusatsu','2019/08/Seoul'])

plt.title ('Weather Data')

ax2 = fig1.add_subplot(312) 
bp2 = ax2.boxplot(Data2, vert = 0) 
ax2.set_yticklabels(['2020/10/Seoul','2018/10/Seoul'])

ax3 = fig1.add_subplot(313) 
bp3 = ax3.boxplot(Data3, vert = 0) 
ax3.set_yticklabels(['2018/12/Kusatsu','2019/12/Kusatsu'])

plt.show()

plt.xlabel('temperature')








