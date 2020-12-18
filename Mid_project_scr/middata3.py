#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Nov 13 03:21:59 2020

@author: Chokeunhee
"""

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


Data1 = [df['201908jp'],df['201908kr'],df['202010kr'],df['201810kr'],df['201812jp'],df['201912jp']]


fig1 = plt.figure(figsize =(10, 7)) 
ax1 = fig1.add_subplot(111) 
bp1 = ax1.boxplot(Data1, vert = 0) 
ax1.set_yticklabels(['2019/08/Kusatsu','2019/08/Seoul','2020/10/Seoul','2018/10/Seoul','2018/12/Kusatsu','2019/12/Kusatsu'])


plt.show()

plt.xlabel('temperature')








