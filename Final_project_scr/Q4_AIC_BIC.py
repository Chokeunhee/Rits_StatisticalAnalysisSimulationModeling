import pandas as pd
import numpy as np

df = pd.read_csv('datafile2.csv',header=None)
df2 = df.values
llf_log = 0
llf_weibull = 0
llf_wald = 0


def aicc(llf, nobs, df_modelwc):
    return -2.0 * llf + 2.0 * df_modelwc * nobs / (nobs - df_modelwc - 1.0)

#https://www.statsmodels.org/stable/_modules/statsmodels/tools/eval_measures.html#aic

def bic(llf, nobs, df_modelwc):
    return -2.0 * llf + np.log(nobs) * df_modelwc

#https://www.statsmodels.org/stable/_modules/statsmodels/tools/eval_measures.html#aic

def loglikelihood_log(x, mu, sigma):
    return np.log((1 / (x * sigma * ((2 * np.pi) ** 0.5))) * np.exp(-(((np.log(x) - mu) ** 2) / (2 * (sigma ** 2)))))

def loglikelihood_wei(x, c, b):
    return np.log((c * (x ** (c - 1))) / ((b ** c) * np.exp((x / b) ** c)))

def loglikelihood_wal(x, eta, lamda):
    return np.log((((lamda) / (2 * np.pi * (x ** 3))) ** 0.5) * np.exp((-lamda) * (((x - eta) ** 2)) / (2 * (eta ** 2) * x)))


for x in df2:
    llf_log += loglikelihood_log(x, 2.42, 1.12)
    llf_weibull += loglikelihood_wei(x, 0.894, 19.631)
    llf_wald += loglikelihood_wal(x, 20.97, 8.32)

print("AIC of lognormal distribution :",aicc(llf_log,len(df2),2))
print("BIC of lognormal distribution :",bic(llf_log,len(df2),2))

print("AIC of weibull distribution   :",aicc(llf_weibull,len(df2),2))
print("BIC of weibull distribution   :",bic(llf_weibull,len(df2),2))

print("AIC of wald distribution      :",aicc(llf_wald,len(df2),2))
print("BIC of wald distribution      :",bic(llf_wald,len(df2),2))

