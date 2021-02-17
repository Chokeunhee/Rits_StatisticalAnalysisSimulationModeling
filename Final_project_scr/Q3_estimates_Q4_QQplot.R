
library(fitdistrplus)
library(actuar)
datafile2 <- read.csv("~/Desktop/Stat_final/final/datafile2.csv", header=FALSE)
datafile2 <- datafile2[[1]]
fw <- fitdist(datafile2, "weibull")
fln <- fitdist(datafile2, "lnorm")
fi <- fitdist(datafile2, "invgauss", start = list(mean = 1, shape = 1))

#estimates
fln$estimate[1]
fln$estimate[2]

fw$estimate[1]
fw$estimate[2]

fi$estimate[1]
fi$estimate[2]

#QQplot
qqcomp(fln)
qqcomp(fw)
qqcomp(fi)

