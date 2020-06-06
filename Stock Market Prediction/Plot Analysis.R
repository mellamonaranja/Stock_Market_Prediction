#1.
library(dplyr)
library(tidyr)
library(ggplot2)
install.packages('vioplot')
library(vioplot)
install.packages('ascii')
library(ascii)
install.packages('corrplot')
library(corrplot)
install.packages('descr')
library(descr)
install.packages('stat_binhex')
library(boot)

PSDS_PATH=file.path('/Users/joohyunyoon/workspace/Stock Market Prediction/')
sp500_px=read.csv(file.path(PSDS_PATH, 'data', 'sp500_px.csv'))
sp500_sym=read.csv(file.path(PSDS_PATH, 'data', 'sp500_sym.csv'), stringsAsFactors = FALSE)
sp500_px <- read.csv(file.path('/Users/joohyunyoon/workspace/Practical_Statistics', 'data', 'sp500_px.csv'))


#Correlation Matrix
etfs=sp500_px[row.names(sp500_px)>'2012-07-01',
              sp500_sym[sp500_sym$sector=='etf','symbol']]
corrplot(cor(etfs),method = 'ellipse') 
##Skewed right side means positive correlation, skewed left side means negative correlation. The color and width mean strengh of correlation.

#Scatter Plot
par(mar=c(4,4,0,1)+.1)
telecom <- sp500_px[, sp500_sym[sp500_sym$sector=="telecommunications_services", 'symbol']]
telecom <- telecom[row.names(telecom)>"2012-07-01", ]
plot(telecom$T, telecom$VZ, xlab="T", ylab="VZ", cex=.8)
abline(h=0, v=0, col="grey")

#QQ Plot(Quantile-Quantile Plot)
nflx=sp500_px[,'NFLX']
nflx=diff(log(nflx[nflx>0]))
qqnorm(nflx)
abline(a=0,b=1,col='orange') 
##There are many outliers although data following ND which imply there is much more probability to observe the outlier than expect.
##Points within SD range are closer to the line.

