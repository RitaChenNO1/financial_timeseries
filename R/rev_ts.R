x<-decompose(ts_rev)
plot(x)
#Step 1. diff, found out that 1 diff is ok
par(mfrow=c(2,3))
# diff
plot(y=rev$edw_rev,rev$x,type='o')
plot(diff(rev$edw_rev),type='o')
plot(diff(rev$edw_rev,2),type='o')

#log diff
#plot(y=rev$edw_rev,rev$x,type='o')
plot(log(rev$edw_rev),type='o')
plot(diff(log(rev$edw_rev)),type='o')
plot(diff(log(rev$edw_rev),2),type='o')

#step 2. acf ,pacf, is ARMA(1,1)
# after 1 , then censoring, this is for MA(q) q=1
acf(diff(ts_rev))
# after 1 , then censoring, this is for AR(p) p=1
pacf(diff(ts_rev))

rev.fit<-arima(ts_rev,order=c(1,1,1))
plot.ts(rev.fit$residuals)
#install.packages("forecast")
#library("forecast")
rev.forecast<-forecast(rev.fit,1)
plot.ts(rev.fit$residuals)
summary(rev.forecast)
gap<-1042134404-977700783
gap_pct<-gap/1042134404*100
