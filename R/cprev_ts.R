x<-decompose(ts_cprev)
plot(x)
#Step 1. diff, found out that 1 diff is ok
par(mfrow=c(2,3))
# diff
plot(y=rev$cp_rev,rev$x,type='o')
plot(diff(rev$cp_rev),type='o')
plot(diff(rev$cp_rev,2),type='o')

#log diff
#plot(y=rev$edw_rev,rev$x,type='o')
plot(log(rev$cp_rev),type='o')
plot(diff(log(rev$cp_rev)),type='o')
plot(diff(log(rev$cp_rev),2),type='o')

#step 2. acf ,pacf, is ARMA(1,1)
# after 1 , then censoring, this is for MA(q) q=1
acf(diff(ts_cprev,2))
# after 1 , then censoring, this is for AR(p) p=1
pacf(diff(ts_cprev,2))

rev.fit<-arima(ts_cprev,order=c(0,2,0))
#install.packages("forecast")
#library("forecast")
rev.forecast<-forecast(rev.fit,1)
summary(rev.forecast)
gap<-