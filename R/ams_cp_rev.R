setwd("C:/WORK/EGI/data")  
#1. ams:year_mth        edw_rev	cp_rev
ams_all_rev = read.table("ams_all_rev.csv",header=T, sep=",")
#1. set data
data_src<-ams_all_rev
len<-dim(data_src)[1]
#2. filter data
data1<-data_src[which(data_src$edw_rev>0),]
max_rowdata1<-max(as.integer(row.names(data1)))
data0<-data1[which(as.integer(row.names(data1))<max_rowdata1),]
#len<-dim(data0)[1]
#2.1 trainnig dataset
fit.training<-data0[which(as.integer(row.names(data0))<max_rowdata1-3),]
#2.2 validation dataset
fit.val<-data0[which(as.integer(row.names(data0))>=max_rowdata1-3),]
#3. set time series data
ts_rev <- ts(fit.training$cp_rev, start=c(2014,02), frequency=12)
#3.1 set the source data
rev<-data.frame(as.integer(row.names(fit.training)),as.character(fit.training$year_mth),fit.training$cp_rev)
names(rev)<-c('x','year_mth','rev')

par(mfrow=c(1,2))
plot(stl(ts_rev,s.window="periodic") )
plot(decompose(ts_rev))

#stl
plot(forecast(stl(ts_rev,s.window="periodic")))
plot.forecast(forecast.HoltWinters(HoltWinters(ts_rev,gamma=FALSE),h=19)

predAll1<-build_arima_all(ts_rev,fit.val)
#colnames(predAll)<-c('month','actual','forecast','forecast_lower','forecast_upper','gap','gap_percent','mean_cp_rev')
write.table(predAll1, file = "C:/WORK/EGI/data/forecast_ams_cp_rev_all.csv",quote = TRUE, sep = ",",row.names = FALSE)


#3.2. explore the basic data
par(mfrow=c(2,2))
hist(rev$rev)
#plot(y=rev$rev,x=zlag(rev$rev))
#plot(y=rev$rev,x=lag(rev$rev))
qqnorm(rev$rev)
qqline(rev$rev)
plot(density(rev$rev))
rug(rev$rev)
smoothScatter(rev$x,rev$rev)

#4. explore the trend
m<-decompose(ts_rev)
m$figure
round(m$figure / 10, 2)
plot(m)

#install.packages("TSA")
library("TSA")
par(mfrow=c(1,2))
plot(y=ts_rev,x=as.vector(time(ts_rev)),type='o')
points(y=ts_rev,x=as.vector(time(ts_rev)),pch=as.vector(season(ts_rev)))
plot(y=rev$rev,x=as.vector(time(ts_rev)),type='n')
points(y=rev$rev,x=as.vector(time(ts_rev)),pch=as.vector(season(ts_rev)))

#5. diff to check the Stability
par(mfrow=c(2,3))
plot(y=rev$rev,x=as.vector(time(ts_rev)),type='o')
plot(diff(rev$rev),type='o')
plot(diff(rev$rev,2),type='o')
plot(diff(rev$rev,3),type='o')
plot(diff(rev$rev,4),type='o')
plot(diff(rev$rev,5),type='o')
plot(log(rev$rev),type='o')
plot(diff(log(rev$rev)),type='o')
plot(diff(log(rev$rev),2),type='o')
plot(diff(log(rev$rev),3),type='o')
plot(diff(log(rev$rev),4),type='o')
plot(diff(log(rev$rev),5),type='o')

par(mfrow=c(1,2))
#7. acf ,pacf, is ARMA(1,1)  
# after 1 , then censoring, this is for AR(p) p=1
#pacf(diff(ts_rev),lag.max=20,plot=FALSE)
pacf(diff(ts_rev),lag.max=25)
# after 1 , then censoring, this is for MA(q) q=1
acf(diff(ts_rev),lag.max=25)

library("forecast")
#with seasonal
test<-ets(ts_rev)
test
summary(test)
predict(test,3)
auto.arima(ts_rev,seasonal=TRUE,ic=c("aicc", "aic", "bic"), stepwise=TRUE, trace=TRUE)

rev.fit<-arima(ts_rev,order=c(0,0,1), seasonal=list(order=c(0,1,1), period=4))
plot.ts(rev.fit$residuals)
#summary(rev.fit)
rev.fit
rev.fit<-arima(ts_rev,order=c(0,0,0))
plot.ts(rev.fit$residuals)
summary(rev.fit)
rev.fit

rev.fit$aic
i<-1
rev.fit<-arima(ts_rev,order=pdq[i,])
plot.ts(rev.fit$residuals)
summary(rev.fit)
rev.fit

par(mfrow=c(1,1))
#install.packages("forecast")
#library("forecast")
forecast.fit <- forecast.Arima(rev.fit,h=3,level=c(99.9))
summary(forecast.fit)  
plot.forecast(forecast.fit)
#get the forcase of 2016.may
f_may<-forecast.fit$mean[1]
f_june<-forecast.fit$mean[2]

#f_may<-forecast.fit$lower[1]
#f_june<-forecast.fit$lower[2]

a_may<-fit.val$cp_rev[1]
a_june<-fit.val$cp_rev[2]
gap_may<-a_may-f_may
gap_pct_may<-gap_may/a_may*100
gap_pct_may
gap_june<-a_june-f_june
gap_pct_june<-gap_june/a_june*100
gap_pct_june
mean<-mean(fit.training$cp_rev)
print(mean)
forecast1<-cbind(fit.val$year_mth[1],a_may,f_may,forecast.fit$lower[1],forecast.fit$upper[1],gap_may,gap_pct_may,mean)
forecast2<-cbind(fit.val$year_mth[2],a_june,f_june,forecast.fit$lower[2],forecast.fit$upper[2],gap_june,gap_pct_june,mean)
fall<-rbind(forecast1,forecast2)
colnames(fall)<-c('month','actual','forecast','forecast_lower','forecast_upper','gap','gap_percent','mean of previous')
write.table(fall, file = "C:/WORK/EGI/data/forecast_ams_cp_rev011_110.csv",quote = TRUE, sep = ",",row.names = FALSE)
