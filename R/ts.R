setwd("C:/WORK/EGI/sql")
#install.packages("xlsx")
library("xlsx")
#step 1. the result of sql file month_rev.sql, and save as xlsx file, then load to R for analyzing
data_src<-read.xlsx(file="ts.xlsx",header=T,sheetIndex=1)
len<-dim(data_src)[1]
data_src$row.names
#step 2. 2012,2013, the rev is 0, so ignore them
data0<-data_src[which(data_src$edw_rev>0),]
data<-data0[which(row.names(data0)<len-2),]
#step 3. conbine the year and month, then plot
rev<-data.frame(as.integer(row.names(data))-18,as.character(data$year*100+data$month),data$edw_rev,data$cp_rev)
names(rev)<-c('x','year_mth','edw_rev','cp_rev')
ts_rev <- ts(rev$edw_rev, start=1, frequency=12)
ts_cprev <- ts(rev$cp_rev, start=1, frequency=12)


plot(rev$edw_rev~rev$x)
plot(y=rev$edw_rev,x=rev$year_mth)

#check correlation of the lag of rev
#install.packages("TSA")
library("TSA")
plot(y=rev$edw_rev,x=zlag(rev$edw_rev))
plot(y=rev$edw_rev,x=lag(rev$edw_rev))
qqnorm(rev$edw_rev)
qqline(rev$edw_rev)
plot(density(rev$edw_rev))
rug(rev$edw_rev)
smoothScatter(rev$x,rev$edw_rev)
#lmfit <- lm(rev$edw_rev~rev$x)
lmfit <- lm(ts_rev~time(ts_rev))
abline(lmfit)
summary(lmfit)

#check the seasons, show the month name
plot(y=rstudent(lmfit),x=as.vector(time(ts_rev)),type='o')
points(y=rstudent(lmfit),x=as.vector(time(ts_rev)),pch=as.vector(season(ts_rev)))
#M changed too much, other are normal
plot(y=rstudent(lmfit),x=as.vector(fitted(lmfit)),type='n')
points(y=rstudent(lmfit),x=as.vector(fitted(lmfit)),pch=as.vector(season(ts_rev)))
# the residume
hist(rstudent(lmfit))
#some points are far from the nomal line
qqnorm(rstudent(lmfit))
qqline(rstudent(lmfit)) 
acf(rstudent(lmfit))

#move average, zlag to set MA(q)
par(mfrow=c(2,3))
plot(y=rev$edw_rev,rev$x)
plot(y=rev$edw_rev,x=zlag(rev$edw_rev))
plot(y=rev$edw_rev,x=zlag(rev$edw_rev,2))
plot(y=rev$edw_rev,x=zlag(rev$edw_rev,3))
plot(y=rev$edw_rev,x=zlag(rev$edw_rev,4))

# diff
plot(y=rev$edw_rev,rev$x,type='o')
plot(diff(rev$edw_rev),type='o')
plot(diff(rev$edw_rev,2),type='o')

#log
#plot(y=rev$edw_rev,rev$x,type='o')
plot(log(rev$edw_rev),type='o')
plot(diff(log(rev$edw_rev)),type='o')
plot(diff(log(rev$edw_rev),2),type='o')

par(mfrow=c(3,2))
#acf
acf(ts_rev)
pacf(ts_rev)
#eacf(ts_rev)
acf(diff(ts_rev))
pacf(diff(ts_rev))
acf(diff(ts_rev),ci.type='ma',xaxp=c(0,18,9)) 
pacf(diff(ts_rev),ci.type='ma',xaxp=c(0,18,9))

#Loglikehood
win.graph(width=3,height=3,pointsize=8) 
BoxCox.ar(ts_rev)

#BIC
res<-armasubsets(y=diff(ts_rev),nar=7,nma=7,y.name='test',ar.method='ols')
plot(res)
f
#s
max<-1
ar(ts_rev,order.max=max,AIC=F,method='yw')
ar(ts_rev,order.max=max,AIC=F,method='ols')
ar(ts_rev,order.max=max,AIC=F,method='mle')

estimate.ma1.mon(ts_rev)

arima(ts_rev,order=c(1,0,1),method='CSS')
arima(ts_rev,order=c(1,0,1),method='ML')
arima(ts_rev,order=c(1,0,0))

plot(rev$cp_rev~rev$x)
lmfit <- lm(rev$cp_rev~rev$x)
abline(lmfit)
qqnorm(rev$cp_rev)
qqline(rev$cp_rev)
plot(density(rev$cp_rev))
rug(rev$cp_rev)
smoothScatter(rev$x,rev$cp_rev)


#step 4. diff for them

