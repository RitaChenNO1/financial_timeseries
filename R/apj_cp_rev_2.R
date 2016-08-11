setwd("C:/WORK/EGI/data")  
#1. ams:year_mth        edw_rev        cp_rev
ams_all_rev = read.table("apj_all_rev.csv",header=T, sep=",")
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

predAll1<-build_arima_all(ts_rev,fit.val)
#colnames(predAll)<-c('month','actual','forecast','forecast_lower','forecast_upper','gap','gap_percent','mean_cp_rev')
write.table(predAll1, file = "C:/WORK/EGI/data/forecast_apj_cp_rev_all.csv",quote = TRUE, sep = ",",row.names = FALSE)
