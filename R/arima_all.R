#install.packages("forecast")
setwd("C:/WORK/EGI/data")  
#1. apj, prod_ln_id
APJ_rev_prod_ln = read.table("APJ_rev_prod_ln.csv",header=T, sep=",")
#prod_ln_id        year_mth	edw_rev	cp_rev
data_src<-APJ_rev_prod_ln
start_yr<-2015
start_mth<-02
rgn<-"APJ"
rev_type<-"cp_rev"
analysis_dim<-"prod_ln_id"
seasonaltype<-"110"
seasonal_value<-c(1,1,0)
filepath<-"C:/WORK/EGI/data/forecast"
arima_all(data_src,start_yr,start_mth,prod_ln_id,rgn,rev_type,analysis_dim,seasonaltype,seasonal_value,filepath)


#2. ams, prod_ln_id
AMS_rev_prod_ln = read.table("AMS_rev_prod_ln.csv",header=T, sep=",")
data_src<-AMS_rev_prod_ln
start_yr<-2014
start_mth<-02
rgn<-"AMS"
rev_type<-"cp_rev"
analysis_dim<-"prod_ln_id"
seasonaltype<-"000"
seasonal_value<-c(0,0,0)
filepath<-"C:/WORK/EGI/data/forecast"
arima_all(data_src,start_yr,start_mth,prod_ln_id,rgn,rev_type,analysis_dim,seasonaltype,seasonal_value,filepath)

#3. apj, geo_cd
APJ_rev_geo = read.table("APJ_rev_geo.csv",header=T, sep=",")
data_src<-APJ_rev_geo
start_yr<-2015
start_mth<-02
rgn<-"APJ"
rev_type<-"cp_rev"
analysis_dim<-"geo_cd"
seasonaltype<-"001"
seasonal_value<-c(0,0,1)
filepath<-"C:/WORK/EGI/data/forecast"
arima_all(data_src,start_yr,start_mth,prod_ln_id,rgn,rev_type,analysis_dim,seasonaltype,seasonal_value,filepath)


#2. ams, prod_ln_id
AMS_rev_geo = read.table("AMS_rev_geo.csv",header=T, sep=",")
data_src<-AMS_rev_geo
start_yr<-2014
start_mth<-02
rgn<-"AMS"
rev_type<-"cp_rev"
analysis_dim<-"geo_cd"
seasonaltype<-"001"
seasonal_value<-c(0,0,1)
filepath<-"C:/WORK/EGI/data/forecast"
arima_all(data_src,start_yr,start_mth,prod_ln_id,rgn,rev_type,analysis_dim,seasonaltype,seasonal_value,filepath)



