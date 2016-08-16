###
#*************1.load data*************
setwd("C:/WORK/EGI/data") 
ams_all_rev = read.table("AMS_rev_PRODCATG_NM.csv",header=T, sep=",")
###
#*************2.get the initial defined R functions(which is automatically run arima with specified parameters(pdq PDQ period))*************
setwd("C:/WORK/EGI/Finance_project") 
source("build_arima_all.R")
source("arima_prod_catg.R")
arima_all(ams_all_rev,2014,2,"C:/WORK/EGI/data/","AMS","CP")

