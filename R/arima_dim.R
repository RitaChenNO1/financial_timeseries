arima_all <- function(data_src,start_yr,start_mth,prod_ln_id,rgn,rev_type,analysis_dim,seasonaltype,seasonal_value,filepath)
{
        #find out rev, 0 or <0,remove the prod_ln_id
        prod_ln_id_negative<-data_src[which(data_src$cp_rev<=0 | data_src$edw_rev<=0 ),]
        plid_negative<-prod_ln_id_negative[,1] 
        prod_ln_id_negative_unique<-unique(plid_negative[!duplicated(plid_negative)])
        for(p in prod_ln_id_negative_unique){
                data_src<-data_src[which(data_src[,1] !=p),]
        }
        #get the meaningful prod_ln_id
        #plid<-data_src$prod_ln_id  
        plid<-data_src[,1] 
        #data_src[which(data_src[,1]=="1U" ),]
        prod_ln_id_unique<-unique(plid[!duplicated(plid)])
        
        prod_ln_id_NO<-length(prod_ln_id_unique)
        predAll<-NULL
        #prod_ln_id<-prod_ln_id_unique[1]
        for (prod_ln_id in prod_ln_id_unique ) {
                print(prod_ln_id)
                predAll1<-arima_dim(data_src,start_yr,start_mth,prod_ln_id,rgn,rev_type,analysis_dim,seasonaltype,seasonal_value,filepath)
                fileFullPath<-paste(filepath, rgn, rev_type,analysis_dim,seasonaltype,".csv", sep = "_")
                predAll<-rbind(predAll,predAll1)
        }
        colnames(predAll)<-c("rgn","rev_type","analysis_dim","prod_ln_id","seasonaltype",'month','actual','forecast','forecast_lower','forecast_upper','gap','gap_percent','mean_cp_rev','p','d','q')
        write.table(predAll, file = fileFullPath,quote = TRUE, sep = ",",row.names = FALSE)
}

arima_dim <- function(data_src,start_yr,start_mth,prod_ln_id,rgn,rev_type,analysis_dim,seasonaltype,seasonal_value,filepath){
        Start_yr_mth<-start_yr*100+start_mth        
        #2. filter data
        data1<-data_src[which(data_src[,1]==prod_ln_id ),]
        #print(data1)
        max_rowdata1<-max(as.integer(row.names(data1)))
        data0<-data1[which(as.integer(row.names(data1))<max_rowdata1),]
        data0<-data0[which(as.integer(data0$year_mth)>=Start_yr_mth),]
        #len<-dim(data0)[1]
        #2.1 trainnig dataset
        fit.training<-data0[which(as.integer(row.names(data0))<max_rowdata1-3),]
        #summary(fit.training)
        
        print(dim(fit.training)[1])
        if(dim(fit.training)[1]<4)
        {
                return(NULL)               
        }else{
        #2.2 validation dataset
        fit.val<-data0[which(as.integer(row.names(data0))>=max_rowdata1-3),]
        #3. set time series data
        if(fit.training$year_mth[1]>Start_yr_mth){
                start_yr<-fit.training$year_mth[1]/100
                start_mth<-fit.training$year_mth[1]%%100
        }
        ts_rev <- ts(fit.training$cp_rev, start=c(start_yr,start_mth), frequency=12)
        #3.1 set the source data
        #rev<-data.frame(as.character(fit.training$year_mth),fit.training$cp_rev)
        rev<-data.frame(as.character(fit.training$year_mth),fit.training$cp_rev)
        names(rev)<-c('year_mth','rev')
        
        #auto generate the p,d,q

        library("forecast")
        #dummy_val = seasonaldummy(ts_rev) 
        #dummy_val<-fourier(ts_rev, K=3)
        #print(dummy_val)
        #auto<-auto.arima(ts_rev,seasonal=TRUE,ic=c("aicc", "aic", "bic"), stepwise=TRUE, trace=TRUE)
        auto<-auto.arima(ts_rev, trace=T)
        pdq.arima<-arimaorder(auto)        
        #dummy_val = seasonaldummy(ts_rev)   
        if(seasonaltype=="000")
        {
                rev.fit<-arima(ts_rev,order=c(pdq.arima[1],pdq.arima[2],pdq.arima[3]), method="ML")
                
        }
        else{
                rev.fit<-arima(ts_rev,order=c(pdq.arima[1],pdq.arima[2],pdq.arima[3]),seasonal=list(order=c(seasonal_value[1],seasonal_value[2],seasonal_value[3]), period=12), method="ML")
        }
        
        
        forecast.fit <- forecast.Arima(rev.fit,h=3,level=c(99.5))
        f_may<-forecast.fit$mean[1]
        f_june<-forecast.fit$mean[2]
        f_july<-forecast.fit$mean[3]
        a_may<-fit.val$cp_rev[1]
        a_june<-fit.val$cp_rev[2]
        a_july<-fit.val$cp_rev[3]
        gap_may<-a_may-f_may
        gap_pct_may<-gap_may/a_may*100
        gap_june<-a_june-f_june
        gap_pct_june<-gap_june/a_june*100
        gap_july<-a_july-f_july
        gap_pct_july<-gap_july/a_july*100
        
        #get the average of cp_rev
        mean<-mean(fit.training$cp_rev)
        print(mean)
        
        rev
        forecast1=cbind(rgn,rev_type,analysis_dim,prod_ln_id,seasonaltype,fit.val$year_mth[1],a_may,f_may,forecast.fit$lower[1],forecast.fit$upper[1],gap_may,gap_pct_may,mean,pdq.arima[1],pdq.arima[2],pdq.arima[3])
        forecast2=cbind(rgn,rev_type,analysis_dim,prod_ln_id,seasonaltype,fit.val$year_mth[2],a_june,f_june,forecast.fit$lower[2],forecast.fit$upper[2],gap_june,gap_pct_june,mean,pdq.arima[1],pdq.arima[2],pdq.arima[3])
        forecast3=cbind(rgn,rev_type,analysis_dim,prod_ln_id,seasonaltype,fit.val$year_mth[3],a_july,f_july,forecast.fit$lower[3],forecast.fit$upper[3],gap_july,gap_pct_july,mean,pdq.arima[1],pdq.arima[2],pdq.arima[3])
        fall<-rbind(as.matrix(forecast1),as.matrix(forecast2),as.matrix(forecast3))
        print(fall)        
        return(fall)
        }
}

