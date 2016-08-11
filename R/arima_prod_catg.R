###1.automaticaly automatically run arima with specified parameters(pdq PDQ period)
###2. predict validation dataset, and compare actual with forecast to get the gap & gap%
###@ts_rev: the time series data, @fit.val: the validation dataset
###Author: Rita Chen 2016/8/10
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
                #predAll<-rbind(predAll,predAll1)
                #every prod catg nm is a file
                colnames(predAll)<-c("rgn","rev_type","analysis_dim","prod_ln_id","seasonaltype",'month','actual','forecast','forecast_lower','forecast_upper','gap','gap_percent','mean_cp_rev','p','d','q')
                write.table(predAll, file = fileFullPath,quote = TRUE, sep = ",",row.names = FALSE)
        }
        
}

###1.sampling data to trainging dataset and validation dataset
###2. build arima model
###@ts_rev: the time series data, @fit.val: the validation dataset
###Author: Rita Chen 2016/8/10
sample_buildArima <- function(data_src,start_yr,start_mth,selectedid){
        Start_yr_mth<-start_yr*100+start_mth        
        #2. filter data
        data1<-data_src[which(data_src[,1]==selectedid ),]
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
                fall<-build_arima_all(ts_rev,fit.val)     
                return(fall)
        }
}