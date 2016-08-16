###1.automaticaly automatically run arima with specified parameters(pdq PDQ period)
###2. predict validation dataset, and compare actual with forecast to get the gap & gap%
###@ts_rev: the time series data, @fit.val: the validation dataset
###Author: Rita Chen 2016/8/10
arima_all <- function(data_src,start_yr,start_mth,filepath,AMS_APJ_EMEA,rev_type)
{        
        all_id<-unique(data_src[,1])        
        predAll<-NULL
        #prod_ln_id<-prod_ln_id_unique[1]
        for (id in all_id ) {
                print(id)
                predAll1<-sample_buildArima(data_src,start_yr,start_mth,id)
                fileFullPath<-paste(filepath,AMS_APJ_EMEA,rev_type,id,".csv", sep = "_")
                #predAll<-rbind(predAll,predAll1)
                #every prod catg nm is a file
                #colnames(predAll)<-c("rgn","rev_type","analysis_dim","prod_ln_id","seasonaltype",'month','actual','forecast','forecast_lower','forecast_upper','gap','gap_percent','mean_cp_rev','p','d','q')
                write.table(predAll, file = fileFullPath,quote = TRUE, sep = ",",row.names = FALSE)
        }
        
}

###1.sampling data to trainging dataset and validation dataset
###2. build arima model
###@ts_rev: the time series data, @fit.val: the validation dataset
###Author: Rita Chen 2016/8/10
sample_buildArima <- function(data_src,start_yr,start_mth,selectedid){
        Start_yr_mth<-start_yr*100+start_mth   
        print(Start_yr_mth)
        #2. filter data
        data1<-data_src[which(data_src[,1]==selectedid ),]
        print(dim(data1))
        #print(data1)
        max_rowdata1<-max(as.integer(row.names(data1)))
        print(max_rowdata1)
        data0<-data1[which(as.integer(row.names(data1))<max_rowdata1),]
        #year month>201402
        data0<-data0[which(as.integer(data0[,2])>=Start_yr_mth),]
        print(dim(data0))
        #len<-dim(data0)[1]
        #2.1 trainnig dataset
        fit.training<-data0[which(as.integer(row.names(data0))<max_rowdata1-3),]
        summary(fit.training)
        
        print(dim(fit.training)[1])
        if(dim(fit.training)[1]<4)
        {
                return(NULL)               
        }else{
                #2.2 validation dataset
                fit.val<-data0[which(as.integer(row.names(data0))>=max_rowdata1-3),]
                #3. set time series data
                if(fit.training[,2][1]>Start_yr_mth){
                        start_yr<-fit.training[,2][1]/100
                        start_mth<-fit.training[,2][1]%%100
                }
                ts_rev <- ts(fit.training[,4], start=c(start_yr,start_mth), frequency=12)
                #3.1 set the source data
                #rev<-data.frame(as.character(fit.training$year_mth),fit.training$cp_rev)
                rev<-data.frame(as.character(fit.training[,2]),fit.training[,4])
                names(rev)<-c('year_mth','rev')
                fall<-build_arima_all(ts_rev,fit.val,fit.training)     
                return(fall)
        }
}