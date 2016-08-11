###1.automaticaly automatically run arima with specified parameters(pdq PDQ period)
###2. predict validation dataset, and compare actual with forecast to get the gap & gap%
###@ts_rev: the time series data, @fit.val: the validation dataset
###Author: Rita Chen 2016/8/10
build_arima_all <- function(ts_rev,fit.val)
{
        #ARIMA(p,d,q)x(P,D,Q)
        pdq<-matrix(c(0,0,0, 1,1,0, 1,0,0, 1,0,1, 1,1,1, 0,0,1, 0,1,1, 0,1,0),nrow=8,ncol=3,byrow=TRUE)
        sPDQ<-matrix(c(1,1,0, 0,1,1, 1,0,0, 0,0,1),nrow=4,ncol=3,byrow=TRUE)
        period<-matrix(c(0,12,4),nrow=3,ncol=1,byrow=TRUE)
        #pdq_i<- 1 
        model_i<-1
        predAll<-NULL
        #install.packages("forecast")
        library("forecast")
        for (pdq_i in 1:nrow(pdq) )
        #while(pdq_i<=nrow(pdq))
        {
                #****************1. fit model without seasonal
                #sPDQ_i<- 1
                for (sPDQ_i in 1:nrow(sPDQ) )
                #while(sPDQ_i<=nrow(sPDQ))
                {
                        #period_i<- 1
                        for (period_i in 1:nrow(period) )
                        #while(period_i<=nrow(period))
                        {
                                delayedAssign("do.next", {next})
                                if(period[period_i]==0)
                                {
                                        tryCatch({                                     
                                                
                                                rev.fit<-arima(ts_rev,order=pdq[pdq_i,], method="ML")
                                        },finally=print('whoops'),error=function(e) force(do.next)  )
                                       
                                        
                                }
                                else{
                                        tryCatch({ 
                                        rev.fit<-arima(ts_rev,order=pdq[pdq_i,],seasonal=list(order=sPDQ[sPDQ_i,], period=period[period_i]), method="ML")
                                        } ,finally=print('whoops'),error=function(e) force(do.next) )
                                }    
                              
                                plot.ts(rev.fit$residuals)
                                #summary(rev.fit)
                                rev.fit    
                                tryCatch({                                     
                                
                                forecast.fit <- forecast.Arima(rev.fit,h=3,level=c(99.9))
                                },finally=print('whoops'),error=function(e) force(do.next)  )
                                summary(forecast.fit)  
                                plot.forecast(forecast.fit)
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
                                forecast1=cbind(model_i,rev.fit$aic,fit.val$year_mth[1],a_may,f_may,gap_may,gap_pct_may,mean,forecast.fit$lower[1],forecast.fit$upper[1],pdq[pdq_i,1],pdq[pdq_i,2],pdq[pdq_i,3],sPDQ[sPDQ_i,1],sPDQ[sPDQ_i,2],sPDQ[sPDQ_i,3],period[period_i])
                                forecast2=cbind(model_i,rev.fit$aic,fit.val$year_mth[2],a_june,f_june,gap_june,gap_pct_june,mean,forecast.fit$lower[2],forecast.fit$upper[2],pdq[pdq_i,1],pdq[pdq_i,2],pdq[pdq_i,3],sPDQ[sPDQ_i,1],sPDQ[sPDQ_i,2],sPDQ[sPDQ_i,3],period[period_i])
                                forecast3=cbind(model_i,rev.fit$aic,fit.val$year_mth[3],a_july,f_july,gap_july,gap_pct_july,mean,forecast.fit$lower[3],forecast.fit$upper[3],pdq[pdq_i,1],pdq[pdq_i,2],pdq[pdq_i,3],sPDQ[sPDQ_i,1],sPDQ[sPDQ_i,2],sPDQ[sPDQ_i,3],period[period_i])
                                fall<-rbind(as.matrix(forecast1),as.matrix(forecast2),as.matrix(forecast3))
                                predAll<-rbind(predAll,fall)
                                model_i<-model_i+1
                                #cat("ERROR :",conditionMessage(e),"\n")
                                
#period_i<-period_i+1
                        }  
#sPDQ_i<-sPDQ_i+1
                }
#pdq_i<-pdq_i+1
        }
return(predAll)
}