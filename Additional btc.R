##########################################
#Additional Work on Bitcoin
########################################


# Same procedure has been repeated Only changes are the chnsnges needed to clean the data set
rm(list=ls(all=TRUE))
library("QRM")
library("dplyr")
library("ggplot2")
library("psych")

data<-data.frame(read.csv("dataset\\bitcoin.csv"))

data<-as.timeSeries(data$Close.Price)


rows<-nrow(data)
cols<-ncol(data)


logPrices<-data.frame(data)
logPrices<-log(logPrices)

logReturnPrices<-data.frame(logPrices)
logReturnPrices[2:rows,]<-logReturnPrices[2:rows,]-
  logReturnPrices[1:rows-1,]


logReturnPrices<-as.timeSeries(logReturnPrices)


dev.new()
plot(logReturnPrices[2:rows], type="l")
dev.off()

#############################################################################

#############################################################################



quantile_probability=0.99



capitalPerShare<-1



loss.fun<-function(x,capitalPerShare,start,delta){
  
  tDelta<-as.timeSeries(x[start:(start+delta)])
  tDelta[(start+1):(start+delta)]<-capitalPerShare*exp(cumsum(tDelta[(start+1):(start+delta)]))
  tDelta[start]<-capitalPerShare
  
  loss<-as.timeSeries(tDelta)
  loss[(start+1):(start+delta)]<--(loss[(start+1):(start+delta)]-loss[start:((start+delta)-1)])
  
  loss$dailyLosses<-rowSums(loss)
  loss<-loss[-1]
  
  return(loss)
}

los<-loss.fun(logReturnPrices,1,1,2900)

los<-as.data.frame(los)
colnames(los)<-"dailyLosses"
varhs<-data.frame(los)


cauchy.sigma=26.99022
cauchy.mu=-0.367632
cauchy.beta=0.08275281

meanLoss<-mean(los$dailyLosses)
varLoss <-var(los$dailyLosses)*((nrow(los))/(nrow(los)-1))
hist(los$dailyLosses,nclass=1000)


hs.data <- los$dailyLosses


dev.new()
qqnorm(hs.data)
dev.off()
VaR.hs <- quantile(hs.data,quantile_probability)


hist(hs.data,nclass=100)
#~ Plot results
dev.new()
hist(hs.data,nclass=250, prob=TRUE, xlab="Loss Distribution",
     main="Value at Risk for 2007 : delta(1000 trading days)")
abline(v=c(VaR.normal),col=c(1),lty=1);

legendnames <- c(paste(quantile_probability,"normal VaR = ",VaR.normal))
legend("topleft", legend = legendnames, col=c(1), lty=c(1),text.width = 15000) 
dev.off()

#############################################################################

#############################################################################
MardiaTest(as.matrix(hs.data))
library("tseries")
jarque.bera.test(hs.data)
cauchy_test(hs.data,N=1000)

