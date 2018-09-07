########################################################################################
# ME317 Project
#########################################################################################

# Setting up the environment
rm(list=ls(all=TRUE))
library("QRM")
library("psych")
library("tseries")

# Reading CSV file and refining data
inputData<-data.frame(read.csv("dataset\\dataSetRefined.csv"))
colnames(inputData)<-c("PERMNO", "date","ticker","PRC")
assets<-unique(inputData[,c(-2,-4)])
inputData<-inputData[,-3]

# Reshaping Data and Removing NA
data <- reshape(inputData,
              timevar = "PERMNO",
              idvar = c("date"),
              direction = "wide")

data<-data[complete.cases(data), ]

# Coutning the number of rows and columns
rows<-nrow(data)
cols<-ncol(data)

# Refining Data
data$date<-as.Date(as.character(data[,1]),"%Y%m%d")
colnames(data)<-c("date",as.character(assets$ticker))

# Reordering data sector wise
data<-data[c(1,2,3,4,7,5,6,8,9,10,11)]
assets$ticker<-colnames(data[,2:11])

########################################################################################
# Log Prices and Daiy Log Returns
#########################################################################################


logPrices<-data.frame(data)
logPrices[,2:cols]<-log(logPrices[,2:cols])

logReturnPrices<-data.frame(logPrices)
logReturnPrices[2:rows,2:cols]<-logReturnPrices[2:rows,2:cols]-
                                    logReturnPrices[1:(rows-1),2:cols]

# Important Periods
periods<-as.Date(c("20070104","20080102","20100104","20110103"),"%Y%m%d")
index<-matrix(NA,1,length(periods))
for(i in 1:length(periods)){
index[1,i]<-which(data[,1]==periods[i])
}

# Important Dates
impDates<-as.Date(c("20080314","20080915","20081001","20081015"),"%Y%m%d")
ind<-matrix(NA,1,length(impDates))
for(i in 1:length(impDates)){
ind[1,i]<-which(data[,1]==impDates[i])
}

# Plotting the stock dynamics from 2007-2011
dev.new()
par(mfrow=c(4,3))

for (i in 2:11){
plot(logReturnPrices$date[index[1]:index[2]], logReturnPrices[index[1]:index[2],i],type="l",col="black",
xlim=c(logReturnPrices[index[1],1],logReturnPrices[index[length(index)],1]),
ylim=c(-0.2,0.2)
,xlab="Dates(in years)",ylab=assets$ticker[i-1],
main=paste("Log Returns from 2007-2011: ",assets$ticker[i-1]))
lines(logReturnPrices$date[index[2]:index[3]], logReturnPrices[index[2]:index[3],i],type="l",col="red")
lines(logReturnPrices$date[index[3]:index[4]], logReturnPrices[index[3]:index[4],i],type="l",col="blue")
abline(v=logReturnPrices$date[ind[1]])
abline(v=logReturnPrices$date[ind[2]])
abline(v=logReturnPrices$date[ind[3]])
abline(v=logReturnPrices$date[ind[4]])
}
dev.off()



# Pair Wise Scatter Plots

# Financial Sector
dev.new();pairs.panels(logReturnPrices[2:1008,7:11],hist.col="yellow",density=TRUE,alpha=0.2)
dev.off()

# Non Financial Sector
dev.new();pairs.panels(logReturnPrices[2:1008,2:6],hist.col="yellow",density=TRUE,alpha=0.2)
dev.off()

# Mixture of FInancial and Non FInancial Sector
dev.new();pairs.panels(logReturnPrices[2:1008,c(3,4,5,8,11)],hist.col="yellow",density=TRUE,alpha=0.2)
dev.off()

########################################################################################
# Loss Function and daily Losses
#########################################################################################

# Quantile Probability
quantile_probability=0.95

# Capital per Share
capitalPerShare<-1000

# Daily Loss Function
# It takes in a start index and a delta time period. It calcultes the daily losses
# from start to start+delta

loss.fun<-function(x,capitalPerShare,start,delta){
  tDelta<-data.frame(x[start:(start+delta),])
  tDelta[(start+1):(start+delta),2:cols]<-capitalPerShare*exp(cumsum(tDelta[(start+1):(start+delta),2:cols]))
  tDelta[start,2:cols]<-capitalPerShare
  loss<-data.frame(tDelta)
  loss[(start+1):(start+delta),2:cols]<--(loss[(start+1):(start+delta),2:cols]-loss[start:((start+delta)-1),2:cols])
  loss$dailyLosses<-rowSums(loss[,2:cols])
  loss<-loss[-1,-(2:cols)]
  return(loss)
  
}

# Calling the function for the period 2007-2011
los<-loss.fun(logReturnPrices,1000,1,1000)

# hs.data is the list of all daliy losses over the time period
hs.data<-los$dailyLosses

########################################################################################
# Normality Testing
#########################################################################################

JBTest <- function(dataMatrix){
  ncol <- dim(dataMatrix)[2]
  pvals <- array(NA, ncol)
  for (i in 1:ncol) pvals[i] <- jarque.bera.test(dataMatrix[,i])$p.value
  if (!is.null(names(dataMatrix))) names(pvals) <- names(dataMatrix)
  return(pvals)
}

MardiaTest(as.matrix(hs.data))
JBTest(as.matrix(hs.data))


########################################################################################
# Value-At-Risk Calculation
#########################################################################################

# Mean Loss
meanLoss<-mean(los$dailyLosses)

# Varince of Loss
varLoss <-var(los$dailyLosses)*((nrow(los))/(nrow(los)-1))

# VaR according to model assumption
VaR.normal <- meanLoss+ sqrt(varLoss) * (qnorm(quantile_probability))

# Plotting the QQNORM of the data
dev.new();qqnorm(hs.data)
dev.off()

# VaR accoeding to the emperical data
VaR.hs <- quantile(hs.data,quantile_probability)

# Plot results
dev.new()
hist(hs.data,nclass=250, prob=TRUE, xlab="Loss Distribution",
main="Value at Risk for 2007 : delta(1000 trading days)")
abline(v=c(VaR.normal),col=c(1),lty=1)
abline(v=c(VaR.hs),col=c(2),lty=2)
legendnames <- c(paste(quantile_probability,"normal VaR = ",VaR.normal)
,paste(quantile_probability,"HS VaR = ",VaR.hs)
)
legend("topleft", legend = legendnames, col=c(1,2), lty=c(1,2),text.width = 500)
dev.off()

########################################################################################
# Back Testing and Validation
#########################################################################################

# This function checks for the number of violations in the past trading days.
backtest<-function(dailyLosses){
  normal=0
  N<-length(dailyLosses)
  for(i in 1:1000){
    if (dailyLosses[i]>VaR.normal){
      normal=normal+1
    }
  }
  return (normal)
}

backtest(hs.data)
#if backTest for 1000 trading days at 95% level is 37 to 65, then model
# is not rejected


########################################################################################
# Copula Fitting
#########################################################################################


# Creating a pseudo copula
copula<-apply(logReturnPrices[2:1000,2:11],2,edf,adjust=1)

# calculating the Spearman's Rho
corrMatrix<-Spearman(copula)

# Plotting the bivariate model
dev.new();
pairs(copula)
dev.off();

# Gaussian Copula
copulaGauss <- fit.gausscopula(copula)

# T COpula
copulaT <- fit.tcopula(copula)

########################################################################################
# END
#########################################################################################

