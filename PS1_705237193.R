setwd("/Users/jiaminghuang/Downloads")

library(data.table)

##1. Construct the value-weighted market return using CRSP data,1
##replicating the market return
##time series available in Kenneth French website.2 Also calculate the equal-weighted market return,
##and the lagged total market capitalization. Your output should be from January 1926 to December
##2018, at a monthly frequency.

CRSP_Stocks <-  as.data.table(read.csv("CRSP_Stocks.csv", header = TRUE))

PS1_Q1 <- function(CRSP_Stocks){
  #Data Cleaning Part
  CRSP_Stocks$PRC <- abs(CRSP_Stocks$PRC)
  CRSP_Stocks <- na.omit(CRSP_Stocks)
  CRSP_Stocks[RET == "C"]$RET <- "0"
  CRSP_Stocks[RET == "B"]$RET <- "0"
  CRSP_Stocks[DLRET == "A"]$DLRET  <- "0"
  CRSP_Stocks[DLRET == "P"]$DLRET  <- "0" 
  CRSP_Stocks[DLRET == "S"]$DLRET  <- "0"
  CRSP_Stocks[DLRET == "T"]$DLRET  <- "0" 
  CRSP_Stocks$RET = as.numeric(as.character(CRSP_Stocks$RET))
  CRSP_Stocks$DLRET = as.numeric(as.character(CRSP_Stocks$DLRET))
  CRSP_Stocks$date = as.Date(as.character(CRSP_Stocks$date),format = "%Y%m%d")
  CRSP_Stocks$Year = year(CRSP_Stocks$date)
  CRSP_Stocks$Month = month(CRSP_Stocks$date)
  CRSP_Stocks = CRSP_Stocks[CRSP_Stocks$SHRCD == 10|CRSP_Stocks$SHRCD == 11]
  CRSP_Stocks = CRSP_Stocks[CRSP_Stocks$EXCHCD == 1|CRSP_Stocks$EXCHCD == 2|CRSP_Stocks$EXCHCD == 3]
  #Calculating Value-Weighted Return
  #Calculating Value Weight
  CRSP_Stocks[,MKT_Cap := PRC * SHROUT]
  #Calculating Return
  CRSP_Stocks[(!is.na(RET))&(!is.na(DLRET)),RETURN := ((1+RET)*(1+DLRET)-1)]
  CRSP_Stocks[(is.na(RET))&(!is.na(DLRET)),RETURN := DLRET]
  CRSP_Stocks[(!is.na(RET))&(is.na(DLRET)),RETURN := RET]
  #Calculating market value the previous month
  CRSP_Stocks[,MKT_Cap_Pre := shift(MKT_Cap,1),by = c("PERMNO")]
  CRSP_Stocks = CRSP_Stocks[!is.na(CRSP_Stocks$MKT_Cap_Pre)]
  #Calculating Value-Weighted Return
  Stock_Vw_Ret = CRSP_Stocks[,.(Stock_Vw_Ret = weighted.mean(RETURN,MKT_Cap_Pre,na.rm = TRUE)),by = list(Year,Month)]
  #Calculating Equal-Weighted Return
  Stock_Ew_Ret = CRSP_Stocks[,.(Stock_Ew_Ret = mean(RETURN,na.rm = TRUE)),by = list(Year,Month)]
  #Calculating Total market value the previous month (in millions)
  Stock_lag_MV = CRSP_Stocks[,.(Stock_lag_MV = sum(MKT_Cap_Pre,na.rm = TRUE)/1000000), by = list(Year,Month)]
  #Combine the result
  Result = merge(Stock_Ew_Ret,Stock_Vw_Ret,by = c("Year","Month"))
  Result = merge(Result,Stock_lag_MV, by = c("Year","Month"))
  return(Result)
}

Result1 <- PS1_Q1(CRSP_Stocks)
Result1

##2. Using the risk-free rate of return from French???s website3
##, report the following moments of
##the market excess returns for both time series (4 decimal digits): annualized return, annualized
##volatility, annualized Sharpe ratio, skewness, and excess kurtosis. You should be comparing
##between July 1926 to December 2018, at a monthly frequency. 

library(DistributionUtils)

FF_mkt <- as.data.table(read.csv("F-F_Research_Data_Factors.csv"))

PS1_Q2 <- function(Result1,FF_mkt){
  FF_mkt$date = as.Date(paste(FF_mkt$date,"-01",sep=""),format = "%Y%m-%d")
  FF_mkt$Year = year(FF_mkt$date)
  FF_mkt$Month = month(FF_mkt$date)
  data1 = FF_mkt[,c("Mkt.RF","RF","Year","Month")]
  data2 = Result1[,c("Stock_Vw_Ret","Year","Month")]
  data = merge(data1,data2,by = c("Year","Month"))
  data$RF = data$RF/100
  data[,MKT_Excess_Ret:= Stock_Vw_Ret - RF]
  data$Mkt.RF = (data$Mkt.RF/100)
  Result = data.table(
    Stats = c("Annualized Mean","Annualized Standard Deviation","Annualized Sharp Ratio","Skewness","Kurtosis"),
    French = c(12*mean(data$Mkt.RF),sqrt(12)*sd(data$Mkt.RF),12*mean(data$Mkt.RF)/(sqrt(12)*sd(data$Mkt.RF)),skewness(data$Mkt.RF),kurtosis(data$Mkt.RF)),
    Replication = c(12*mean(data$MKT_Excess_Ret),sqrt(12)*sd(data$MKT_Excess_Ret),12*mean(data$MKT_Excess_Ret)/(sqrt(12)*sd(data$MKT_Excess_Ret)),skewness(data$MKT_Excess_Ret),kurtosis(data$MKT_Excess_Ret))
  )
  return(Result)
}

Result2 = PS1_Q2(Result1,FF_mkt)
Result2

##3. Report (up to 8 decimal digits) the correlation between your time series and French???s time
##series, and the maximum absolute difference between the two time series. It is zero? If not, justify
##whether the difference is economically negligible or not. What are the reasons a nonzero difference?
##You should be comparing between July 1926 to December 2018, at a monthly frequency.

PS1_Q3 <- function(Result1,FF_mkt){
  FF_mkt$date = as.Date(paste(FF_mkt$date,"-01",sep=""),format = "%Y%m-%d")
  FF_mkt$Year = year(FF_mkt$date)
  FF_mkt$Month = month(FF_mkt$date)
  data1 = FF_mkt[,c("Mkt.RF","RF","Year","Month")]
  data2 = Result1[,c("Stock_Vw_Ret","Year","Month")]
  data = merge(data1,data2,by = c("Year","Month"))
  data$RF = data$RF/100
  data[,MKT_Excess_Ret:= Stock_Vw_Ret - RF]
  data$Mkt.RF = (data$Mkt.RF/100)
  data$difference = abs(data$Mkt.RF-data$MKT_Excess_Ret)
  Result = data.table(
    Correlation = cor(data$Mkt.RF,data$MKT_Excess_Ret),
    Max.difference = max(data$difference)
  )
  return(Result)
}

Result3 = PS1_Q3(Result1,FF_mkt)

