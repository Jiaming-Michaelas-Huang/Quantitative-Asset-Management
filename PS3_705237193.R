setwd("/Users/jiaminghuang/Downloads")

library(data.table)
library(DistributionUtils)

#1. Using CRSP stock data, define the universe of monthly returns that can be used in calculating
#momentum portfolios, as well as their ranking return, following the procedure in Daniel and
#Moskowitz (2016) 1. Your output should be from 1927-2018.

CRSP_Stocks <-  as.data.table(read.csv("CRSP_Stocks.csv", header = TRUE))

PS3_Q1 <- function(CRSP_Stocks){
  #data cleaning process
  CRSP_Stocks$date = as.Date(as.character(CRSP_Stocks$date),format = "%Y%m%d")
  CRSP_Stocks$PRC = abs(CRSP_Stocks$PRC)
  CRSP_Stocks$DLRET = as.numeric(as.character(CRSP_Stocks$DLRET))
  CRSP_Stocks$RET = as.numeric(as.character(CRSP_Stocks$RET))
  #data restriction
  CRSP_Stocks = CRSP_Stocks[SHRCD == 10 | SHRCD == 11]
  CRSP_Stocks = CRSP_Stocks[EXCHCD == 1 | EXCHCD == 2 | EXCHCD == 3]
  #Return Calculation
  CRSP_Stocks[(!is.na(RET))&(!is.na(DLRET)),RETURN := ((1+RET)*(1+DLRET)-1)]
  CRSP_Stocks[(is.na(RET))&(!is.na(DLRET)),RETURN := DLRET]
  CRSP_Stocks[(!is.na(RET))&(is.na(DLRET)),RETURN := RET]
  CRSP_Stocks = CRSP_Stocks[,list(PERMNO,date,SHRCD,EXCHCD,PRC,RETURN,SHROUT)]
  CRSP_Stocks[is.na(RETURN) & !is.na(PRC)]$RETURN = 0
  #Mkt_Cap
  CRSP_Stocks[,Mkt_Cap := PRC * SHROUT]
  #lag_Mkt_Cap
  CRSP_Stocks[,lag_Mkt_Cap := shift(Mkt_Cap,1), by = c("PERMNO")]
  #log_Ret
  CRSP_Stocks[,log_Ret := log(1+RETURN)]
  #Past Cumulative Log Return
  CRSP_Stocks[,past_Cumulative_Log_Ret := shift(log_Ret,2)+shift(log_Ret,3)+shift(log_Ret,4)+shift(log_Ret,5)+shift(log_Ret,6)+
                shift(log_Ret,7)+shift(log_Ret,8)+shift(log_Ret,9)+shift(log_Ret,10)+shift(log_Ret,11)+shift(log_Ret,12), by = c("PERMNO")]
  #data restriction
  CRSP_Stocks[,rettm2 := shift(RETURN,2),by = c("PERMNO")]
  CRSP_Stocks[,prctm13 := shift(PRC,13),by = c("PERMNO")]
  CRSP_Stocks = CRSP_Stocks[!is.na(prctm13) & !is.na(rettm2) & !is.na(lag_Mkt_Cap)]
  CRSP_Stocks = na.omit(CRSP_Stocks)
  #Ranking
  CRSP_Stocks[,Ranking_Ret := frankv(past_Cumulative_Log_Ret,na.last = TRUE,order = 1),by = c("date")]
  #Year and Month
  CRSP_Stocks[,Year := year(date)]
  CRSP_Stocks[,Month := month(date)]
  
  Result1 = CRSP_Stocks[,list(Year,Month,PERMNO,EXCHCD,lag_Mkt_Cap,RETURN,Ranking_Ret)]
  colnames(Result1) = c("Year","Month","PERMNO","EXCHCD","lag_Mkt_Cap","Ret","Ranking_Ret")
  Result1 = Result1[Year>=1927 & Year <= 2018]
  return(Result1)
}

#2. Define the monthly momentum portfolio decile of each stock as defined by both Daniel and
#Moskowitz (2016) and Kenneth R. French. Your output should be from 1927-2018.

CRSP_Stocks_Momentum <- PS3_Q1(CRSP_Stocks)

PS3_Q2 <- function(CRSP_Stocks_Momentum){
  #Daniel & Moskowitz decile
  CRSP_Stocks_Momentum[, DM_decile := findInterval(Ranking_Ret,
                                    quantile(Ranking_Ret, probs = 1:10 / 10),
                                    rightmost.closed = T) + 1L, by = c("Year", "Month")]
  #KRF decile 
  years = sort(unique(CRSP_Stocks_Momentum$Year))
  months = sort(unique(CRSP_Stocks_Momentum$Month))
  for (y in years) {
    for (m in months) {
      NYSE = CRSP_Stocks_Momentum[Year == y & Month == m & EXCHCD == 1]
      nyse_decile = sort(NYSE$Ranking_Ret)
      cut = nyse_decile[(length(nyse_decile)/10)*seq(1,10,1)]
      decile = rep(1,nrow(CRSP_Stocks_Momentum[Year == y & Month == m]))
      decile[(cut[1]+1):(cut[2])] = 2
      decile[(cut[2]+1):(cut[3])] = 3
      decile[(cut[3]+1):(cut[4])] = 4
      decile[(cut[4]+1):(cut[5])] = 5
      decile[(cut[5]+1):(cut[6])] = 6
      decile[(cut[6]+1):(cut[7])] = 7
      decile[(cut[7]+1):(cut[8])] = 8
      decile[(cut[8]+1):(cut[9])] = 9
      decile[(cut[9]+1):(nrow(CRSP_Stocks_Momentum[Year == y & Month == m]))] = 10
      CRSP_Stocks_Momentum[Year == y & Month == m, KRF_decile := decile[Ranking_Ret]]
    }
  }
  Result2 = CRSP_Stocks_Momentum[,list(Year,Month,PERMNO,lag_Mkt_Cap,Ret,DM_decile,KRF_decile)]
  return(Result2)
}

#3. Calculate the monthly momentum portfolio decile returns as defined by both Daniel and
#Moskowitz (2016) and Kenneth R. French. Your output should be from 1927-2018.

CRSP_Stocks_Momentum_decile <- PS3_Q2(CRSP_Stocks_Momentum)
FF_mkt <- as.data.table(read.csv("F-F_Research_Data_Factors.csv"))

PS3_Q3 <- function(CRSP_Stocks_Momentum_decile,FF_mkt){
  DM_Ret = CRSP_Stocks_Momentum_decile[,.(DM_Ret = weighted.mean(Ret,lag_Mkt_Cap,na.rm = TRUE)),by = c("Year","Month","DM_decile")]
  KRF_Ret = CRSP_Stocks_Momentum_decile[,.(KRF_Ret = weighted.mean(Ret,lag_Mkt_Cap,na.rm = TRUE)),by = c("Year","Month","KRF_decile")]
  colnames(DM_Ret) = c("Year","Month","decile","DM_Ret")
  colnames(KRF_Ret) = c("Year","Month","decile","KRF_Ret")
  setkey(DM_Ret,Year,Month,decile)
  setkey(KRF_Ret,Year,Month,decile)
  Result3 = merge(DM_Ret,KRF_Ret)
  FF_mkt$date = as.Date(paste(FF_mkt$date,"-01",sep=""),format = "%Y%m-%d")
  FF_mkt$Year = year(FF_mkt$date)
  FF_mkt$Month = month(FF_mkt$date)
  Result3[,Rf := 0]
  years = sort(unique(Result3$Year))
  months = sort(unique(Result3$Month))
  for (y in years) {
    for (m in months) {
      Result3[Year == y & Month == m]$Rf =FF_mkt[Year == y & Month == m]$RF
    }
  }
  Result3$Rf = Result3$Rf/100
  return(Result3)
}

#4. Replicate Table 1 in Daniel and Moskowitz (2016), except for ??, t(??), ??, and sk(d) rows, and
#the Market column. Match the format and methodology to the extent possible.

CRSP_Stocks_Momentum_returns <- PS3_Q3(CRSP_Stocks_Momentum_decile,FF_mkt)

PS3_Q4 <- function(CRSP_Stocks_Momentum_returns){
  Q4 = CRSP_Stocks_Momentum_returns[Year <= 2013]
  Q4 = Q4[Year != 2013 | Month <= 3]
  avg = Q4[,.(mean_excess_ret = 1200*mean(DM_Ret-Rf)),by = c("decile")]
  sigma = Q4[,.(STD = 100*sqrt(12)*sd(DM_Ret-Rf)),by = c("decile")]
  SR = avg$mean_excess_ret/sigma$STD
  SKm = Q4[,.(skewness = skewness(log(1+DM_Ret))),by = c("decile")]
  
  Winner = CRSP_Stocks_Momentum_returns[decile == 10]
  Loser = CRSP_Stocks_Momentum_returns[decile == 1]
  WML = Winner$DM_Ret-Loser$DM_Ret
  
  Result4 = data.table(
    Variables = c("r-rf","sigma","SR","sk(m)"),
    Decile1 = c(avg[decile == 1]$mean_excess_ret,sigma[decile == 1]$STD,SR[1],SKm[decile == 1]$skewness),
    Decile2 = c(avg[decile == 2]$mean_excess_ret,sigma[decile == 2]$STD,SR[2],SKm[decile == 2]$skewness),
    Decile3 = c(avg[decile == 3]$mean_excess_ret,sigma[decile == 3]$STD,SR[3],SKm[decile == 3]$skewness),
    Decile4 = c(avg[decile == 4]$mean_excess_ret,sigma[decile == 4]$STD,SR[4],SKm[decile == 4]$skewness),
    Decile5 = c(avg[decile == 5]$mean_excess_ret,sigma[decile == 5]$STD,SR[5],SKm[decile == 5]$skewness),
    Decile6 = c(avg[decile == 6]$mean_excess_ret,sigma[decile == 6]$STD,SR[6],SKm[decile == 6]$skewness),
    Decile7 = c(avg[decile == 7]$mean_excess_ret,sigma[decile == 7]$STD,SR[7],SKm[decile == 7]$skewness),
    Decile8 = c(avg[decile == 8]$mean_excess_ret,sigma[decile == 8]$STD,SR[8],SKm[decile == 8]$skewness),
    Decile9 = c(avg[decile == 9]$mean_excess_ret,sigma[decile == 9]$STD,SR[9],SKm[decile == 9]$skewness),
    Decile10 = c(avg[decile == 10]$mean_excess_ret,sigma[decile == 10]$STD,SR[10],SKm[decile == 10]$skewness),
    WML = c(1200*mean(WML),100*sqrt(12)*sd(WML),1200*mean(WML)/(100*sqrt(12)*sd(WML)),skewness(log(1+WML)))
  )
  return(Result4)
}

Result4 = PS3_Q4(CRSP_Stocks_Momentum_returns)
Result4

#5. Calculate the correlation of your portfolio returns with the Daniel and Moskowitz (2016)
#breakpoints (by decile), to the portfolio returns on Daniel???s website. Also calculate the correlation
#of your portfolio returns with the Kenneth R. French breakpoints (by decile), to the portfolio
#returns on French???s website. Round to 4 decimal places. Correlations should be calculated from
#1927-2018.

DM_returns = as.data.table(read.delim("m_m_pt_tot.txt",header = FALSE,sep = "",dec = "\n"))
KRF_returns = as.data.table(read.csv("10_Portfolios_Prior_12_2.csv"))
colnames(DM_returns) = c("date","decile","Return","Mkt_Cap","Num_of_Stocks")

PS3_Q5 <- function(CRSP_Stocks_Momentum_returns,DM_returns,KRF_returns){
  colnames(DM_returns) = c("date","decile","DM_Return","Mkt_Cap","Num_of_Stocks")
  DM_returns$DM_Return = as.numeric(as.character(DM_returns$DM_Return))
  DM_returns$date = as.Date(as.character(DM_returns$date),format = "%Y%m%d")
  DM_returns[,Year := year(date)]
  DM_returns[,Month := month(date)]
  DM_returns$date = NULL
  setkey(DM_returns,Year,Month,decile)
  Q5 = CRSP_Stocks_Momentum_returns[Year <= 2016]
  Q5 = merge(Q5,DM_returns)
  
  
  KRF_returns$date =  as.Date(paste(KRF_returns$X,"-01",sep=""),format = "%Y%m-%d")
  KRF_returns[,Year := year(date)]
  KRF_returns[,Month := month(date)]
  KRF_returns = na.omit(KRF_returns)
  KRF_returns$X = NULL
  KRF_returns$date = NULL
  colnames(KRF_returns) = c("1","2","3","4","5","6","7","8","9","10","Year","Month")
  KRF_returns = melt(KRF_returns,id.vars = c("Year","Month"),measure.vars = c("1","2","3","4","5","6","7","8","9","10"))
  colnames(KRF_returns) = c("Year","Month","decile","KRF_Return")
  KRF_returns$KRF_Return = as.numeric(KRF_returns$KRF_Return)/100
  KRF_returns = KRF_returns[Year <= 2016]
  KRF_returns$decile = as.numeric(as.character(KRF_returns$decile))
  setkey(KRF_returns,Year,Month,decile)
  Q5 = merge(Q5,KRF_returns)
  
  DM_WML_Realized = Q5[decile == 10]$DM_Return - Q5[decile == 1]$DM_Return
  KRF_WML_Realized = Q5[decile == 10]$KRF_Return - Q5[decile == 1]$KRF_Return
  DM_WML_Estimated = Q5[decile == 10]$DM_Ret - Q5[decile == 1]$DM_Ret
  KRF_WML_Estimated = Q5[decile == 10]$KRF_Ret - Q5[decile == 1]$KRF_Ret
  
  
  cor_DM = Q5[,.(DM_cor = cor(DM_Ret,DM_Return)),by = c("decile")]
  cor_KRF = Q5[,.(KRF_cor = cor(KRF_Ret,KRF_Return)),by = c("decile")]
  Result5 = data.table(
    variable = c("DM correlation","KRF correlation"),
    Decile1 = c(cor_DM[decile == 1]$DM_cor,cor_KRF[decile == 1]$KRF_cor),
    Decile2 = c(cor_DM[decile == 2]$DM_cor,cor_KRF[decile == 2]$KRF_cor),
    Decile3 = c(cor_DM[decile == 3]$DM_cor,cor_KRF[decile == 3]$KRF_cor),
    Decile4 = c(cor_DM[decile == 4]$DM_cor,cor_KRF[decile == 4]$KRF_cor),
    Decile5 = c(cor_DM[decile == 5]$DM_cor,cor_KRF[decile == 5]$KRF_cor),
    Decile6 = c(cor_DM[decile == 6]$DM_cor,cor_KRF[decile == 6]$KRF_cor),
    Decile7 = c(cor_DM[decile == 7]$DM_cor,cor_KRF[decile == 7]$KRF_cor),
    Decile8 = c(cor_DM[decile == 8]$DM_cor,cor_KRF[decile == 8]$KRF_cor),
    Decile9 = c(cor_DM[decile == 9]$DM_cor,cor_KRF[decile == 9]$KRF_cor),
    Decile10 = c(cor_DM[decile == 10]$DM_cor,cor_KRF[decile == 10]$KRF_cor),
    WML = c(cor(DM_WML_Estimated,DM_WML_Realized),cor(KRF_WML_Estimated,KRF_WML_Realized))
  )
  return(Result5)
}

Result5 = PS3_Q5(CRSP_Stocks_Momentum_returns,DM_returns,KRF_returns)
Result5

library(ggplot2)

PS3_Q6 <- function(CRSP_Stocks_Momentum_returns,FF_mkt){
  FF_mkt$date = as.Date(paste(FF_mkt$date,"-01",sep=""),format = "%Y%m-%d")
  FF_mkt$Year = year(FF_mkt$date)
  FF_mkt$Month = month(FF_mkt$date)
  FF_mkt$date = NULL
  FF_mkt$Mkt.RF = FF_mkt$Mkt.RF/100
  FF_mkt$SMB = FF_mkt$SMB/100
  FF_mkt$HML = FF_mkt$HML/100
  FF_mkt$RF = FF_mkt$RF/100
  FF_mkt = FF_mkt[Year <=2018 & Year >= 1927]
  setkey(FF_mkt,Year,Month)
  setkey(CRSP_Stocks_Momentum_returns,Year,Month)
  Q6 = merge(CRSP_Stocks_Momentum_returns,FF_mkt)
  DM_WML = Q6[decile == 10]$DM_Ret - Q6[decile == 1]$DM_Ret- Q6[decile == 1]$Rf
  KRF_WML = Q6[decile == 10]$KRF_Ret - Q6[decile == 1]$KRF_Ret- Q6[decile == 1]$Rf
  Market = Q6[decile == 1]$Mkt.RF
  SMB = Q6[decile == 1]$SMB
  HML = Q6[decile == 1]$HML
  Result6 = data.table(
    Year = Q6[decile == 1]$Year,
    Month = Q6[decile == 1]$Month,
    Market = Market,
    SMB = SMB,
    HML = HML,
    DM_WML = DM_WML,
    KRF_WML = KRF_WML
  )
  ggplot(Result6[Year>=2013], aes(1:nrow(Result6[Year>=2013])))+
    geom_line(aes(y=cumsum(Result6[Year>=2013]$Market),col = "Value-Weighted"))+
    geom_line(aes(y=cumsum(Result6[Year>=2013]$SMB),col = "SMB"))+
    geom_line(aes(y=cumsum(Result6[Year>=2013]$HML),col = "HML"))+
    geom_line(aes(y=cumsum(Result6[Year>=2013]$DM_WML),col = "DM_WML"))+
    geom_line(aes(y=cumsum(Result6[Year>=2013]$KRF_WML),col = "KRF_WML"))+
    theme_bw()
  ggplot(Result6[Year>=2008], aes(1:nrow(Result6[Year>=2008])))+
    geom_line(aes(y=cumsum(Result6[Year>=2008]$Market),col = "Value-Weighted"))+
    geom_line(aes(y=cumsum(Result6[Year>=2008]$SMB),col = "SMB"))+
    geom_line(aes(y=cumsum(Result6[Year>=2008]$HML),col = "HML"))+
    geom_line(aes(y=cumsum(Result6[Year>=2008]$DM_WML),col = "DM_WML"))+
    geom_line(aes(y=cumsum(Result6[Year>=2008]$KRF_WML),col = "KRF_WML"))+
    theme_bw()
}

