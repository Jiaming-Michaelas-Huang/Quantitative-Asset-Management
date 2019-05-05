# loading the data 
library(readxl)
library(ggplot2)
library(data.table)
library(DistributionUtils)
setwd("/Users/jiaminghuang/Downloads")

CRSP_Bonds <- as.data.table(read.csv("CRSP_Bonds.csv")[,c("KYCRSPID","MCALDT","TMRETNUA","TMTOTOUT")])

#1. Construct the equal-weighted bond market return, value-weighted bond market return, and
#lagged total bond market capitalization using CRSP Bond data 1.
#Your output should be from January 1926 to December 2018, at a monthly frequency.

PS2_Q1 <- function(CRSP_Bonds){
  #data preprocessing
  CRSP_Bonds[,`:=`(TMRETNUA, ifelse(TMRETNUA == -99.0, NA, TMRETNUA))]
  CRSP_Bonds$MCALDT = as.Date(as.character(CRSP_Bonds$MCALDT),format = "%m/%d/%Y")
  CRSP_Bonds$MKTCAP = CRSP_Bonds$TMTOTOUT*1000/1000000
  #calculation of lagged total bond market capitalization
  CRSP_Bonds[,Pre_MKTCAP := shift(MKTCAP,1),by = KYCRSPID]
  lagged_total_bond_market_capitalization = CRSP_Bonds[,.(lagged_total_market_capitalization=sum(Pre_MKTCAP, na.rm = TRUE)), by = MCALDT]
  #construct equal-weighted bond market return
  equal_weighted_bond_market_return = CRSP_Bonds[,.(equal_weighted_return = mean(TMRETNUA,na.rm = TRUE)), by = MCALDT]
  #construct value-weighted bond market return
  value_weighted_bond_market_return = CRSP_Bonds[,.(value_weighted_return = sum(TMRETNUA*Pre_MKTCAP, na.rm = TRUE)/sum(Pre_MKTCAP,na.rm = TRUE)),by = MCALDT]
  #output result
  output1 <- data.table(
    Year = year(value_weighted_bond_market_return$MCALDT),
    Month = month(value_weighted_bond_market_return$MCALDT),
    Bond_lag_MV = lagged_total_bond_market_capitalization$lagged_total_market_capitalization,
    Bond_Ew_Ret = equal_weighted_bond_market_return$equal_weighted_return,
    Bond_Vw_Ret = value_weighted_bond_market_return$value_weighted_return
  )
  output1$Bond_Vw_Ret = na.fill(output1$Bond_Vw_Ret,0)
  return(output1)
}


#2. Aggregate stock, bond, and riskless datatables. For each year-month, 
#calculate the lagged market value and excess value-weighted returns for both stocks and bonds. 
#Your output should be from January 1926 to December 2018, at a monthly frequency.

CRSP_Stocks <-  as.data.table(read.csv("CRSP_Stocks.csv", header = TRUE))

PS1_Q1 <- function(CRSP_Stocks){
  #Data Cleaning Part
  CRSP_Stocks$PRC <- abs(CRSP_Stocks$PRC)
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
  CRSP_Stocks$MKT_Cap_Pre = na.fill(CRSP_Stocks$MKT_Cap_Pre,0)
  #Calculating Value-Weighted Return
  Stock_Vw_Ret = CRSP_Stocks[,.(Stock_Vw_Ret = weighted.mean(RETURN,MKT_Cap_Pre,na.rm = TRUE)),by = list(Year,Month)]
  #Calculating Equal-Weighted Return
  Stock_Ew_Ret = CRSP_Stocks[,.(Stock_Ew_Ret = mean(RETURN,na.rm = TRUE)),by = list(Year,Month)]
  #Calculating Total market value the previous month (in millions)
  Stock_lag_MV = CRSP_Stocks[,.(Stock_lag_MV = sum(MKT_Cap_Pre,na.rm = TRUE)/1000000), by = list(Year,Month)]
  #Combine the result
  Result = merge(Stock_Ew_Ret,Stock_Vw_Ret,by = c("Year","Month"))
  Result = merge(Result,Stock_lag_MV, by = c("Year","Month"))
  Result$Stock_Vw_Ret = na.fill(Result$Stock_Vw_Ret,0)
  return(Result)
}

Monthly_CRSP_Stocks <- PS1_Q1(CRSP_Stocks)
Monthly_CRSP_Bonds <- PS2_Q1(CRSP_Bonds)
Monthly_CRSP_Riskless <- as.data.table(read.csv("CRSP_Risk_Free.csv"))

PS2_Q2 <- function(Monthly_CRSP_Stocks,Monthly_CRSP_Bonds,Monthly_CRSP_Riskless){
  DT = merge(Monthly_CRSP_Bonds,Monthly_CRSP_Stocks, by = c("Year","Month"))
  Monthly_CRSP_Riskless$caldt = as.Date(as.character(Monthly_CRSP_Riskless$caldt),format = "%Y%m%d")
  Monthly_CRSP_Riskless[,Year:=year(caldt)]
  Monthly_CRSP_Riskless[,Month:=month(caldt)]
  Monthly_CRSP_Riskless$caldt = NULL
  DT = merge(DT,Monthly_CRSP_Riskless,by = c("Year","Month"))
  DT[,Stock_Excess_Vw_Ret:=Stock_Vw_Ret-t30ret]
  DT[,Bond_Excess_Vw_Ret:=Bond_Vw_Ret-t30ret]
  return(DT[,c("Year","Month","Stock_lag_MV","Stock_Excess_Vw_Ret","Bond_lag_MV","Bond_Excess_Vw_Ret")])
}

#3. Calculate the monthly unlevered and levered risk-parity portfolio returns as defined by Asness,Frazzini, and Pedersen (2012).3
#For the levered risk-parity portfolio, match the value-weighted
#portfolio???s ???? over the longest matched holding period of both. Your output should be from January
#1926 to December 2018, at a monthly frequency.

Monthly_CRSP_Universe <- PS2_Q2(Monthly_CRSP_Stocks, Monthly_CRSP_Bonds,Monthly_CRSP_Riskless)

PS2_Q3 <- function(Monthly_CRSP_Universe){
  Monthly_CRSP_Universe[,Excess_Vw_Ret := (Stock_Excess_Vw_Ret*(Stock_lag_MV/(Stock_lag_MV+Bond_lag_MV)))+(Bond_Excess_Vw_Ret*(Bond_lag_MV/(Stock_lag_MV+Bond_lag_MV)))]
  Monthly_CRSP_Universe[,Excess_60_40_Ret := 0.6*Stock_Excess_Vw_Ret+0.4*Bond_Excess_Vw_Ret]
  Monthly_CRSP_Universe[,Stock_inverse_sigma_hat:=0]
  Monthly_CRSP_Universe[,Bond_inverse_sigma_hat:=0]
  for (r in 1:36) {
    Monthly_CRSP_Universe$Stock_inverse_sigma_hat[r] = 1/sd(Monthly_CRSP_Universe$Stock_Excess_Vw_Ret[1:r-1])
    Monthly_CRSP_Universe$Bond_inverse_sigma_hat[r] = 1/sd(Monthly_CRSP_Universe$Bond_Excess_Vw_Ret[1:r-1])
  }
  for (r in 37:nrow(Monthly_CRSP_Universe)) {
    Monthly_CRSP_Universe$Stock_inverse_sigma_hat[r] = 1/sd(Monthly_CRSP_Universe$Stock_Excess_Vw_Ret[abs(r-36):r-1])
    Monthly_CRSP_Universe$Bond_inverse_sigma_hat[r] = 1/sd(Monthly_CRSP_Universe$Bond_Excess_Vw_Ret[abs(r-36):r-1])
  }
  Monthly_CRSP_Universe[,Unlevered_k := 1/(Stock_inverse_sigma_hat+Bond_inverse_sigma_hat)]
  Monthly_CRSP_Universe[,Excess_Unlevered_RP_Ret:=Unlevered_k*Stock_inverse_sigma_hat*Stock_Excess_Vw_Ret+Unlevered_k*Bond_inverse_sigma_hat*Bond_Excess_Vw_Ret]
  vol_levered_port <- sd(Monthly_CRSP_Universe$Stock_inverse_sigma_hat * Monthly_CRSP_Universe$Stock_Excess_Vw_Ret 
                         + Monthly_CRSP_Universe$Bond_inverse_sigma_hat * Monthly_CRSP_Universe$Bond_Excess_Vw_Ret, na.rm = TRUE)
  k <- sd(Monthly_CRSP_Universe$Excess_Vw_Ret)/vol_levered_port
  Monthly_CRSP_Universe[,Levered_k := k]
  Monthly_CRSP_Universe[,Excess_Levered_RP_Ret := shift(Levered_k * Stock_inverse_sigma_hat) * Stock_Excess_Vw_Ret 
                        + Levered_k * Bond_inverse_sigma_hat * Bond_Excess_Vw_Ret]
  return(Monthly_CRSP_Universe[,c("Year","Month","Stock_Excess_Vw_Ret","Bond_Excess_Vw_Ret","Excess_Vw_Ret","Excess_60_40_Ret","Stock_inverse_sigma_hat",
                                 "Bond_inverse_sigma_hat","Unlevered_k","Excess_Unlevered_RP_Ret","Levered_k","Excess_Levered_RP_Ret")])
}

Port_Rets = PS2_Q3(Monthly_CRSP_Universe)

PS2_Q4 <- function(Port_Rets){
  Port_Rets = Port_Rets[49:1014]
  annual_avg = 12*c(mean(Port_Rets$Stock_Excess_Vw_Ret),mean(Port_Rets$Bond_Excess_Vw_Ret),mean(Port_Rets$Excess_Vw_Ret),
                 mean(Port_Rets$Excess_60_40_Ret),mean(Port_Rets$Excess_Unlevered_RP_Ret),mean(Port_Rets$Excess_Levered_RP_Ret))
  t_stat = c(t.test(Port_Rets$Stock_Excess_Vw_Ret)$statistic,t.test(Port_Rets$Bond_Excess_Vw_Ret)$statistic,t.test(Port_Rets$Excess_Vw_Ret)$statistic,
             t.test(Port_Rets$Excess_60_40_Ret)$statistic,t.test(Port_Rets$Excess_Unlevered_RP_Ret)$statistic,t.test(Port_Rets$Excess_Levered_RP_Ret)$statistic)
  annual_std = sqrt(12)*c(sd(Port_Rets$Stock_Excess_Vw_Ret),sd(Port_Rets$Bond_Excess_Vw_Ret),sd(Port_Rets$Excess_Vw_Ret),
                          sd(Port_Rets$Excess_60_40_Ret),sd(Port_Rets$Excess_Unlevered_RP_Ret),sd(Port_Rets$Excess_Levered_RP_Ret))
  annual_sr = annual_avg/annual_std
  skewness = c(skewness(Port_Rets$Stock_Excess_Vw_Ret),skewness(Port_Rets$Bond_Excess_Vw_Ret),skewness(Port_Rets$Excess_Vw_Ret),
               skewness(Port_Rets$Excess_60_40_Ret),skewness(Port_Rets$Excess_Unlevered_RP_Ret),skewness(Port_Rets$Excess_Levered_RP_Ret))
  kurtosis = c(kurtosis(Port_Rets$Stock_Excess_Vw_Ret),kurtosis(Port_Rets$Bond_Excess_Vw_Ret),kurtosis(Port_Rets$Excess_Vw_Ret),
               kurtosis(Port_Rets$Excess_60_40_Ret),kurtosis(Port_Rets$Excess_Unlevered_RP_Ret),kurtosis(Port_Rets$Excess_Levered_RP_Ret))
  output4 = data.table(
    Asset = c("CRSP stocks","CRSP bonds","Value-weighted portfolio","60/40 portfolio","unlevered RP","levered RP"),
    Annualized_Mean = annual_avg,
    t_stat_of_Annualized_Mean = t_stat,
    Annualized_Standard_Deviation = annual_std,
    Annualized_Sharpe_Ratio = annual_sr,
    Skewness = skewness,
    Excess_Kurtosis = kurtosis
  )
  return(output4)
}
PS2_Q4(Port_Rets)
