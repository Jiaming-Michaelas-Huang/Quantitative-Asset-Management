library(data.table)
library(DistributionUtils)

setwd("/Users/jiaminghuang/Downloads/")

##CRSP data calibration

CRSP_Stocks <- as.data.table(read.csv("CRSP_Data.csv"))

#data cleaning process
CRSP_Stocks$date = as.Date(as.character(CRSP_Stocks$date),format = "%Y%m%d")
CRSP_Stocks[,Year := year(CRSP_Stocks$date)]
CRSP_Stocks[,Month :=month(CRSP_Stocks$date)]
CRSP_Stocks$PRC = abs(CRSP_Stocks$PRC)
CRSP_Stocks[DLRET %in% c(-66,-77,-88,-99), DLRET := NA]
CRSP_Stocks[RET %in% c(-66,-77,-88,-99), RET := NA]



CRSP_Stocks$DLRET = as.numeric(as.character(CRSP_Stocks$DLRET))
CRSP_Stocks$RET = as.numeric(as.character(CRSP_Stocks$RET))
#data restriction
CRSP_Stocks = CRSP_Stocks[SHRCD == 10 | SHRCD == 11]
CRSP_Stocks = CRSP_Stocks[EXCHCD == 1 | EXCHCD == 2 | EXCHCD == 3]
#Return Calculation
CRSP_Stocks[(!is.na(RET))&(!is.na(DLRET)),RETURN := ((1+RET)*(1+DLRET)-1)]
CRSP_Stocks[(is.na(RET))&(!is.na(DLRET)),RETURN := DLRET]
CRSP_Stocks[(!is.na(RET))&(is.na(DLRET)),RETURN := RET]
CRSP_Stocks = CRSP_Stocks[,list(PERMNO,PERMCO,date,Year,Month,SHRCD,EXCHCD,PRC,RETURN,SHROUT)]

##Size Portfolio

#Mkt_Cap
CRSP_Stocks[,Mkt_Cap := PRC * SHROUT]
CRSP_Stocks = CRSP_Stocks[!is.na(PRC)]
CRSP_Stocks[is.na(RETURN)]$RETURN = 0

Size = CRSP_Stocks[Month == 6,list(PERMNO,Year,EXCHCD,Mkt_Cap)]
Size[,decile := 1]
NYSE_Size = Size[EXCHCD == 1]

for (y in unique(Size$Year)) {
  cut = quantile(NYSE_Size[Year == y]$Mkt_Cap,probs = seq(0,1,0.1))
  Size[Year == y & Mkt_Cap > cut[2]]$decile = 2
  Size[Year == y & Mkt_Cap > cut[3]]$decile = 3
  Size[Year == y & Mkt_Cap > cut[4]]$decile = 4
  Size[Year == y & Mkt_Cap > cut[5]]$decile = 5
  Size[Year == y & Mkt_Cap > cut[6]]$decile = 6
  Size[Year == y & Mkt_Cap > cut[7]]$decile = 7
  Size[Year == y & Mkt_Cap > cut[8]]$decile = 8
  Size[Year == y & Mkt_Cap > cut[9]]$decile = 9
  Size[Year == y & Mkt_Cap > cut[10]]$decile = 10
  print(y)
}

Size[,Pre_decile := shift(decile,1,type = "lag"),by = c("PERMNO")]

setkey(CRSP_Stocks,PERMNO,Year)
setkey(Size,PERMNO,Year)
CRSP_Stocks = merge(CRSP_Stocks,Size)
CRSP_Stocks$EXCHCD.y = NULL
CRSP_Stocks$Mkt_Cap.y = NULL
colnames(CRSP_Stocks) <- c("PERMNO","Year","PERMCO","date","Month","SHRCD","EXCHCD","PRC","RETURN","SHROUT","Mkt_Cap","decile","Pre_decile")
CRSP_Stocks[,Size_Decile:=ifelse(Month>6,decile,Pre_decile)]
CRSP_Stocks$decile = NULL
CRSP_Stocks$Pre_decile = NULL

CRSP_Stocks_Size = na.omit(CRSP_Stocks)


Result1 = CRSP_Stocks_Size[,.(Size_Ret = mean(RETURN,na.rm = TRUE)),by = c("Year","Month","Size_Decile")]

## B/M Portfolio

# Calculation of book equity value

Compustat_Stocks = data.table(read.csv("Compustat_Data.csv"))
Compustat_Stocks$datadate = as.Date(as.character(Compustat_Stocks$datadate),format = "%Y%m%d")
Compustat_Stocks = Compustat_Stocks[LINKPRIM=="P" | LINKPRIM=="C"]
PRBA_Stocks = data.table(read.csv("PRBA_Data.csv"))
colnames(PRBA_Stocks)[1] = "GVKEY"
PRBA_Stocks$datadate = as.Date(as.character(PRBA_Stocks$datadate),format = "%Y%m%d")

setkey(Compustat_Stocks,GVKEY,datadate)
setkey(PRBA_Stocks,GVKEY,datadate)

Compustat_Stocks[is.na(pstkrv)]$pstkrv = 0
Compustat_Stocks[is.na(txditc)]$txditc = 0
PRBA_Stocks[is.na(prba)]$prba = 0

Fundemental_Stocks = merge(PRBA_Stocks,Compustat_Stocks)

Fundemental_Stocks[is.na(prba)]$prba <- 0
# shareholders' equity
Fundemental_Stocks[, SHE := coalesce(seq, ceq + pstk, at - lt - mib, at - lt)]
# deferred taxes and investment tax credit
Fundemental_Stocks[, DT := coalesce(txditc, itcb + txdb, itcb, txdb)]
Fundemental_Stocks[is.na(DT)]$DT <- 0
# book value of preferred stock
Fundemental_Stocks[, PS := coalesce(pstkrv, pstkl, pstk)]
Fundemental_Stocks[is.na(PS)]$PS <- 0
# find book value

Fundemental_Stocks[,BE := SHE-PS+DT-prba]

Fundemental_Stocks = Fundemental_Stocks[BE>0]

B_M = Fundemental_Stocks[,list(datadate,LPERMCO,BE)]

B_M[,Year:=year(datadate)]
B_M$datadate = NULL

B_to_M = CRSP_Stocks[Month == 12,list(Year,PERMCO,EXCHCD,Mkt_Cap)]
colnames(B_M) = c("PERMCO","BE","Year")
setkey(B_to_M,PERMCO,Year)
setkey(B_M,PERMCO,Year)

B_to_M = merge(B_to_M,B_M)

B_to_M[,BE_ME := BE*1000000/Mkt_Cap]

B_to_M[,decile := 1]
B_to_M = na.omit(B_to_M)

NYSE_B_to_M = B_to_M[EXCHCD == 1]

for (y in unique(B_to_M$Year)) {
  cut = quantile(NYSE_B_to_M[Year == y]$BE_ME,probs = seq(0,1,0.1))
  B_to_M[Year == y & BE_ME > cut[2]]$decile = 2
  B_to_M[Year == y & BE_ME > cut[3]]$decile = 3
  B_to_M[Year == y & BE_ME > cut[4]]$decile = 4
  B_to_M[Year == y & BE_ME > cut[5]]$decile = 5
  B_to_M[Year == y & BE_ME > cut[6]]$decile = 6
  B_to_M[Year == y & BE_ME > cut[7]]$decile = 7
  B_to_M[Year == y & BE_ME > cut[8]]$decile = 8
  B_to_M[Year == y & BE_ME > cut[9]]$decile = 9
  B_to_M[Year == y & BE_ME > cut[10]]$decile = 10
  print(y)
}

B_to_M[,Pre_decile := shift(decile,1,type = "lag"),by = c("PERMCO")]

setkey(CRSP_Stocks,PERMCO,Year)
setkey(B_to_M,PERMCO,Year)
CRSP_Stocks = merge(CRSP_Stocks,B_to_M)
CRSP_Stocks$EXCHCD.y = NULL
CRSP_Stocks$Mkt_Cap.y = NULL
#
colnames(CRSP_Stocks) <- c("PERMCO","Year","PERMNO","date","Month","SHRCD","EXCHCD","PRC","RETURN","SHROUT","Mkt_Cap","Size_Decile","BE","decile","BE_ME","Pre_decile")
CRSP_Stocks[,BM_Decile:=Pre_decile]
CRSP_Stocks$decile = NULL
CRSP_Stocks$Pre_decile = NULL

CRSP_Stocks_BM = CRSP_Stocks[!is.na(BM_Decile)]

Result2 = CRSP_Stocks_BM[,.(BtM_Ret = mean(RETURN,na.rm = TRUE)),by = c("Year","Month","BM_Decile")]

## HML and SMB Portfolio
##Size decile

Size[,SMB_decile := 1]
NYSE_Size = Size[EXCHCD == 1]

for (y in unique(Size$Year)) {
  cut = quantile(NYSE_Size[Year == y]$Mkt_Cap,probs = seq(0,1,0.5))
  Size[Year == y & Mkt_Cap > cut[2]]$SMB_decile = 2
  print(y)
}

Size[,Pre_SMB_decile := shift(SMB_decile,1,type = "lag"),by = c("PERMNO")]

Size$decile = NULL
Size$Pre_decile = NULL

setkey(CRSP_Stocks,PERMNO,Year)
setkey(Size,PERMNO,Year)
CRSP_Stocks = merge(CRSP_Stocks,Size)
CRSP_Stocks$EXCHCD.y = NULL
CRSP_Stocks$Mkt_Cap.y = NULL
colnames(CRSP_Stocks) <- c("PERMNO","Year","PERMCO","date","Month","SHRCD","EXCHCD","PRC","RETURN","SHROUT","Mkt_Cap","Size_Decile","BE","BE_ME","BM_decile","SMB_decile","Pre_SMB_decile")
CRSP_Stocks[,SMB_Size_Decile:=ifelse(Month>6,SMB_decile,Pre_SMB_decile)]
CRSP_Stocks$SMB_decile = NULL
CRSP_Stocks$Pre_SMB_decile = NULL

CRSP_Stocks$BE = NULL
CRSP_Stocks$BE_ME = NULL
##BM decile
B_to_M$Pre_decile = NULL
B_to_M$decile = NULL
B_to_M[,HML_decile := 1]

NYSE_B_to_M = B_to_M[EXCHCD == 1]

for (y in unique(B_to_M$Year)) {
  cut = quantile(NYSE_B_to_M[Year == y]$BE_ME,probs = c(0,0.3,0.7,1))
  B_to_M[Year == y & BE_ME > cut[2]]$HML_decile = 2
  B_to_M[Year == y & BE_ME > cut[3]]$HML_decile = 3
  print(y)
}

B_to_M[,Pre_HML_decile := shift(HML_decile,1,type = "lag"),by = c("PERMCO")]

setkey(CRSP_Stocks,PERMCO,Year)
setkey(B_to_M,PERMCO,Year)
CRSP_Stocks = merge(CRSP_Stocks,B_to_M,allow.cartesian = T)
CRSP_Stocks$EXCHCD.y = NULL
CRSP_Stocks$Mkt_Cap.y = NULL
CRSP_Stocks$BE = NULL
CRSP_Stocks$BE_ME = NULL
colnames(CRSP_Stocks) <- c("PERMCO","Year","PERMNO","date","Month","SHRCD","EXCHCD","PRC","RETURN","SHROUT","Mkt_Cap","Size_Decile","BM_decile","SMB_Size_Decile","HML_decile","Pre_HML_decile")
CRSP_Stocks[,HML_BM_Decile:=Pre_HML_decile]
CRSP_Stocks$HML_decile = NULL
CRSP_Stocks$Pre_HML_decile = NULL

#SMB portfolio

CRSP_Stocks_HML_SMB = na.omit(CRSP_Stocks)

CRSP_Stocks_HML_SMB[,lag_Mkt_Cap := shift(Mkt_Cap),by=c("PERMNO")]
CRSP_Stocks_HML_SMB = na.omit(CRSP_Stocks_HML_SMB)

SL = CRSP_Stocks_HML_SMB[ SMB_Size_Decile == 1 & HML_BM_Decile == 1, .(SL = weighted.mean(RETURN,lag_Mkt_Cap,na.rm = TRUE)),by = c("Year","Month")]
SM = CRSP_Stocks_HML_SMB[ SMB_Size_Decile == 1 & HML_BM_Decile == 2, .(SM = weighted.mean(RETURN,lag_Mkt_Cap,na.rm = TRUE)),by = c("Year","Month")]
SH = CRSP_Stocks_HML_SMB[ SMB_Size_Decile == 1 & HML_BM_Decile == 3, .(SH = weighted.mean(RETURN,lag_Mkt_Cap,na.rm = TRUE)),by = c("Year","Month")]
BL = CRSP_Stocks_HML_SMB[ SMB_Size_Decile == 2 & HML_BM_Decile == 1, .(BL = weighted.mean(RETURN,lag_Mkt_Cap,na.rm = TRUE)),by = c("Year","Month")]
BM = CRSP_Stocks_HML_SMB[ SMB_Size_Decile == 2 & HML_BM_Decile == 2, .(BM = weighted.mean(RETURN,lag_Mkt_Cap,na.rm = TRUE)),by = c("Year","Month")]
BH = CRSP_Stocks_HML_SMB[ SMB_Size_Decile == 2 & HML_BM_Decile == 3, .(BH = weighted.mean(RETURN,lag_Mkt_Cap,na.rm = TRUE)),by = c("Year","Month")]

setkey(SL,Year,Month)
setkey(SM,Year,Month)
Result_SMB_HML = merge(SL,SM)
setkey(Result_SMB_HML,Year,Month)
setkey(SH,Year,Month)
Result_SMB_HML = merge(Result_SMB_HML,SH)
setkey(Result_SMB_HML,Year,Month)
setkey(BL,Year,Month)
Result_SMB_HML = merge(Result_SMB_HML,BL)
setkey(Result_SMB_HML,Year,Month)
setkey(BM,Year,Month)
Result_SMB_HML = merge(Result_SMB_HML,BM)
setkey(Result_SMB_HML,Year,Month)
setkey(BH,Year,Month)
Result_SMB_HML = merge(Result_SMB_HML,BH)

HML = Result_SMB_HML[,.(HML_Ret = 0.5*(SH+BH)-0.5*(SL+BL)),by = c("Year","Month")]

SMB = Result_SMB_HML[,.(SMB_Ret = (1/3)*(SL+SM+SH)-(1/3)*(BL+BM+BH)),by = c("Year","Month")]


colnames(Result1) = c("Year","Month","decile","Size_Ret")
colnames(Result2) = c("Year","Month","decile","BtM_Ret")
setkey(Result1,Year,Month,decile)
setkey(Result2,Year,Month,decile)
Result = merge(Result1,Result2)
setkey(Result,Year,Month)
setkey(HML,Year,Month)
Result = merge(Result,HML)
setkey(Result,Year,Month)
setkey(SMB,Year,Month)
Result = merge(Result,SMB)

colnames(Result) = c("Year","Month","port","Size_Ret","BtM_Ret","HML_Ret","SMB_Ret")

Result


##2. For each size decile and the long-short portfolio, 
##report the annualized average excess returns, annualized volatility, Sharpe Ratio, and skewness. 
##Also report the correlation between the port- folios that you have constructed 
##(the 10 portfolios and the long-short portfolio) and those from French???s website.

Result4_2 = Result

FF_mkt <- as.data.table(read.csv("F-F_Research_Data_Factors.csv"))
FF_mkt$date = as.Date(paste(FF_mkt$date,"-01",sep=""),format = "%Y%m-%d")
FF_mkt$Year = year(FF_mkt$date)
FF_mkt$Month = month(FF_mkt$date)
FF_mkt$date = NULL
setkey(FF_mkt,Year,Month)
setkey(Result4_2,Year,Month)

Result4_2 = merge(Result4_2,FF_mkt)

real_me = data.table(read.csv("Portfolios_Formed_on_ME.csv"))
real_me$date = as.Date(paste(real_me$X,"-01",sep=""),format = "%Y%m-%d")
real_me[,Year := year(date)]
real_me[,Month := month(date)]
real_me = real_me[,list(Year,Month,Lo.10,X2.Dec,X3.Dec,X4.Dec,X5.Dec,X6.Dec,X7.Dec,X8.Dec,X9.Dec,Hi.10)]
colnames(real_me) = c("Year","Month",1,2,3,4,5,6,7,8,9,10)
real_me = melt(real_me,id.vars = c("Year","Month"),measure.vars = colnames(real_me)[3:12])
colnames(real_me) = c("Year","Month","port","real_Size_Ret")
real_me$port = as.numeric(as.character(real_me$port))
setkey(Result4_2,Year,Month,port)
setkey(real_me,Year,Month,port)
Result4_2 = merge(Result4_2,real_me)

annual_mean = 1:11
annual_vol = 1:11
sr = 1:11
skewness = 1:11
corr = 1:11
for (d in 1:10) {
  annual_mean[d] = 12*mean(Result4_2[port == d]$Size_Ret-Result4_2[port == d]$RF/100)
  annual_vol[d] = sqrt(12)*sd(Result4_2[port == d]$Size_Ret-Result4_2[port == d]$RF/100)
  sr[d] = annual_mean[d]/annul_vol[d]
  corr[d] = cor(Result4_2[port==d]$Size_Ret,Result4_2[port==d]$real_Size_Ret)
  skewness[d] = skewness(Result4_2[port==d]$Size_Ret)
}
annual_mean[11] = 12*mean(Result4_2[port == 1]$Size_Ret-Result4_2[port == 10]$Size_Ret)
annual_vol[11] = sqrt(12)*sd(Result4_2[port == 1]$Size_Ret-Result4_2[port == 10]$Size_Ret)
sr[11] = annual_mean[11]/annual_vol[11]
skewness[11] = skewness(Result4_2[port == 1]$Size_Ret-Result4_2[port == 10]$Size_Ret)
corr[11] = cor(Result4_2[port == 1]$Size_Ret-Result4_2[port == 10]$Size_Ret,Result4_2[port==1]$real_Size_Ret-Result4_2[port==10]$real_Size_Ret)

Result_Q2 = data.table(
  Statistic = c("Excess Mean Return","Volatility","Sharp Ratio","Skewness","Correlation"),
  Decile1 = c(annual_mean[1],annual_vol[1],sr[1],skewness[1],corr[1]),
  Decile2 = c(annual_mean[2],annual_vol[2],sr[2],skewness[2],corr[2]),
  Decile3 = c(annual_mean[3],annual_vol[3],sr[3],skewness[3],corr[3]),
  Decile4 = c(annual_mean[4],annual_vol[4],sr[4],skewness[4],corr[4]),
  Decile5 = c(annual_mean[5],annual_vol[5],sr[5],skewness[5],corr[5]),
  Decile6 = c(annual_mean[6],annual_vol[6],sr[6],skewness[6],corr[6]),
  Decile7 = c(annual_mean[7],annual_vol[7],sr[7],skewness[7],corr[7]),
  Decile8 = c(annual_mean[8],annual_vol[8],sr[8],skewness[8],corr[8]),
  Decile9 = c(annual_mean[9],annual_vol[9],sr[9],skewness[9],corr[9]),
  Decile10 = c(annual_mean[10],annual_vol[10],sr[10],skewness[10],corr[10]),
  Long_Short_Portfolio = c(annual_mean[11],annual_vol[11],sr[11],skewness[11],corr[11])
)

##3. For each book-to-market decile and the long-short portfolio, 
##report the annualized average excess returns, annualized volatility, Sharpe Ratio, and skewness. 
##Also report the correlation between the portfolios that you have constructed 
##(the 10 portfolios and the long-short portfolio) and those from French???s website.

real_beme = data.table(read.csv("Portfolios_Formed_on_BE-ME.csv"))
real_beme$date = as.Date(paste(real_beme$X,"-01",sep=""),format = "%Y%m-%d")
real_beme[,Year := year(date)]
real_beme[,Month := month(date)]
real_beme = real_beme[,list(Year,Month,Lo.10,X2.Dec,X3.Dec,X4.Dec,X5.Dec,X6.Dec,X7.Dec,X8.Dec,X9.Dec,Hi.10)]
colnames(real_beme) = c("Year","Month",1,2,3,4,5,6,7,8,9,10)
real_beme = melt(real_beme,id.vars = c("Year","Month"),measure.vars = colnames(real_beme)[3:12])
colnames(real_beme) = c("Year","Month","port","real_BM_Ret")
real_beme$port = as.numeric(as.character(real_beme$port))
setkey(Result4_2,Year,Month,port)
setkey(real_beme,Year,Month,port)
Result4_2 = merge(Result4_2,real_beme)

annual_mean = 1:11
annual_vol = 1:11
sr = 1:11
skewness = 1:11
corr = 1:11
for (d in 1:10) {
  annual_mean[d] = 12*mean(Result4_2[port == d]$BtM_Ret-Result4_2[port == d]$RF/100)
  annual_vol[d] = sqrt(12)*sd(Result4_2[port == d]$BtM_Ret-Result4_2[port == d]$RF/100)
  sr[d] = annual_mean[d]/annul_vol[d]
  corr[d] = cor(Result4_2[port==d]$BtM_Ret,Result4_2[port==d]$real_BM_Ret)
  skewness[d] = skewness(Result4_2[port==d]$BtM_Ret)
}
annual_mean[11] = 12*mean(Result4_2[port == 1]$BtM_Ret-Result4_2[port == 10]$BtM_Ret)
annual_vol[11] = sqrt(12)*sd(Result4_2[port == 1]$BtM_Ret-Result4_2[port == 10]$BtM_Ret)
sr[11] = annual_mean[11]/annual_vol[11]
skewness[11] = skewness(Result4_2[port == 1]$BtM_Ret-Result4_2[port == 10]$BtM_Ret)
corr[11] = cor(Result4_2[port == 1]$BtM_Ret-Result4_2[port == 10]$BtM_Ret,Result4_2[port==1]$real_BM_Ret-Result4_2[port==10]$real_BM_Ret)

Result_Q3 = data.table(
  Statistic = c("Excess Mean Return","Volatility","Sharp Ratio","Skewness","Correlation"),
  Decile1 = c(annual_mean[1],annual_vol[1],sr[1],skewness[1],corr[1]),
  Decile2 = c(annual_mean[2],annual_vol[2],sr[2],skewness[2],corr[2]),
  Decile3 = c(annual_mean[3],annual_vol[3],sr[3],skewness[3],corr[3]),
  Decile4 = c(annual_mean[4],annual_vol[4],sr[4],skewness[4],corr[4]),
  Decile5 = c(annual_mean[5],annual_vol[5],sr[5],skewness[5],corr[5]),
  Decile6 = c(annual_mean[6],annual_vol[6],sr[6],skewness[6],corr[6]),
  Decile7 = c(annual_mean[7],annual_vol[7],sr[7],skewness[7],corr[7]),
  Decile8 = c(annual_mean[8],annual_vol[8],sr[8],skewness[8],corr[8]),
  Decile9 = c(annual_mean[9],annual_vol[9],sr[9],skewness[9],corr[9]),
  Decile10 = c(annual_mean[10],annual_vol[10],sr[10],skewness[10],corr[10]),
  Long_Short_Portfolio = c(annual_mean[11],annual_vol[11],sr[11],skewness[11],corr[11])
)

##4. Has the value and size anomaly worked in the past few years? Show some empirical evidence.

Long_Short_Size = Result4_2[port == 1 & Year >2013]$Size_Ret - Result4_2[port == 10& Year >2013]$Size_Ret
Long_Short_BM = Result4_2[port == 10& Year >2013]$BtM_Ret - Result4_2[port == 1& Year >2013]$BtM_Ret
SMB = Result4_2[port == 1& Year >2013]$SMB_Ret
HML = Result4_2[port == 1& Year >2013]$HML_Ret
Market = Result4_2[port == 1& Year >2013]$Mkt.RF/100

comparasion = data.table(
  time = 1:length(Market),
  Size = Long_Short_Size,
  BM = Long_Short_BM,
  SMB = SMB,
  HML = HML,
  Market = Market
)

library(ggplot2)

ggplot(comparasion,aes(x = time))+
  geom_line(aes(y = cumsum(Market), col = "Market"))+
  geom_line(aes(y = cumsum(Size), col = "Size"))+
  geom_line(aes(y = cumsum(BM), col = "BM"))+
  geom_line(aes(y = cumsum(SMB), col = "SMB"))+
  geom_line(aes(y = cumsum(HML), col = "HML"))+
  theme_bw()




##5. For both HML and SMB portfolios, report the annualized average excess returns, annualized volatility, Sharpe Ratio, and skewness. 
##Report correlations between the replicated factors and the factor from French???s website. Have the factors been consistent across time? Show some empirical evidence.


