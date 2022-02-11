library(tseries)
library(quantmod)
 
# FRED
firstdate = "1978-01-01"
lastdate = "2018-12-01"

# Loading returns all
dat.all <- read.csv("CBSA_All_name_state_1978-2019.csv", header = TRUE, sep = ";")
names(dat.all)[1] <- "Periods"
dat.all <- dat.all[order(dat.all$CBSAorDIV,dat.all$Periods),]
periods = seq(1,166)

# Loading Apartment
dat.apartment <- read.csv("CBSA_Apartment_name_state.csv", header = TRUE, sep = ";")
names(dat.apartment)[1] <- "Periods"
dat.apartment <- dat.apartment[order(dat.apartment$CBSAorDIV,dat.apartment$Periods),]

# Loading Hotel
dat.hotel <- read.csv("CBSA_Hotel_name_state.csv", header = TRUE, sep = ";")
names(dat.hotel)[1] <- "Periods"
dat.hotel <- dat.hotel[order(dat.hotel$CBSAorDIV,dat.hotel$Periods),]

# Loading Industrial
dat.industrial <- read.csv("CBSA_Industrial_name_state.csv", header = TRUE, sep = ";")
names(dat.industrial)[1] <- "Periods"
dat.industrial <- dat.industrial[order(dat.industrial$CBSAorDIV,dat.industrial$Periods),]

# Loading Office
dat.office <- read.csv("CBSA_Office_name_state.csv", header = TRUE, sep = ";")
names(dat.office)[1] <- "Periods"
dat.office <- dat.office[order(dat.office$CBSAorDIV,dat.office$Periods),]

# Loading Retail
dat.retail <- read.csv("CBSA_Retail_name_state.csv", header = TRUE, sep = ";")
names(dat.retail)[1] <- "Periods"
dat.retail <- dat.retail[order(dat.retail$CBSAorDIV,dat.retail$Periods),]

# Risk Free rate
getSymbols("TB3MS", src = "FRED")
dat.monthly.rf <- window(TB3MS, start= firstdate, end =lastdate)/100 + 1
dat.rf.ts <- ts(as.numeric(dat.monthly.rf), start=c(1978,1), frequency = 12)
dat.quarter.rf <- aggregate.ts(dat.rf.ts, nfrequency = 4, FUN = prod)-1

# Period series for each CBSA


dat.return <- dat.all
CBSA <- unique(dat.return$CBSAorDIV)
mat.cbsa.returns = matrix(nrow = length(periods), ncol = length(CBSA))
j = 0
cbsa.periods <- data.frame()
cbsa.returns <- data.frame()
# Create matrix of sizes (CBSA in col and periods in row)
for (x in CBSA) {
  j = j + 1
  cbsa.periods <- dat.return[dat.return$CBSAorDIV == x,]$Periods
  cbsa.returns <- dat.return[dat.return$CBSAorDIV == x,]$Total.Return
  jj= 0
  for(i in cbsa.periods) {
    jj = jj + 1
    mat.cbsa.returns[i,j] = cbsa.returns[jj]
  }
}
mat.cbsa.returns <- mat.cbsa.returns[-1,]
mat.cbsa.returns <- mat.cbsa.returns[-165,]


                           ###################
                           # Loading factors #
                           ###################


#------------------------------ 1. Liquidity (Pastor) -------------------------------------------
dat.liquidity <- read.csv("liquidity.csv", header = TRUE, sep = ";")
dat.liquidity <- dat.liquidity[match(197801,dat.liquidity$Month):match(201812,dat.liquidity$Month),]

fac.aggliquidity <- ts(as.numeric(trimws(as.character(dat.liquidity[,2])))+1,start = c(1978,1), frequency = 12)
fac.aggliquidity = aggregate.ts(fac.aggliquidity, nfrequency = 4, FUN = prod)-1
fac.innoliquidity <- ts(as.numeric(trimws(as.character(dat.liquidity[,3])))+1,start = c(1978,1), frequency = 12)
fac.innoliquidity = aggregate.ts(fac.innoliquidity, nfrequency = 4, FUN = prod)-1
fac.trdliquidity <- ts(as.numeric(trimws(as.character(dat.liquidity[,4])))+1,start = c(1978,1), frequency = 12)
fac.trdliquidity = aggregate.ts(fac.trdliquidity, nfrequency = 4, FUN = prod)-1

#------------------------------ 2. FAMA 3 factors ----------------------------------------------

# Code overturned par la section /3/

# Market factor
dat.fama <- read.csv("F-F_Research_Data_Factors.CSV", header = TRUE)
dat.fama <- dat.fama[match(197801,dat.fama$X):match(201903,dat.fama$X),]
fac.mkt <- ts(as.numeric(trimws(as.character(dat.fama[,2])))/100+1,start = c(1978,1), frequency = 12)
fac.mkt = aggregate.ts(fac.mkt, nfrequency = 4, FUN = prod)-1

# SMB factor
fac.smb <- ts(as.numeric(trimws(as.character(dat.fama[,3])))/100+1,start = c(1978,1), frequency = 12)
fac.smb = aggregate.ts(fac.smb, nfrequency = 4, FUN = prod)-1

# HML factor
fac.hml <- ts(as.numeric(trimws(as.character(dat.fama[,4])))/100+1,start = c(1978,1), frequency = 12)
fac.hml = aggregate.ts(fac.hml, nfrequency = 4, FUN = prod)-1

# ----------------------------- 3. Fama 5 factors --------------------------------------

dat.fama <- read.table("F-F_Research_Data_5_Factors_2x3.txt", header = TRUE)
dat.fama <- dat.fama[match(197801,dat.fama$Dates):match(201812,dat.fama$Dates),]

# MKT factor
fac.mkt <- ts(as.numeric(trimws(as.character(dat.fama[,2])))/100+1,start = c(1978,1), frequency = 12)
fac.mkt = aggregate.ts(fac.mkt, nfrequency = 4, FUN = prod)-1

# SMB factor
fac.smb <- ts(as.numeric(trimws(as.character(dat.fama[,3])))/100+1,start = c(1978,1), frequency = 12)
fac.smb = aggregate.ts(fac.smb, nfrequency = 4, FUN = prod)-1

# HML factor
fac.hml <- ts(as.numeric(trimws(as.character(dat.fama[,4])))/100+1,start = c(1978,1), frequency = 12)
fac.hml = aggregate.ts(fac.hml, nfrequency = 4, FUN = prod)-1

# RMW factor
fac.rmw <- ts(as.numeric(trimws(as.character(dat.fama[,5])))/100+1,start = c(1978,1), frequency = 12)
fac.rmw = aggregate.ts(fac.rmw, nfrequency = 4, FUN = prod)-1

# CMA factor
fac.cma <- ts(as.numeric(trimws(as.character(dat.fama[,6])))/100+1,start = c(1978,1), frequency = 12)
fac.cma = aggregate.ts(fac.cma, nfrequency = 4, FUN = prod)-1

#------------------------------ 4. Bond market -----------------------------------------

# Term spread

# 1 year
getSymbols("GS1", src = "FRED")
dat.1year <- window(GS1, start= firstdate, end =lastdate)/100 + 1
dat.1year.ts <- ts(as.numeric(dat.1year), start=c(1978,1), frequency = 12)
dat.1year.quarter <- aggregate.ts(dat.1year.ts, nfrequency = 4, FUN = prod)-1

# 10 years
getSymbols("GS10", src = "FRED")
dat.10year <- window(GS10, start= firstdate, end =lastdate)/100 + 1
dat.10year.ts <- ts(as.numeric(dat.10year), start=c(1978,1), frequency = 12)
dat.10year.quarter <- aggregate.ts(dat.10year.ts, nfrequency = 4, FUN = prod)-1

fac.spread <- dat.10year.quarter-dat.1year.quarter


# Credit spread

getSymbols("AAA", src = "FRED")
dat.AAA <- window(AAA, start= firstdate, end =lastdate)/100 + 1
dat.AAA.ts <- ts(as.numeric(dat.AAA), start=c(1978,1), frequency = 12)
dat.AAA.quarter <- aggregate.ts(dat.AAA.ts, nfrequency = 4, FUN = prod)-1

getSymbols("BAA", src = "FRED")
dat.BAA <- window(BAA, start= firstdate, end =lastdate)/100 + 1
dat.BAA.ts <- ts(as.numeric(dat.BAA), start=c(1978,1), frequency = 12)
dat.BAA.quarter <- aggregate.ts(dat.BAA.ts, nfrequency = 4, FUN = prod)-1

fac.credit <- dat.AAA.quarter-dat.BAA.quarter

# ----------------------------- 5. Real Estate Factor ---------------------------------------

temp = data.frame()
returns = numeric(165)
for (p in periods) {
  temp = dat.all[dat.all$Periods == p,]
  returns[p] = sum(temp$Prop.Count * temp$Total.Return)/sum(temp$Prop.Count)
}
returns <- returns[-1]
returns <- returns[-165]
fac.realestate <- returns - dat.quarter.rf


# ----------------------------- 6. Momentum --------------------------------------------------

dat.all$AvgSize <- dat.all$MV/dat.all$Prop.Count
CBSA <- unique(dat.all$CBSAorDIV)
mat.cbsa.size = matrix(nrow = length(periods), ncol = length(CBSA))
j = 0
cbsa.periods <- data.frame()
cbsa.size <- data.frame()

# Create matrix of returns (CBSA in col and periods in row)
for (x in CBSA) {
  j = j + 1
  cbsa.periods <- dat.all[dat.all$CBSAorDIV == x,]$Periods
  cbsa.size <- dat.all[dat.all$CBSAorDIV == x,]$AvgSize
  jj= 0
  for(i in cbsa.periods) {
    jj = jj + 1
    mat.cbsa.size[i,j] = cbsa.size[jj]
  }
}
mat.cbsa.size <- mat.cbsa.size[-1,]
mat.cbsa.size <- mat.cbsa.size[-165,]
colnames(mat.cbsa.size) = CBSA
colnames(mat.cbsa.returns) = CBSA

vec.size <- numeric(length(CBSA))
vec.returns <- numeric(length(CBSA))
fac.mom <- numeric(164)

rebalance = seq(3,164,2)
for (i in rebalance) {
  
  for (x in 1:length(CBSA)){
    vec.size[x] = (mat.cbsa.size[i-1,x]+mat.cbsa.size[i-2,x])/2
    vec.returns[x] = (mat.cbsa.returns[i-1,x]+mat.cbsa.returns[i-2,x])/2
  }
  med.size = median(vec.size, na.rm = TRUE)
  med.ret = median(vec.returns, na.rm = TRUE)
  
  group1 <- as.integer(vec.size <= med.size & vec.returns <= med.ret)*CBSA
  temp <- which(group1>0)
  group1 <- group1[temp]
  for (j in 1:length(group1)) {group1[j] = toString(group1[j])}
  
  group2 <- as.integer(vec.size > med.size & vec.returns <= med.ret)*CBSA
  temp <- which(group2>0)
  group2 <- group2[temp]
  for (j in 1:length(group2)) {group2[j] = toString(group2[j])}
  
  group3 <- as.integer(vec.size <= med.size & vec.returns > med.ret)*CBSA
  temp <- which(group3>0)
  group3 <- group3[temp]
  for (j in 1:length(group3)) {group3[j] = toString(group3[j])}
  
  group4 <- as.integer(vec.size > med.size & vec.returns > med.ret)*CBSA
  temp <- which(group4>0)
  group4 <- group4[temp]
  for (j in 1:length(group4)) {group4[j] = toString(group4[j])}
  
  fac.mom[i] = mean(mat.cbsa.returns[i,group3], na.rm = TRUE) + 
    mean(mat.cbsa.returns[i,group4], na.rm = TRUE) -
    mean(mat.cbsa.returns[i,group1], na.rm = TRUE) - 
    mean(mat.cbsa.returns[i,group2], na.rm = TRUE)
  fac.mom[i+1] = mean(mat.cbsa.returns[i+1,group3], na.rm = TRUE) + 
    mean(mat.cbsa.returns[i+1,group4], na.rm = TRUE) -
    mean(mat.cbsa.returns[i+1,group1], na.rm = TRUE) - 
    mean(mat.cbsa.returns[i+1,group2], na.rm = TRUE)
}

# ----------------------------- 7. Macro ----------------------------------------------------

# GDP
getSymbols("A191RL1Q225SBEA", src = "FRED")
dat.gdp <- window(A191RL1Q225SBEA, start= firstdate, end =lastdate)/100
fac.gdp <- ts(as.numeric(dat.gdp), start=c(1978,1), frequency = 4)

# Inflation
getSymbols("CPIAUCSL", src = "FRED")
getSymbols("UNRATE", src = "FRED")

dat.cpi <- log(CPIAUCSL)
dat.cpi.rates <- numeric(length(dat.cpi)-1)
for (i in 1:length(dat.cpi.rates)) {dat.cpi.rates[i] = as.double(dat.cpi[i+1])-as.double(dat.cpi[i])}
dat.cpi.ts = ts(dat.cpi.rates,start=c(1947,2),frequency = 12)
y <- window(dat.cpi.ts, start= c(1978,1), end =c(2018,12))
x1 <- window(dat.cpi.ts, start= c(1977,12), end =c(2018,11))
x2 <- window(dat.cpi.ts, start= c(1977,7), end =c(2018,6))
x3 <- window(dat.cpi.ts, start= c(1977,6), end =c(2018,5))
x4 <- window(dat.cpi.ts, start= c(1977,4), end =c(2018,3))
x5 <- window(dat.cpi.ts, start= c(1977,2), end =c(2018,1))

dat.unrate <- window(UNRATE, start= "1977-01-01", end =lastdate)/100
dat.unrate.ts <- ts(as.numeric(dat.unrate), start=c(1977,1), frequency = 12)
x6 <- window(dat.unrate.ts, start= c(1977,12), end =c(2018,11))
x7 <- window(dat.unrate.ts, start= c(1977,2), end =c(2018,1))

reg.inflation <- lm(y ~ x1+x2+x3+x4+x5+x6+x7)
fac.unexinf <- aggregate.ts(ts(residuals(reg.inflation)+1, start=c(1978,1), frequency = 12), nfrequency = 4, FUN = prod)-1
fac.exinf <- aggregate.ts(ts(fitted(reg.inflation)+1, start=c(1978,1), frequency = 12), nfrequency = 4, FUN = prod)-1
fac.exinf2 = numeric(length(fac.exinf))
for (i in 1:(length(fac.exinf)-1)) {fac.exinf2[i+1] = as.double(fac.exinf[i+1]-fac.exinf[i])}
fac.exinf2[1] = mean(fac.exinf2)


# Consumption
getSymbols("PCEC", src = "FRED")
dat.pcec <- log(window(PCEC, start= "1977-10-01", end ="2018-10-01"))
fac.pcec = numeric(length(dat.pcec)-1)
for (i in 1:length(fac.pcec)) {fac.pcec[i] = as.double(dat.pcec[i+1])-as.double(dat.pcec[i])}

# WTI
getSymbols("WTISPLC", src = "FRED")
dat.wti <- log(window(WTISPLC, start= "1977-12-01", end ="2018-12-01"))
dat.wti2 <- numeric(length(dat.wti)-1)
for (i in 1:length(dat.wti2)) {dat.wti2[i] = as.double(dat.wti[i+1])-as.double(dat.wti[i])}
dat.wti.ts = ts(dat.wti2+1, start = c(1978,1), frequency = 12)
fac.wti = aggregate.ts(dat.wti.ts, nfrequency = 4, FUN = prod)-1


# ----------------------------- 8. Uncertainty -----------------------------------------------

dat.mu <- read.csv("MacroUncertaintyToCirculate.csv", header = TRUE, sep = ",")
dat.fu <- read.csv("FinancialUncertaintyToCirculate.csv", header = TRUE, sep = ",")
dat.ru <- read.csv("RealUncertaintyToCirculate.csv", header = TRUE, sep = ",")

# Macro
v <- log(dat.mu$h.12)
w <- numeric(length(v)-1)
for (i in 1:length(w)) {w[i] = v[i+1]-v[i]}
w2 <- window(ts(w, start=c(1960,8), frequency = 12),start=c(1978,1), end=c(2018,12))+1
fac.mu = aggregate.ts(w2, nfrequency = 4, FUN = prod)-1

# Financial
v <- log(dat.fu$h.12)
w <- numeric(length(v)-1)
for (i in 1:length(w)) {w[i] = v[i+1]-v[i]}
w2 <- window(ts(w, start=c(1960,8), frequency = 12),start=c(1978,1), end=c(2018,12))+1
fac.fu = aggregate.ts(w2, nfrequency = 4, FUN = prod)-1


# ----------------------------- Build the matrix --------------------------------------------
n.fac = 18 # To update as factors are added
fac.periods = length(fac.mkt) # 1978 to 2018
mat.factors <- matrix(nrow = fac.periods,ncol = n.fac)
mat.factors[,1] = fac.mkt
mat.factors[,2] = fac.smb
mat.factors[,3] = fac.hml
mat.factors[,4] = fac.rmw
mat.factors[,5] = fac.cma
mat.factors[,6] = fac.credit
mat.factors[,7] = fac.spread
mat.factors[,8] = fac.aggliquidity
mat.factors[,9] = fac.innoliquidity
mat.factors[,10] = fac.trdliquidity
mat.factors[,11] = fac.realestate
mat.factors[,12] = fac.mom
mat.factors[,13] = fac.gdp
mat.factors[,14] = fac.unexinf
mat.factors[,15] = fac.exinf2
mat.factors[,16] = fac.pcec
mat.factors[,17] = fac.wti
mat.factors[,18] = fac.mu
mat.factors[,19] = fac.fu

CBSA <- unique(dat.return$CBSAorDIV)
mat.cbsa.returns = matrix(nrow = length(periods), ncol = length(CBSA))
j = 0
cbsa.periods <- data.frame()
cbsa.returns <- data.frame()
# Create matrix of sizes (CBSA in col and periods in row)
for (x in CBSA) {
  j = j + 1
  cbsa.periods <- dat.return[dat.return$CBSAorDIV == x,]$Periods
  cbsa.returns <- dat.return[dat.return$CBSAorDIV == x,]$Total.Return
  jj= 0
  for(i in cbsa.periods) {
    jj = jj + 1
    mat.cbsa.returns[i,j] = cbsa.returns[jj]
  }
}
mat.cbsa.returns <- mat.cbsa.returns[-1,]
mat.cbsa.returns <- mat.cbsa.returns[-165,]
