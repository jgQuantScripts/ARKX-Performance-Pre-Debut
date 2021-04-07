require("pbapply");require("quantmod");require("PerformanceAnalytics")
# ARKX performance Pre ETF Debut
# https://ark-funds.com/arkx

# download ETF Constituents & Weights
url = paste0("https://ark-funds.com/wp-content/fundsiteliterature/csv/",
             "ARK_SPACE_EXPLORATION_&_INNOVATION_ETF_ARKX_HOLDINGS.csv")
etf <- read.csv(url,header=TRUE,sep=",")
etf <- na.omit(etf)

# sum of weights
sum(etf[,ncol(etf)])

# change to YF ticker symbol 
# Meituan - Class B (MPNGF)
# source: https://www.nasdaq.com/market-activity/stocks/mpngf
# source: https://stockmarketmba.com/analyze.php?s=HK:3690
etf$ticker <- gsub("3690","3690.HK",etf$ticker)

# Komatsu Ltd ADR (KMTUY)
# source: https://www.nasdaq.com/market-activity/stocks/kmtuy
# https://stockmarketmba.com/analyze.php?s=JT:6301
etf$ticker <- gsub("6301","6301.T",etf$ticker)

# Garmin Ltd. Common Stock (Switzerland) (GRMN)
# source: https://www.nasdaq.com/market-activity/stocks/grmn
# source: https://stockmarketmba.com/analyze.php?s=GRMN
etf$ticker <- gsub("GRMN U","GRMN",etf$ticker)

# Dassault Systèmes SE (DSY)
# source: https://stockmarketmba.com/analyze.php?s=FP:DSY
etf$ticker <- gsub("DSY","DSY.PA",etf$ticker)

# Morgan Stanley Instl Lqudty Govt Instl (MVRXX) <- comparable
# https://ycharts.com/mutual_funds/M:MVRXX
# etf$ticker[39] <- "MVRXX"
# no historical data for mutual funds :/

# subset ticker & weights only
etf_w <- etf[,c("ticker","weight...")]
colnames(etf_w) <- c("ticker","weight")
etf_w$weight <- etf_w$weight/100
# drop MVRXX
etf_w <- etf_w[-nrow(etf_w),]

# get historical data for tickers
tickers <- etf_w$ticker
e <- new.env()  # environment to send historical data
getSymbols(Symbols = tickers, from="2010-01-01", env = e)

# merge Adjusted close - 
allAdj <- do.call(merge,eapply(e,Ad))
colnames(allAdj) <- gsub(".Adjusted","",names(allAdj))
# backfill missing data
allAdj <- na.locf(allAdj,fromLast = TRUE)
allAdj <- na.locf(allAdj)
# Calculate % returns
retsAdj <- na.omit(ROC(allAdj,type="discrete"))
# add CASH column for missing weight %
CASH <- as.data.frame(cbind("CASH",1-sum(etf_w$weight)))
colnames(CASH) <- c("ticker","weight")
etf_w <- rbind(etf_w,CASH)
etf_w$weight <- as.numeric(etf_w$weight)
sum(etf_w$weight)
# add CASH column -> we will use 0 returns for CASH
CASH <- reclass(rep(0,nrow(retsAdj)),match.to = retsAdj)
colnames(CASH) <- "CASH"
# add it to rets df
retsAdj <- merge(retsAdj,CASH)
# transpose weights (in the same order as retsAdj)
etf_w$ticker <- gsub("6301.T","X6301.T",etf_w$ticker)
etf_w$ticker <- gsub("3690.HK","X3690.HK",etf_w$ticker)

tWts <- t(etf_w) # transpose
ord <- tWts[1,]  # order of ticker symbols 
tWts <- as.numeric(tWts[2,]) # Weights
# put the retsAdj in the same order
retsAdj <- retsAdj[,ord]

# multiply stock returns by weights 
# **note the following will translate in keeping weights the same 
#  each day which may not reflect the actual ETF weights each day**

ARKX <- reclass(coredata(retsAdj) %*% tWts, match.to = retsAdj)
colnames(ARKX)<-"ARKX"
charts.PerformanceSummary(ARKX, geometric = FALSE)

# get a benchmark to compare it to
BM <- ROC(Ad(getSymbols("SPY",from="2010-01-01",auto.assign = FALSE)),
           type="discrete")
BM[is.na(BM)]<-0
colnames(BM) <- gsub(".Adjusted","",names(BM))
comp <- merge(ARKX,BM)
comp[is.na(comp)] <-0
# plot against benchmark
charts.PerformanceSummary(comp, geometric = FALSE)

# plot against benchmark - 2020 -> forward
charts.PerformanceSummary(comp["2020::"], geometric = FALSE)

# View Stats
View(table.Stats(comp))
# correlation to benchmark
table.Correlation(Ra=comp$ARKX, Rb=comp[,2])

# *********************************************************************
#                           MONTHLY
# *********************************************************************
ticsMO <- etf$ticker[-length(etf$ticker)]
moPRC <- lapply(as.list(ticsMO), function(x) Ad(to.monthly(e[[x]],name = x)))
moPRC <- do.call(merge,moPRC)
colnames(moPRC) <- gsub(".Adjusted","",names(moPRC))
# backfill missing data
moPRC <- na.locf(moPRC,fromLast = TRUE)
moPRC <- na.locf(moPRC)
# Calculate % returns
moRetsAdj <- na.omit(ROC(moPRC,type="discrete"))

# add CASH column -> we will use 0 returns for CASH
CASH <- reclass(rep(0,nrow(moRetsAdj)),match.to = moRetsAdj)
colnames(CASH) <- "CASH"
# add it to rets df
moRetsAdj <- merge(moRetsAdj,CASH)

tWts <- t(etf_w) # transpose
ord <- tWts[1,]  # order of ticker symbols 
tWts <- as.numeric(tWts[2,]) # Weights
# put the retsAdj in the same order
moRetsAdj <- moRetsAdj[,ord]

# multiply stock returns by weights 
# **monthly rebalancing might be more feasible**

moARKX <- reclass(coredata(moRetsAdj) %*% tWts, match.to = moRetsAdj)
colnames(moARKX)<-"ARKX"
charts.PerformanceSummary(moARKX, geometric = FALSE)
# get a benchmark to compare it to
ticker = "SPY"
BM <- ROC(Ad(to.monthly(getSymbols(ticker,from="2010-01-01",auto.assign = FALSE),
                        name = ticker)),type="discrete")
BM[is.na(BM)]<-0
colnames(BM) <- gsub(".Adjusted","",names(BM))
comp <- merge(moARKX,BM)
comp[is.na(comp)] <-0
# plot against benchmark
charts.PerformanceSummary(comp, geometric = FALSE)
# Stats
View(table.Stats(comp))