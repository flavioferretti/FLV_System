#!/usr/bin/env Rscript
#
#options(warn=0)
#
# WD need to be change between different hosts
setwd("/home/flavio/Downloads")
# args stuff
args = commandArgs(trailingOnly = TRUE)
# test if there is at least one argument: if not, return an error
if (length(args) == 0) {
  stop("arg[1]=( Symbol p.es AAPL  ) arg[2]=(data tail p.es 1200) )",
       call. = FALSE)
}
 

## load libraries ##====================================================
library(RMariaDB)
library(quantstrat)
library(PerformanceAnalytics) # performance and risk management
#library(ssh)
## load flv_lib
source("http://192.168.4.1/~flavio/lib/lib_flv.R")

## chart param
my_chart_zoom = 'last 1 weeks'

## Downloading Stock Ticker Data from  MariaDB ##=======================
# getSymbols.MySQL parameters
db = 'PriceDB'
param_db = 'parameters'
host = '192.168.4.1'
user = 'flavio'
pw = '27182818'
#getSymbols.MySQL("AAPL",env = globalenv(), user = user, password = pw, dbname = db, port = 3306, host = host)
getSymbols.MySQL(
  args[1],
  env = globalenv(),
  user = user,
  password = pw,
  dbname = db,
  port = 3306,
  host = host
)
MyTickerName <- .Last.value # keep them
####====================================================================

########################################################################
## BT/OPTIMIZATION PARAMETERS ##========================================
STG_NAME = "smaCX"
ResultSet = 12
MinimumProfitFactor 		=   1.4
OptimizationMinimumReturns 	=   1.0 #%
OptimizationMaximumDD 		=  10.0 #%


## prepare data #=======================================================
#SYMBL <- na.omit(get(MyTickerName))
#print(paste(
#  "> Ticker Name:",
#  MyTickerName,
#  " Total sample lenght:",
#  nrow(SYMBL)
#))
#SYMBL <- tail(SYMBL, 500)
#SYMBL <- tail(SYMBL, as.numeric(args[2]))
####====================================================================


stock.str=MyTickerName # what are we trying it on
currency('USD')
stock(stock.str,currency='USD',multiplier=1)

startDate=as.Date(Sys.time())-as.numeric(args[2])
initEq=10000

#rm.strat(portfolio.st, silent = FALSE)

portfolio.st='macross'
account.st='macross'
initPortf(portfolio.st,symbols=stock.str)
#> [1] "macross"
initAcct(account.st,portfolios=portfolio.st, initEq=initEq)
#> [1] "macross"
initOrders(portfolio=portfolio.st)
stratMACROSS<- strategy(portfolio.st)
# add indicators
stratMACROSS <- add.indicator(strategy = stratMACROSS, name = "SMA", arguments = list(x=quote(Cl(mktdata)), n=50),label= "ma50" )
stratMACROSS <- add.indicator(strategy = stratMACROSS, name = "SMA", arguments = list(x=quote(Cl(mktdata)[,1]), n=200),label= "ma200")

stratMACROSS <- add.signal(strategy = stratMACROSS,name="sigCrossover",arguments = list(columns=c("ma50","ma200"), relationship="gte"),label="ma50.gt.ma200")
stratMACROSS <- add.signal(strategy = stratMACROSS,name="sigCrossover",arguments = list(column=c("ma50","ma200"),relationship="lt"),label="ma50.lt.ma200")

stratMACROSS <- add.rule(strategy = stratMACROSS,name='ruleSignal', arguments = list(sigcol="ma50.gt.ma200",sigval=TRUE, orderqty=1, ordertype='market', orderside='long'),type='enter')
stratMACROSS <- add.rule(strategy = stratMACROSS,name='ruleSignal', arguments = list(sigcol="ma50.lt.ma200",sigval=TRUE, orderqty='all', ordertype='market', orderside='long'),type='exit')

# if you want a long/short Stops and Reverse MA cross strategy, you would add two more rules for the short side:

# stratMACROSS <- add.rule(strategy = stratMACROSS,name='ruleSignal', arguments = list(sigcol="ma50.lt.ma200",sigval=TRUE, orderqty=-1, ordertype='market', orderside='short'),type='enter')
# stratMACROSS <- add.rule(strategy = stratMACROSS,name='ruleSignal', arguments = list(sigcol="ma50.gt.ma200",sigval=TRUE, orderqty=all, ordertype='market', orderside='short'),type='exit')

# exec
for(i in stock.str)
  assign(i, adjustOHLC(get(i),use.Adjusted=TRUE))

start_t<-Sys.time()
out<-applyStrategy(strategy=stratMACROSS , portfolios=portfolio.st)
#
end_t<-Sys.time()
print(end_t-start_t)


big_portfolio <- ''
  big_returns <- trace_in_sample <- i <- j <- k <- l <-  NULL
opt_pass_counter = 0
SMA_RANGE      = c(seq(from = 10, to = 20, by = 10))
SMA_RANGE_SLOW = c(seq(from = 20, to = 40, by = 20))
LAG_RANGE      = c(seq(from = 0, to = 2, by = 1))
LAG_RANGE_SLOW  = c(seq(from = 0, to = 2, by = 1))

 
#parse_date(b, "%d/%m/%Y")
#b <- print(b)

#return()
#

## Read list of symbols from stock_automate DB
my_query = paste("SELECT `date`
FROM", stock.str,"
ORDER BY `date` DESC
LIMIT 1
")
b <- ReadParamOnMyDB(my_query, user, pw, db, host, "")
print(toString( b[,1]))
typeof(b)

# chart
start_t<-Sys.time()
updatePortf(Portfolio='macross',Dates=paste(startDate,'::',as.Date(Sys.time())-3,sep=''))
#> [1] "macross"
end_t<-Sys.time()
print("trade blotter portfolio update:")
#> [1] "trade blotter portfolio update:"
print(end_t-start_t)
#> Time difference of 0.03264308 secs



png(
  file = paste0("Chart.png"),
  width = 1280,
  height = 800
)

chart.Posn(Portfolio='macross',Symbol=args[1], TA=c("add_SMA(n=50,col='red')","add_SMA(n=200,col='blue')"))

dev.off()
