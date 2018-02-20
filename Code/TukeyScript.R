# load R package and function
source("./utility.R")

# load R data
load("../Data/Tukey.Rdata")


## set hyperparameter before running Tukey =====
# set the rebalance frequecy 
# PERIOD = "Yearly"
# PERIOD = "Quarterly"
PERIOD = "Monthly"
# PERIOD = "Daily"

# set the dollars per transaction (Please refer our paper for detail)
psntTransPerStock = 1

# set investment period (from 1958-01-02 to 2015-12-31)
FROM = "1958-01-02"
TO = "2015-12-31"

# run the Tukey Script and get return ======
CumuResultList = getReturnMat(FROM, TO, PERIOD, funclist)
ReturnSeries = CumuResultList$CumuMat  


# Analysis the return data  ========
# get culmulative return in billion
tail(ReturnSeries) / 10^9

# get fees in terms of bidask spread, administration fee and transaction fee in million
# Please refer to our paper (Page 318) for more detail
CumuResultList$BidAsk / 10^6
CumuResultList$Admin / 10^6
CumuResultList$Transaction / 10^6

# plot the cumulative return in the log scale
# The results are also saved to the folder named "FROM_TO"
getCumuPlt(Year = 5, ReturnSeries, PERIOD)

# Compute yearly return and other performance statistics 
# such as geometric mean, arithmetic mean, standard deviation
# sharp ratio, yearly VaR, monthly VaR, daily VaR, 
# yearly cVaR, monthly cVaR and daily cVaR.
# The results are saved to the folder named "FROM_TO"
r = getPerformance(ReturnSeries, PERIOD)

# Please refer to "SimuFunctions.cpp" on how we simulate 2e4 Tukey bootstraps efficiently via C++
