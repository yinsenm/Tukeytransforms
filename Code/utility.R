## install packages: If installed, then loading
list.of.packages <- c("scales", "reshape2", "xts", "ggplot2", "plyr", "lubridate",
                      "latex2exp", "quantmod", "xtable", "latex2exp", "ggthemes", 
                      "PerformanceAnalytics")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


## Compute annual performance and save them into local directory===
getPerformance = function(ReturnSeries, PERIOD) {
  FROM =  head(index(ReturnSeries), 1)
  TO   =  tail(index(ReturnSeries), 1)
  tag = paste(format(FROM, "%Y_%m"), format(TO, "%Y_%m"), sep = "_")
  
  ## Compute Table 14 & 15===
  YearReturn = vector("list", ncol(ReturnSeries))
  for(i in 1:length(YearReturn)) {
    tda = ReturnSeries[,i]
    YearReturn[[i]] = annualReturn(tda)
  }
  M = matrix(unlist(YearReturn), ncol = ncol(ReturnSeries))
  AnnualReturn = as.data.frame(M)
  colnames(AnnualReturn) = colnames(ReturnSeries)
  rownames(AnnualReturn) = unique(year(index(ReturnSeries)))
  dir.create(sprintf("./%s", tag), showWarnings = FALSE)
  print(xtable(AnnualReturn * 100, digits = 2), 
        file = paste0("./", tag, "/", PERIOD, "AnnualReturn", tag, ".tex"))
  print(xtable(AnnualReturn * 100, digits = 2), type = "html", 
        file = paste0("./", tag, "/", PERIOD, "AnnualReturn", tag, ".html"))
  
  ## Compute Table 8, 9, 10 and Table 12, 13
  ## Compute return matrix based for different periods
  DailyReturnList = lapply(1:8, function(x) dailyReturn(ReturnSeries[,x]))
  DailyReturnMat  = Reduce(cbind, DailyReturnList)
  funcstr = c("1/x2", "1/x", "1/sqrt", "log", "EQU", "sqrt", "MKC", "x2")
  colnames(DailyReturnMat) = funcstr
  
  MonthlyReturnList = lapply(1:8, function(x) monthlyReturn(ReturnSeries[,x]))
  MonthlyReturnMat  = Reduce(cbind, MonthlyReturnList)
  colnames(MonthlyReturnMat) = funcstr
  
  YearlyReturnList = lapply(1:8, function(x) yearlyReturn(ReturnSeries[,x]))
  YearlyReturnMat  = Reduce(cbind, YearlyReturnList)
  colnames(YearlyReturnMat) = funcstr
  
  
  sharpRatios = SharpeRatio(YearlyReturnMat, Rf = 0.0175)[1,]
  
  dVAR = VaR(DailyReturnMat, p = 0.95, method = "historical")
  mVAR = VaR(MonthlyReturnMat, p = 0.95, method = "historical")
  yVAR = VaR(YearlyReturnMat, p = 0.95, method = "historical")
  dCVAR = CVaR(DailyReturnMat, p = 0.95, method = "historical")
  mCVAR = CVaR(MonthlyReturnMat, p = 0.95, method = "historical")
  yCVAR = CVaR(YearlyReturnMat, p = 0.95, method = "historical")
  
  arithMean = apply(YearlyReturnMat, 2, mean)
  geomMean = apply(YearlyReturnMat, 2, function(x) ((prod(x+1))^(1/length(x))) - 1)
  standSD = apply(YearlyReturnMat, 2, sd)
  
  
  AnnalStat = data.frame(geomMean  = geomMean, 
                         arithMean = arithMean, 
                         standSD   = standSD,
                         sharpRatios = as.numeric(sharpRatios),
                         yVAR = as.numeric(yVAR),
                         mVAR = as.numeric(mVAR),
                         dVaR = as.numeric(dVAR),
                         yCVAR = as.numeric(yCVAR),
                         mCVAR = as.numeric(mCVAR),
                         dCVaR = as.numeric(dCVAR))
  
  print(xtable(AnnalStat * 100, digits = 2), 
        file = paste0("./", tag, "/", PERIOD, "AnnualSummary", tag, ".tex"))
  print(xtable(AnnalStat * 100, digits = 2), type = "html", 
        file = paste0("./", tag, "/", PERIOD, "AnnualSummary", tag, ".html"))
  list(AnnalStat = AnnalStat, AnnualReturn = AnnualReturn)
}


funclist = list(function(x) 1/(x^2), function(x) 1/(x), function(x) 1/sqrt(x),
                function(x) log(x),  function(x) indict(x), function(x) sqrt(x), function(x) x, function(x) x^2)
funcstr = c("MKC 1/x2", "MKC 1/x", "MKC 1/sqrt", "MKC log", "EQU", "MKC sqrt", "MKC", "MKC x2")

## dollarInflate function for computing money worth ====
dollarInflate = function(bgn, end, bgn_val, CPI = CPI) {
  bgn = as.Date(bgn)
  end = as.Date(end)
  bgnCPI = as.numeric(CPI[bgn])
  endCPI = as.numeric(CPI[end])
  bgn_val * (bgnCPI / endCPI)
} 

## Reccession Periods====
recessions.df = read.table(textConnection(
  "Peak, Trough
  1926-10-01, 1927-11-01
  1929-08-01, 1933-03-01
  1937-05-01, 1938-06-01
  1945-02-01, 1945-10-01
  1948-11-01, 1949-10-01
  1953-07-01, 1954-05-01
  1957-08-01, 1958-04-01
  1960-04-01, 1961-02-01
  1969-12-01, 1970-11-01
  1973-11-01, 1975-03-01
  1980-01-01, 1980-07-01
  1981-07-01, 1982-11-01
  1990-07-01, 1991-03-01
  2001-03-01, 2001-11-01
  2007-12-01, 2009-06-01"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)

## Indicator Function: For EQU====
indict <- function(x){
  naidx = which(is.na(x))
  y = rep(1, length(x))
  y[naidx] = NA
  y
}

## Print Cumulative return Table ====
getCumuPrt = function(ReturnSeries, PERIOD) {
  FROM =  head(index(ReturnSeries), 1)
  TO   =  tail(index(ReturnSeries), 1)
  ReturnMatrix = as.data.frame(ReturnSeries)
  ReturnMatrix$Datadate = index(ReturnSeries)
  tag = paste(format(FROM, "%Y_%m"), format(TO, "%Y_%m"), sep = "_")
  bgnInvest = ReturnMatrix[1,1]
  tempM = tail(ReturnMatrix, 10)
  row.names(tempM) = tempM$Datadate
  caption = sprintf("The cumulative for Tukey transformation portfolios from %s (assumed $%.3f invested) to %s.", as.character(FROM), 
                    bgnInvest, as.character(TO))
  print(xtable(tempM[,1:8], caption = caption, digits = 2),
        file = paste0("./", tag, "/", PERIOD, "Cumulative_", tag, "_RET.tex"))
  print(xtable(tempM[,1:8], caption = caption, digits = 2), type = "html",
        file = paste0("./", tag, "/", PERIOD, "Cumulative_", tag, "_RET.html"))
  print(xtable(tempM[,1:8], display = rep("E", 9), caption = caption, digits = 2), 
        file = paste0("./", tag, "/", PERIOD, "Cumulative_", tag, "_RETExp.tex"))
  print(xtable(tempM[,1:8], display = rep("E", 9), caption = caption, digits = 2), type = "html",
        file = paste0("./", tag, "/", PERIOD, "Cumulative_", tag, "_RETExp.html"))
  return(NULL)
}

## Print Cumulative Return Figure ====
getCumuPlt = function(Year = 5, ReturnSeries, PERIOD) {
  FROM =  head(index(ReturnSeries), 1)
  TO   =  tail(index(ReturnSeries), 1)
  label = list(TeX("$1/x^{2}$"), TeX("$1/x$"), TeX("$1/\\sqrt{x}$"), TeX("$\\log(x)$"), 
               TeX("EQU"), TeX("\\sqrt{x}"), TeX("$x$ (MKC)"), TeX("$x^{2}$"))
  tag = paste(format(FROM, "%Y_%m"), format(TO, "%Y_%m"), sep = "_")
  ReturnMatrix = as.data.frame(ReturnSeries)
  ReturnMatrix$Datadate = index(ReturnSeries)
  df <- melt(ReturnMatrix, id.vars="Datadate", value.name = "values")
  ord = order(ReturnMatrix[nrow(ReturnMatrix),-ncol(ReturnMatrix)], decreasing = T)
  df$variable <- factor(df$variable,levels = levels(df$variable)[ord])
  recess = recessions.df[recessions.df$Peak > FROM, ]
  ## log base = 10
  plt = ggplot(df) + geom_line(aes(x = Datadate, y = log10(values),
                                   colour = variable), size = .4) + 
    geom_rect(data = recess, aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = +Inf), fill='red', alpha=0.1) +
    scale_x_date(breaks = date_breaks(paste(Year,"years")), labels = date_format("%Y")) +
    labs(title = paste("S&P 500 from", format(FROM, "%Y-%m"), "to", 
                       format(TO, "%Y-%m"),", Rebalance: ", PERIOD), x = "Year", y = TeX("$\\log_{10}$(Dollars)")) +
    scale_color_discrete(name="Weighting Methods", breaks = levels(df$variable),labels = label[ord]) +
    scale_y_continuous(breaks = seq(1, round(log10(max(df$value)), digits = 3) + 0.25, by = 0.5)) +
    # scale_y_log10() +
    theme_bw() +
    # theme_minimal() +
    theme(legend.position="top", 
          plot.title = element_text(size = 20, face = "bold", hjust = 0),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 15)) +
    guides(col = guide_legend(nrow = 1, byrow = T))
  dir.create(sprintf("./%s", tag), showWarnings = FALSE)
  ggsave(paste0("./", tag, "/", PERIOD, "Cumulative_", tag, "_Log10.pdf"), height = 6, width = 10)
  plt
}


## get return list for one strategy====
getReturn = function(FUNC = function(x) 1/x^2, PRCS, SHRS, RETS, BIDASKSPD, PERIOD) {
  dataDate = index(PRCS)
  PRCM = t(as.matrix(PRCS))
  SHRM = t(as.matrix(SHRS))
  RETM = t(as.matrix(RETS))
  BIDASKM = t(as.matrix(BIDASKSPD))
  addNum = 0
  delNum = 0
  chgNum = 0
  money = 0
  nBalanced = 0
  WeightList = list()
  BalancedDate = list()
  AdminList = list()
  BidAskList = list()
  TransacList = list()
  CumuReturn = matrix(0, nrow(PRCS), 1)
  DayReturn = matrix(0, nrow(PRCS), 1)
  CumuReturn[1] = dollarInflate(dataDate[1], as.Date("1958-01-02"), 10^5, CPI = CPI)
  
  
  if(PERIOD == "Daily") {
    periodFunc = day
  } else if (PERIOD == "Monthly") {
    periodFunc = month
  } else if (PERIOD == "Quarterly") {
    periodFunc = quarter
  } else if (PERIOD == "Yearly") {
    periodFunc = year
  } else {
    stop("Period can only be chosen from Daily, Monthly Quaterly, Yearly")
  }
  
  PeriodADJ = periodFunc(dataDate[1]) + 1
  Weight = PRCM[,1] * SHRM[,1] / sum(PRCM[,1] * SHRM[,1], na.rm = T)
  NewWeight = FUNC(Weight) / sum(FUNC(Weight), na.rm = T)
  NewWeight[is.na(NewWeight)] = 0
  ## save NewWeight and rebalanced date
  nBalanced = nBalanced + 1
  WeightList[[nBalanced]] = NewWeight
  BalancedDate[[nBalanced]] = as.character(dataDate[1])
  
  # nrow(PRCS)
  for(i in c(2:nrow(PRCS))) {
    # print(i)  
    DayReturn[i] = sum(RETM[,i] * NewWeight, na.rm = T)
    
    ## account for deleted and added companies
    bgn = which(is.na(PRCM[, i-1]) == F)
    end = which(is.na(PRCM[, i]) == F)
    deleted = setdiff(bgn, end)
    added   = setdiff(end, bgn)
    
    if(length(deleted) != 0 & length(added) != 0) {
      chgNum = chgNum + 1;
    } else if (length(deleted) == 0 & length(added) != 0) {
      addNum = addNum + 1;
    } else if (length(deleted) != 0 & length(added) == 0) {
      delNum = delNum + 1;
    }
    
    if(length(deleted) != 0 | length(added) != 0) {
      Weight = PRCM[,i] * SHRM[,i] / sum(PRCM[,i] * SHRM[,i], na.rm = T)
      NewWeight = FUNC(Weight) / sum(FUNC(Weight), na.rm = T)
      NewWeight[is.na(NewWeight)] = 0
    }
    
    if(PERIOD == "Daily" | periodFunc(dataDate[i]) == PeriodADJ) {
      nBalanced = nBalanced + 1
      Weight = PRCM[,i] * SHRM[,i] / sum(PRCM[,i] * SHRM[,i], na.rm = T)
      NewWeight =  FUNC(Weight) / sum(FUNC(Weight), na.rm = T)
      NewWeight[is.na(NewWeight)] = 0
      WeightList[[nBalanced]] = NewWeight
      BalancedDate[[nBalanced]] = as.character(dataDate[i])
      prc = PRCM[,i]
      bidaskspd = PRCM[,i] / 2000
      money = CumuReturn[i-1]
      diffMoney = money  * (WeightList[[nBalanced - 1]]  - WeightList[[nBalanced]])
      
      adminFee = - dollarInflate(dataDate[i], dataDate[length(dataDate)], psntTransPerStock, CPI = CPI)  * sum(!is.na(prc), na.rm = T)
      bidaskspdFee = -sum((abs(diffMoney / prc) * bidaskspd), na.rm = T)
      transactionFee  =  adminFee + bidaskspdFee
      CumuReturn[i] = (money + transactionFee) *  (1 + DayReturn[i])
      AdminList[[nBalanced]] = adminFee
      BidAskList[[nBalanced]] = bidaskspdFee
      TransacList[[nBalanced]] = transactionFee
    } else {
      CumuReturn[i] = CumuReturn[i-1] *  (1 + DayReturn[i])
    }  
    
    if(PERIOD != "Daily" & periodFunc(dataDate[i]) == PeriodADJ) {
      PeriodADJ = PeriodADJ + 1
      if(PERIOD == "Monthly" & PeriodADJ == 13) {
        PeriodADJ = 1
      } else if(PERIOD == "Quarterly" & PeriodADJ == 5) {
        PeriodADJ = 1
      }
    }      
  }
  list(CumuReturn = xts(CumuReturn, order.by = dataDate), 
       DayReturn = xts(DayReturn, order.by = dataDate), 
       nBalanced = nBalanced, 
       ChangeStat = c(addNum, delNum, chgNum),
       Admin= sum(unlist(AdminList)),
       BidAsk = sum(unlist(BidAskList)), Transaction = sum(unlist(TransacList)))
}

## get return mat for multiple strategies====
getReturnMat <- function(FROM, TO, PERIOD, funclist) {
  SHRSS = abs(SHRS[paste0(FROM,"/",TO)])
  PRCSS = abs(PRCS[paste0(FROM,"/",TO)])
  RETSS = RETS[paste0(FROM,"/",TO)]
  BIDASKSPDS = BIDASKSPD[paste0(FROM,"/",TO)]
  
  ResultList = lapply(funclist, function(x) {
    print(x)
    getReturn(x, PRCSS, SHRSS, RETSS, BIDASKSPDS, PERIOD)
  })
  CumuList = lapply(ResultList, function(x) x$CumuReturn)
  CumuMat = Reduce(cbind, CumuList)
  colnames(CumuMat) = funcstr
  nBalanced = ResultList[[1]]$nBalanced
  ChangeStat = ResultList[[1]]$ChangeStat
  Admin = unlist(lapply(ResultList, function(x) x$Admin))
  BidAsk = unlist(lapply(ResultList, function(x) x$BidAsk))
  Transaction = unlist(lapply(ResultList, function(x) x$Transaction))
  list(CumuMat = CumuMat, nBalanced = nBalanced, ChangeStat = ChangeStat,
       Admin = Admin, BidAsk = BidAsk, Transaction = Transaction)
}

# ## Table 11=====
# ## Number of S&P 500 constituents whose stock price increased by at least 50%, 100% or 200%
# PRC = PRCS["1958-01-02/2015-12-31"]
# AnnualReturnMatrix = matrix(NA, nrow = length(1958:2015), ncol = ncol(PRC))
# rownames(AnnualReturnMatrix) = 1958:2015
# 
# 
# for(i in 1:ncol(PRC)) {
#   prc = PRC[,i]
#   anReturn = annualReturn(prc)
#   AnnualReturnMatrix[as.character(year(index(anReturn))),i] = as.numeric(anReturn)
# }
# ## the number of companies whose annual return is larger than 50%
# d1 = apply(AnnualReturnMatrix, 1, function(x) sum(x > 0.5, na.rm = T))
# ## the number of companies whose annual return is larger than 100%
# d2 = apply(AnnualReturnMatrix, 1, function(x) sum(x > 1, na.rm = T))
# ## the number of companies whose annual return is larger than 200%
# d3 = apply(AnnualReturnMatrix, 1, function(x) sum(x > 2, na.rm = T))
# rownames(AnnualReturnMatrix) = 1958:2015
# colnames(AnnualReturnMatrix) = colnames(PRCS)
# CompanyPerformanceMat = cbind(d1,d2,d3)
# colnames(CompanyPerformanceMat) = c("Over50", "Over100", "Over200")
# print(xtable(CompanyPerformanceMat, digits = 3), file = paste0("./Table_11.tex"))
# print(xtable(CompanyPerformanceMat, digits = 3),  type = "html", file = paste0("./Table_11.html"))
