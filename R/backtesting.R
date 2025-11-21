
## ---- backtest_logRet-fn
backtest_log_ret <- function(x){
  price <- x$price
  signal   <- x$signal
  
  ret <- suppressWarnings(diff(log(price))) # Crude oil negative day gets dropped later bc NaN
  
  trade <- lag(signal, 1)
  
  strat <- na.omit(merge(ret, trade, join = "inner"))
  strat_ret <- strat[,1] * strat[,2]
  
  return(strat_ret)
}

## ---- Evel_backtest-fn
eval_backtest <- function(x) {
  r <- exp(x) - 1
  sharpe <- SharpeRatio.annualized(r, scale = 252, Rf = 0)
  mdd <- maxDrawdown(r)
  
  return(list(
    Sharpe = as.numeric(sharpe),
    MaxDrawdown = as.numeric(mdd)
  ))
}


## ---- create_portfolio-fn
create_portfolio <- function(x){
  Rlog <- do.call(merge, c(x, list(all = FALSE)))
  r <- exp(Rlog) - 1 
  
  #Equal Weight
  portfolio_R <- xts(rowMeans(r, na.rm = TRUE), order.by = index(r))
  portfolio_Rlog <- log(portfolio_R + 1)
  
  return(portfolio_Rlog)
}




