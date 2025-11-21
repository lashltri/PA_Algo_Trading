## ---- strat_ag_crisis_tuned-fn

strat_ag_crisis_tuned <- function(price, 
                                  crisis_symbol,
                                  model = "arma(1,0) + garch(1,1)",
                                  start_out_sample = min(common_idx),
                                  crisis_from = "2000-01-01",
                                  crisis_to   = "2004-01-01",
                                  return_strat = FALSE) 
{
  require(quantmod)
  require(fGarch)
  
  #-------------------------------------------------
  # 1) OUT-OF-SAMPLE DATA (your asset)
  #-------------------------------------------------
  price   <- Ad(price)
  log_ret <- na.omit(diff(log(price)))
  
  # where to start OOS on your asset (same logic as before)
  pos <- which(index(price) == start_out_sample)
  
  # Lower by one day because AR(1) structure
  log_ret_out <- log_ret[paste(index(price)[pos - 1], "/",
                               sep = "")]
  
  #-------------------------------------------------
  # 2) IN-SAMPLE CRISIS DATA (downloaded, only for tuning)
  #-------------------------------------------------
  crisis_px <- suppressWarnings(
    getSymbols(
      Symbols     = crisis_symbol,
      src         = "yahoo",
      from        = crisis_from,
      to          = crisis_to,
      auto.assign = FALSE
    )
  )
  
  crisis_ret <- na.omit(diff(log(Ad(crisis_px))))
  
  if (NROW(crisis_ret) < 50) {
    stop("Not enough crisis data for ARMA–GARCH fitting.")
  }
  
  #-------------------------------------------------
  # 3) Fit ARMA–GARCH on CRISIS RETURNS
  #-------------------------------------------------
  ARMA_GARCH <- garchFit(
    as.formula(paste("~", model)),
    data          = crisis_ret,
    delta         = 2,
    include.delta = FALSE,
    include.mean  = TRUE,
    cond.dist     = "sstd",
    trace         = FALSE
  )
  
  #-------------------------------------------------
  # 4) Recursive forecasts on YOUR OUT-OF-SAMPLE RETS
  #-------------------------------------------------
  pred <- sigma_t_ARMA_GARCH(
    y_in  = crisis_ret,   # in-sample: crisis
    fit   = ARMA_GARCH,
    y_out = log_ret_out   # out-of-sample: your asset
  )
  
  #-------------------------------------------------
  # 5) Position sizing and signal generation
  #-------------------------------------------------
  position_size <- 1 / pred$sigma_t
  position_size <- position_size / mean(position_size, na.rm = TRUE)
  
  signal <- ifelse(sign(pred$y_t_hat) > 0, position_size, 0)
  signal <- reclass(signal, log_ret_out)
  
  #-------------------------------------------------
  # 6) Backtest
  #-------------------------------------------------
  arma_garch_log_ret <- signal * log_ret_out
  
  if (!return_strat) {
    return(arma_garch_log_ret[paste(start_out_sample, "/", sep = "")])
  } else {
    return(list(
      price    = price[paste(start_out_sample, "/", sep = "")],
      signal   = signal[paste(start_out_sample, "/", sep = "")],
      param    = NA,
      strategy = pred$y_t_hat[paste(start_out_sample, "/", sep = "")],
      vola     = pred$sigma_t[paste(start_out_sample, "/", sep = "")]
    ))
  }
}

