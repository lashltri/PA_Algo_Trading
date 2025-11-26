
## ---- strat_ag-fn

#_________ AR-GArch Recursive sigma_t and y_t_hat _________

sigma_t_ARMA_GARCH <- function(y_out, fit, y_in) 
{
  # grab coefficients (default to 0 if missing)
  a <- fit@fit$coef["beta1"]   # GARCH term on sigma^2
  b <- fit@fit$coef["alpha1"]  # ARCH term on eps^2
  d <- fit@fit$coef["omega"]
  mu <- if (is.na(fit@fit$coef["mu"])) 0 else fit@fit$coef["mu"]
  ar <- if (is.na(fit@fit$coef["ar1"])) 0 else fit@fit$coef["ar1"]
  ma <- if (is.na(fit@fit$coef["ma1"])) 0 else fit@fit$coef["ma1"]
  
  n <- length(y_out)
  sigma_t  <- rep(NA_real_, n)
  y_t_dach <- rep(NA_real_, n)
  eps_t    <- rep(NA_real_, n)
  
  # seed from last in-sample points
  last_sigma <- tail(fit@sigma.t, 1)
  last_eps   <- tail(fit@residuals, 1)
  last_y_in  <- tail(y_in, 1)
  
  # t = 1 (first OOS step)
  sigma_t[1]  <- sqrt(d + a*last_sigma^2 + b*last_eps^2)
  y_t_dach[1] <- mu + ar*as.numeric(last_y_in) + ma*as.numeric(last_eps)
  eps_t[1]    <- as.numeric(y_out[1] - y_t_dach[1])
  
  # t = 2..n
  for (i in 2:n) {
    sigma_t[i]  <- sqrt(d + a*sigma_t[i-1]^2 + b*eps_t[i-1]^2)
    y_t_dach[i] <- mu + ar*as.numeric(y_out[i-1]) + ma*eps_t[i-1]
    eps_t[i]    <- as.numeric(y_out[i] - y_t_dach[i])
  }
  
  # return as xts (same shape/names as your original)
  out_xts <- cbind(
    reclass(sigma_t,  y_out),
    reclass(y_t_dach, y_out)
  )
  colnames(out_xts) <- c("sigma_t", "y_t_hat")
  out_xts
}

# ____________ ARMA-GARCH Strategy Function ________________
strat_ag_arma_garch <- function(price, 
                              model,
                              start_out_sample = min(common_idx),
                              return_strat = F){
  price <- Ad(price)
  pos <- which(index(price) == start_out_sample)
  
  
  log_ret <- na.omit((suppressWarnings(diff(log(price)))))
  log_ret_in<-log_ret[paste("/",start_out_sample,sep="")]
  #Lower by one day because AR(1)
  log_ret_out<-log_ret[paste(index(price)[pos - 1],"/",sep="")] 
  

  # Model definition
  ARMA_GARCH<-garchFit(as.formula(paste("~", model)),data=log_ret_in,delta=2,
                     include.delta=F,include.mean=T,  cond.dist = "sstd")
  
  
  # Forecasts 
  #predict yt and vola out of sample to create scaled positions
  pred <-sigma_t_ARMA_GARCH(y_in = log_ret_in, 
                          fit = ARMA_GARCH, 
                          y_out = log_ret_out)

  # Position sizing and Signal generation
  position_size<-1/pred$sigma_t
  position_size<-position_size/mean(position_size) #Scaling out of sample not in but negligible difference
  
  signal <- ifelse(sign(pred$y_t_hat)>0, position_size, 0)
  signal     <- reclass(signal, log_ret_out)
  
  # Backtest
  arma_garch_log_ret<-signal*log_ret_out
  
  if (return_strat == F) {
    return(arma_garch_log_ret[paste(start_out_sample,"/",sep="")])
  }else{
    list(
      return = log_ret_out[paste(start_out_sample,"/",sep="")],
      price    = price[paste(start_out_sample,"/",sep="")],
      signal   = signal[paste(start_out_sample,"/",sep="")],
      param    = ARMA_GARCH@fit$coef,
      strategy = pred$y_t_hat[paste(start_out_sample,"/",sep="")],
      vola = pred$sigma_t[paste(start_out_sample,"/",sep="")]
    )
  }
  
}



