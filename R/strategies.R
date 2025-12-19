#--------------------------------Buy Hold---------------------------------------
strat_bh <- function(price) {
  price <- Ad(price)

  signal <- rep(1, length(price))
  signal <- reclass(signal, price)

  
  list(
    price    = price,
    signal   = signal,
    param    = NA,
    strategy = NA
  )
}

#----------------------------MACD Strategy-------------------------------------
## ---- strat_macd-fn

strat_macd <- function(param, price) {
  nFast <- param[1]   # kurzer EMA (z. B. 12)
  nSlow <- param[2]   # langer EMA (z. B. 26)
  nSig  <- param[3]   # Signal-EMA (z. B. 9)
  
  price <- Ad(price)
  
  # MACD
  macd_obj <- TTR::MACD(price,
                        nFast = nFast,
                        nSlow = nSlow,
                        nSig  = nSig,
                        maType = "EMA")
  
  macd_line   <- macd_obj[, "macd"]
  signal_line <- macd_obj[, "signal"]
  
  # MACD-Crossing signals
  signal <- ifelse(macd_line > signal_line,  1,
                   ifelse(macd_line < signal_line, 0, 0))
  signal <- reclass(signal, price)
  

  param_xts <- xts::xts(
    cbind(nFast = rep(nFast, NROW(price)),
          nSlow = rep(nSlow, NROW(price)),
          nSig  = rep(nSig, NROW(price))),
    order.by = index(price)
  )
  
  list(
    price    = price,
    signal   = signal,
    param    = param_xts,
    strategy = macd_obj
  )
}



# ---- strat_ag-fn
# sigma_t_AR_GARCH<-function(y_out,fit, y_in) # recursive sigma_t and y_t_hat
# {
#   # Vola based on in-sample parameters
#   sigma_t<-rep(NA,length(y_out))
#   a<-fit@fit$coef["beta1"]
#   b<-fit@fit$coef["alpha1"]
#   d<-fit@fit$coef["omega"]
#   
#   if (is.na(fit@fit$coef["mu"])){
#     mu <- 0
#   }else mu <- fit@fit$coef["mu"]
#   
#   ar <- fit@fit$coef["ar1"]
#   sigma_t  <- rep(NA, length(y_out))
#   y_t_dach <- rep(NA, length(y_out))
#   eps_t    <- rep(NA, length(y_out))
#   
#   # First data point: based on last in-sample data
#   sigma_t[1]<-sqrt(d+a*fit@sigma.t[length(fit@sigma.t)]^2+
#                      b*fit@residuals[length(fit@residuals)]^2)
#   y_t_dach[1] <- mu + ar * y_in[length(y_in)]
#   eps_t[1] <- y_out[1] - y_t_dach[1]
#   
#   # On out-of-sample span
#   for (i in 2:length(y_out))
#   {
#     sigma_t[i]<-sqrt(d+a*sigma_t[i-1]^2+b*eps_t[i-1]^2)
#     
#     y_t_dach[i]<- mu + ar * y_out[i-1]
#     eps_t[i] <- y_out[i] - y_t_dach[i]
#     
#   }
#   # Transform sigma_t into xts object
#   out_xts <- cbind(
#     reclass(sigma_t, y_out),
#     reclass(y_t_dach, y_out)
#   )
#   colnames(out_xts) <- c("sigma_t", "y_t_hat")
#   
#   return(out_xts)
# }
# 
# # AR GARCh Strategy Function
# strat_ag_ar_garch <- function(price, 
#                               start_in_sample = min(common_idx),
#                               return_signal = F){
#   price <- Ad(price)
#   pos <- which(index(price) == start_in_sample)
#   
#   
#   log_ret <- na.omit((suppressWarnings(diff(log(price)))))
#   log_ret_in<-log_ret[paste("/",start_in_sample,sep="")]
#   #Lower by one day because AR(1)
#   log_ret_out<-log_ret[paste(index(price)[pos - 1],"/",sep="")] 
#   
#   # Model definition
#   AR_GARCH<-garchFit(~arma(1,0)+garch(1,1),data=log_ret_in,delta=2,
#                      include.delta=F,include.mean=T,  cond.dist = "sstd")
#   
#   
#   # Forecasts 
#   #predict yt and vola out of sample to create scaled positions
#   pred <-sigma_t_AR_GARCH(y_in = log_ret_in, 
#                           fit = AR_GARCH, 
#                           y_out = log_ret_out)
#   
#   # Position sizing and Signal generation
#   position_size<-1/pred$sigma_t
#   position_size<-position_size/max(position_size) #Scaling
#   
#   signal <- ifelse(sign(pred$y_t_hat)>0, position_size, 0)
#   signal     <- reclass(signal, log_ret_out)
#   
#   # Backtest
#   arma_garch_log_ret<-signal*log_ret_out
#   
#   if (return_signal == F) {
#     return(arma_garch_log_ret[paste(start_in_sample,"/",sep="")])
#   }else{
#     list(
#       price    = price[paste(start_in_sample,"/",sep="")],
#       signal   = signal[paste(start_in_sample,"/",sep="")],
#       param    = NA,
#       strategy = pred$y_t_hat[paste(start_in_sample,"/",sep="")],
#       vola = pred$sigma_t[paste(start_in_sample,"/",sep="")]
#     )
#   }
#   
# }
# 
# 
# ## ---- end
#----------------------------Moving Average-------------------------------------
## ---- strat_ma-fn
strat_ma <- function(param,
                     price) {
  n1 <- param[1]
  n2 <- param[2]
  price <- Ad(price)
  

  sma_short <- SMA(price, n1)
  sma_long  <- SMA(price, n2)

  
  signal <- ifelse(sma_short > sma_long,  1,
                   ifelse(sma_short < sma_long, 0, 0))
  signal <- reclass(signal, price)
  
  param <- cbind(rep(n1, length(signal)), rep(n2, length(signal)))
  colnames(param) <- c("N1", "N2")
  
  strategy <- cbind(sma_short, sma_long)
  colnames(strategy) <- c("SMA short", "SMA long")
  
  return(list(price = price,
              signal = signal,
              param = param,
              strategy = strategy))
}


#-----------------------------BB - Bands-------------------------------------
## ---- strat_bb-fn

strat_bb <- function(param, price) {
  N <- param[1]
  K <- param[2]

  price <- Ad(price)                         # xts, 1 column
  b_bands <- BBands(price, n = N, sd = K)    # xts with up, dn, mavg, pctB

  generate_signal_bb <- function(bb, p) {
    n <- NROW(p)

    # Start with a numeric vector of the same length as price
    new_sig <- rep(0L, n)

    # Crossings (positions as integers)
    up_cross   <- which((p > bb$up) & (quantmod::Lag(p)  < quantmod::Lag(bb$up)))
    down_cross <- which((p < bb$dn) & (quantmod::Lag(p)  > quantmod::Lag(bb$dn)))

    # If no crossings at all: return zeros (length == n)
    if (length(up_cross) == 0L && length(down_cross) == 0L) {
      return(new_sig)
    }

    # Helper: safe "next index >= k"
    next_ge <- function(vec, k) {
      ix <- which(vec > k)
      if (length(ix)) vec[ix[1]] else NA_integer_
    }

    # Initial end_sig = earliest crossing of either type
    end_sig   <- min(c(up_cross, down_cross))
    start_sig <- 1L

    # Iterate across at most the total number of crossings
    total_steps <- length(up_cross) + length(down_cross)
    for (i in seq_len(total_steps)) {
      if (!is.finite(end_sig)) break

      # If next signal is a BUY (upper-band cross), fill previous block with -1; else +1
      is_buy_next <- any(end_sig == up_cross)
      if (is_buy_next) {
        new_sig[start_sig:end_sig] <- 0L #-1L falls long/short
      } else {
        new_sig[start_sig:end_sig] <-  1L
      }

      # Advance start
      start_sig <- end_sig + 1L
      if (start_sig > n) break

      # Choose next end depending on what we just placed
      end_sig <- if (is_buy_next) {
        next_ge(down_cross, end_sig)
      } else {
        next_ge(up_cross, end_sig)
      }

      # If no more crossings, extend to the end with opposite sign
      if (!is.finite(end_sig) || is.na(end_sig)) {
        if (is_buy_next) {
          # we just placed -1 up to previous end; next was buy -> now SELL to end
          new_sig[start_sig:n] <- 1L
        } else {
          new_sig[start_sig:n] <- 0L #-1L falls long/short
        }
        break
      }
    }

    new_sig
  }

  # Compute signal as numeric vector, then wrap back into xts with same index as price
  signal_vec <- generate_signal_bb(b_bands, price)
  signal <- xts::xts(signal_vec, order.by = index(price))
  colnames(signal) <- "signal"

  # Parameters per row (as 1-col xts will avoid length surprises)
  param_xts <- xts::xts(
    cbind(K = rep(K, NROW(signal)), N = rep(N, NROW(signal))),
    order.by = index(price)
  )

  list(
    price    = price,
    signal   = signal,    
    param    = param_xts,
    strategy = b_bands
  )
}




  

