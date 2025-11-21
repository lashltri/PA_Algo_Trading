
library(quantmod)
library(PerformanceAnalytics)
library(lubridate)
library(fGarch)
source("R/strategies.R")
source("R/backtesting.R")
source("R/Tuning.R")
source("R/ARMA_GARCH/ARMA_GARCH_Forecasts.R")
source("R/Hypothesis_Testing.R")
source("R/Plots.R")

#-----------------------------Config--------------------------------------------

symbols   <- c("^SSMI","^GSPC","^IXIC","^GDAXI","CL=F","NG=F","GC=F","SI=F","HG=F")
date_from <- "2000-01-01"
date_to   <- "2009-12-31"
date_from_to <- paste0(date_from, "::", date_to)

STRATS <- list()

grid_MA <- expand.grid(
  n1 = c(10, 20, 30, 40, 50),
  n2 = c(80, 100, 150, 200, 250)
)
grid_MA <- subset(grid_MA, n1 < n2)

PARAMS <- list(
  MA   = grid_MA,
  BB   = expand.grid(n = c(10, 20, 30, 40), 
                     k = c(1.5, 2, 2.5, 3)),
  grid_MACD <- expand.grid(nFast = c(6, 8, 10, 12, 15),
                           nSlow = c(18, 20, 26, 30, 35),
                           nSig  = c(9, 10, 12)
  )
  
)



#---------------------------------Data------------------------------------------  
getSymbols(symbols, src="yahoo", from= date_from, to=date_to, auto.assign=TRUE)

px_list <- list(
  "SSMI"  = `SSMI`,
  "GSPC"  = `GSPC`,
  "IXIC"  = `IXIC`,
  "GDAXI" = `GDAXI`,
  "CL=F"   = `CL=F`,
  "NG=F"   = `NG=F`,
  "GC=F"   = `GC=F`,
  "SI=F"   = `SI=F`,
  "HG=F"   = `HG=F`
)
rm(SSMI, GSPC, IXIC, GDAXI, `CL=F`, `NG=F`, `GC=F`, `SI=F`, `HG=F`)


px_list <- lapply(px_list, function(x) na.approx(x, maxgap=5))

#lapply(px_list, function(x)any(is.na(c)))         
px_list$`GC=F`$`GC=F.Adjusted`[px_list$`GC=F`$`GC=F.Adjusted`<= 0]<-0.01


#--------------------------indicators and signals-------------------------------
STRATS$BH <- lapply(px_list, strat_bh)

STRATS$MA <- Map(function(x, nm) {
  cat("Running:", nm, "\n")
  tune_rolling_window_LB(price       = x,
                         param_grid  = PARAMS$MA, 
                         strategyfun = strat_ma, 
                         backtestfun = backtest_log_ret,
                         tuning_window = 2)
}, px_list, names(px_list))


STRATS$BB <- Map(function(x, nm) {
  cat("Running:", nm, "\n")
  tune_rolling_window_LB(price       = x,
                         param_grid  = PARAMS$BB,
                         strategyfun = strat_bb,
                         backtestfun = backtest_log_ret,
                         tuning_window = 2)
}, px_list, names(px_list))

STRATS$MACD <- Map(function(x, nm) {
  cat("Running:", nm, "\n")
  tune_rolling_window_LB(price       = x,
                         param_grid  = grid_MACD,
                         strategyfun = strat_macd,
                         backtestfun = backtest_log_ret,
                         tuning_window = 2)
}, px_list, names(px_list))


# Backtests ---------------------------------------------------------------

BACKT <- list(BH = lapply(STRATS$BH, function(x){backtest_log_ret(x)}),
              MA = lapply(STRATS$MA, function(x){backtest_log_ret(x)}),
              BB = lapply(STRATS$BB, function(x){backtest_log_ret(x)}),
              MACD = lapply(STRATS$MACD, function(x){backtest_log_ret(x)}))


#For comparability we want all TS to have intersecting dates
all_series <- unlist(BACKT, recursive = FALSE)
common_idx <- Reduce(intersect, lapply(all_series, function(x) as.Date(index(x))))
BACKT <- lapply(BACKT, function(group) lapply(group, function(x) x[common_idx]))
rm(all_series)


# ARIMA GARCH------------------------------------------------------------
# This strategy requires extra manual care so it circumvents the pipeline

BACKT$AG <- list(SSMI = strat_ag_arma_garch(price = px_list$SSMI$SSMI.Adjusted,
                                            model = "arma(1,0) + garch(1,1)",
                                            start_out_sample = "2005-01-18"),
                 GSPC = strat_ag_arma_garch(price = px_list$GSPC$GSPC.Adjusted,
                                            model = "arma(0,1) + garch(1,1)",
                                            start_out_sample = "2005-01-18"),
                 IXIC = strat_ag_arma_garch(price = px_list$IXIC$IXIC.Adjusted,
                                            model = "arma(0,1) + garch(1,1)",
                                            start_out_sample = "2005-01-18"),
                 GDAXI = strat_ag_arma_garch(price = px_list$GDAXI$GDAXI.Adjusted,
                                             model = "arma(1,0) + garch(1,1)",
                                             start_out_sample = "2005-01-18"),
                 `CL=F` = strat_ag_arma_garch(price = px_list$`CL=F`$`CL=F.Adjusted`,
                                              model = "arma(1,0) + garch(1,1)",
                                              start_out_sample = "2005-01-18"),
                 `NG=F` = strat_ag_arma_garch(price = px_list$`NG=F`$`NG=F.Adjusted`,
                                              model = "arma(1,0) + garch(1,1)",
                                              start_out_sample = "2005-01-18"),
                 `GC=F` = strat_ag_arma_garch(price = px_list$`GC=F`$`GC=F.Adjusted`,
                                              model = "arma(1,0) + garch(1,1)",
                                              start_out_sample = "2005-01-18"),
                 `SI=F` = strat_ag_arma_garch(price = px_list$`SI=F`$`SI=F.Adjusted`,
                                              model = "arma(1,0) + garch(1,1)",
                                              start_out_sample = "2005-01-18"),
                 `HG=F` = strat_ag_arma_garch(price = px_list$`HG=F`$`HG=F.Adjusted`,
                                              model = "arma(1,0) + garch(1,1)",
                                              start_out_sample = "2005-01-18")
)



# Create Portfolio -------------------------------------------------------      

PORTF <- lapply(BACKT, create_portfolio)
PORTF <- do.call(merge, c(PORTF, list(all = F)))


# charts.PerformanceSummary(exp(PORTF$BH)-1)
# charts.PerformanceSummary(exp(PORTF$BB)-1)
# charts.PerformanceSummary(exp(PORTF$MA)-1)


# Evaluate Backtest Index---------------------------------------------------

EVAL_BT <- list(
  BH = lapply(BACKT$BH, eval_backtest),
  MA = lapply(BACKT$MA, eval_backtest),
  BB = lapply(BACKT$BB, eval_backtest),
  AG = lapply(BACKT$AG, eval_backtest),
  MACD = lapply(BACKT$MACD, eval_backtest)
)

EVAL_PORTF <- list(
  BH = lapply(PORTF$BH, eval_backtest),
  MA = lapply(PORTF$MA, eval_backtest),
  BB = lapply(PORTF$BB, eval_backtest),
  AG = lapply(PORTF$AG, eval_backtest),
  MACD = lapply(PORTF$MACD, eval_backtest)
)

# Extract Sharpe ratios and Drawdowns into matrices 

mat_sharpe <- sapply(EVAL_BT, function(x) sapply(x, function(y) y$Sharpe))
mat_drawdown <- sapply(EVAL_BT, function(x) sapply(x, function(y) y$MaxDrawdown))

# Add portfolio rows
mat_sharpe <- rbind(mat_sharpe,
                    sapply(EVAL_PORTF, function(x) sapply(x, function(y) y$Sharpe)))
rownames(mat_sharpe)[nrow(mat_sharpe)] <- "portfolio"

mat_drawdown <- rbind(mat_drawdown,
                      sapply(EVAL_PORTF, function(x) sapply(x, function(y) y$MaxDrawdown)))
rownames(mat_drawdown)[nrow(mat_drawdown)] <- "portfolio"

mat_sharpe
mat_drawdown


# Hypothesis Testing  Backtest Index-----------------------------------------
STD_RET <- standardized_returns(returns = BACKT)

grid <- expand.grid(A = names(STD_RET), B = names(STD_RET), stringsAsFactors = FALSE)
grid <- subset(grid, A != B)

ALL_TVALS_DIR <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
  A <- grid$A[i]; B <- grid$B[i]
  out <- paired_t_value(STD_RET[[A]], STD_RET[[B]])
  transform(out, strat_A = A, strat_B = B)
}))

