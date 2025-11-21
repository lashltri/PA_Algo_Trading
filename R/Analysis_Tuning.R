
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

symbols   <- c("^SSMI","^GSPC","^IXIC","^GDAXI","BTC-USD","CL=F","NG=F","GC=F","SI=F","HG=F")
date_from <- "2010-01-01"
date_to   <- "2024-12-31"
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
  "BTC-USD"= `BTC-USD`,
  "CL=F"   = `CL=F`,
  "NG=F"   = `NG=F`,
  "GC=F"   = `GC=F`,
  "SI=F"   = `SI=F`,
  "HG=F"   = `HG=F`
)
rm(SSMI, GSPC, IXIC, GDAXI, `BTC-USD`, `CL=F`, `NG=F`, `GC=F`, `SI=F`, `HG=F`)


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
                         backtestfun = backtest_log_ret)
}, px_list, names(px_list))


STRATS$BB <- Map(function(x, nm) {
  cat("Running:", nm, "\n")
  tune_rolling_window_LB(price       = x,
                         param_grid  = PARAMS$BB,
                         strategyfun = strat_bb,
                         backtestfun = backtest_log_ret)
}, px_list, names(px_list))

STRATS$MACD <- Map(function(x, nm) {
  cat("Running:", nm, "\n")
  tune_rolling_window_LB(price       = x,
                         param_grid  = grid_MACD,
                         strategyfun = strat_macd,
                         backtestfun = backtest_log_ret)
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
                                            model = "arma(1,0) + garch(1,1)"),
                 GSPC = strat_ag_arma_garch(price = px_list$GSPC$GSPC.Adjusted,
                                            model = "arma(0,1) + garch(1,1)"),
                 IXIC = strat_ag_arma_garch(price = px_list$IXIC$IXIC.Adjusted,
                                            model = "arma(0,1) + garch(1,1)"),
                 GDAXI = strat_ag_arma_garch(price = px_list$GDAXI$GDAXI.Adjusted,
                                             model = "arma(1,0) + garch(1,1)"),
                 `BTC-USD` = strat_ag_arma_garch(price = px_list$`BTC-USD`$`BTC-USD.Adjusted`,
                                              model = "arma(1,1) + garch(1,1)"),
                 `CL=F` = strat_ag_arma_garch(price = px_list$`CL=F`$`CL=F.Adjusted`,
                                              model = "arma(1,0) + garch(1,1)"),
                 `NG=F` = strat_ag_arma_garch(price = px_list$`NG=F`$`NG=F.Adjusted`,
                                              model = "arma(1,0) + garch(1,1)"),
                 `GC=F` = strat_ag_arma_garch(price = px_list$`GC=F`$`GC=F.Adjusted`,
                                              model = "arma(1,0) + garch(1,1)"),
                 `SI=F` = strat_ag_arma_garch(price = px_list$`SI=F`$`SI=F.Adjusted`,
                                              model = "arma(1,0) + garch(1,1)"),
                 `HG=F` = strat_ag_arma_garch(price = px_list$`HG=F`$`HG=F.Adjusted`,
                                              model = "arma(1,0) + garch(1,1)")
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



save.image("R/Results/my_workspace.RData")


#--------Plot möglichkeit hypothesentests----------------------
# df <- ALL_TVALS_DIR
# #df$ticker <- rownames(df)          # Rowname -> Spalte
# df$pair   <- paste(df$strat_A, df$strat_B, sep = " → ")
# df$t      <- round(df$t_val, 3)
# df$p      <- signif(df$p_val, 3)
# df$sig    <- ifelse(df$p_val < .001, "***",
#                     ifelse(df$p_val < .01,  "**",
#                            ifelse(df$p_val < .05,  "*", "")))
# 
# tabs_by_pair <- split(df[, c("t","p","sig")], df$pair)
# 
# # In Bookdown einfach ausgeben:
# for (nm in names(tabs_by_pair)) {
#   cat("\n\n###", nm, "\n\n")
#   knitr::kable(tabs_by_pair[[nm]], align = "lccc",
#                caption = paste("Paired t-Values je Index –", nm))
# }



# # #----Test Plots-------------
#ma
# par(mar = c(5, 4, 4, 2) + 1)
# plot(exp(cumsum(na.omit(BACKT$MA$SSMI))) - 1, col = "darkgreen", lwd = 1)
# lines(exp(cumsum(na.omit(BACKT$BH$SSMI[index(BACKT$MA$SSMI)]))) - 1, col = "darkblue")
# legend("topleft",
#        legend = c("Buy & Hold", "Strategy"),
#        col    = c("darkgreen", "darkblue"),
#        lty    = 1,
#        bty    = "n",
#        lwd = 2)
# #bb
# par(mar = c(5, 4, 4, 2) + 1)
# plot(exp(cumsum(na.omit(BACKT$MA$`BTC-USD`))) - 1, col = "darkgreen", lwd = 1)
# lines(exp(cumsum(na.omit(BACKT$BH$`BTC-USD`[index(BACKT$MA$`BTC-USD`)]))) - 1, col = "darkblue")
# legend("topleft",
#        legend = c("Buy & Hold", "Strategy"),
#        col    = c("darkgreen", "darkblue"),
#        lty    = 1,
#        bty    = "n",
#        lwd = 2)


# 
#--- MA Strategy plot
# par(mar = c(5, 4, 4, 2) + 1)
# SSMI <- px_list$SSMI$SSMI.Adjusted
# SMA_N1 <- STRATS$MA$SSMI$strategy$`SMA short`
# SMA_N2 <- STRATS$MA$SSMI$strategy$`SMA long`
# signal <- STRATS$MA$SSMI$signal
# 
# chartSeries(SSMI, name = "SSMI MA Strategy",
#             type = "line",
#             subset=date_from_to,
#             theme=chartTheme("white"))
# addTA(SMA_N1,col="red", on = 1)
# addTA(SMA_N2,col="blue", on = 1)
# addTA(signal,col="grey")

# 
# 
# #--- BB Strategy plot

# mavg <- STRATS$BB$SSMI$strategy$mavg
# up <- STRATS$BB$SSMI$strategy$up
# dn <- STRATS$BB$SSMI$strategy$dn
# signal <- STRATS$BB$SSMI$signal
# from_to<-"2010-08::2020-09-15"
# 
# chartSeries(STRATS$BB$SSMI$price,
#             type = "line",
#             subset=from_to,
#             theme=chartTheme("white"), name = "SSMI BB Strategy")
# addTA(mavg,type="S",col="blue", on = 1)
# addTA(up,type="S",col="darkgrey", on = 1, lty = 2)
# addTA(dn,type="S",col="darkgrey", on = 1, lty = 2)
# addTA(signal,col="grey", type = "h")
# 
# 
# 
# 
# `BTC-USD` <- px_list$`BTC-USD`$`BTC-USD.Adjusted`
# mavg <- STRATS$BB$`BTC-USD`$strategy$mavg
# up <- STRATS$BB$`BTC-USD`$strategy$up
# dn <- STRATS$BB$`BTC-USD`$strategy$dn
# signal <- STRATS$BB$`BTC-USD`$signal
# from_to<-"2010-08::2020-09-15"
# 
# chartSeries(`BTC-USD`,
#             type = "line",
#             subset=from_to,
#             theme=chartTheme("white"))
# b_bands <-BBands(`BTC-USD`,n=20, sd=2)
# addTA(b_bands$mavg,type="S",col="red", on = 1)
# addTA(b_bands$up,type="S",col="blue", on = 1)
# addTA(b_bands$dn,type="S",col="blue", on = 1)
# addTA(signal,col="grey")
# 
# rm(SSMI, SMA_N1, SMA_N2, signal)

# #--- MACD Strategy plot
# # Pick the same time window
# from_to <- "2019-11-01::2020-11-01"
# 
# price   <- STRATS$MACD$GDAXI$price
# price   <- STRATS$MACD$GDAXI$price
# signal  <- STRATS$MACD$GDAXI$signal
# 
# macd_line   <- STRATS$MACD$GDAXI$strategy$macd
# signal_line <- STRATS$MACD$GDAXI$strategy$signal
# macd_hist   <- macd_line - signal_line
# 
# chartSeries(price,
#             type   = "line",
#             subset = from_to,
#             theme  = chartTheme("white"),
#             name   = "GDAXI MACD Strategy")
# 
# ## 1) MACD indicator in panel 2
# addTA(macd_line,   type = "l", col = "blue", on = NA, legend = NULL)  # creates panel 2
# addTA(signal_line, type = "l", col = "red",  on = 2)   # same panel as MACD
# addTA(macd_hist,      type = "h", col = "darkgrey", on = 2)
# 
# 
# ## 2) Trading signals in panel 3
# addTA(signal, type = "h", col = "grey", on = NA)   # creates panel 3



#--- Sharpe Ratio Plot

# #--- Matrix of the Sharpe ratios for visualisation
# library(tidyr); library(dplyr); library(ggplot2)
# 
# df <- imap_dfr(EVAL_BT, ~tibble(ticker = names(.x), value = unlist(.x), strategy = .y))
# 
# ggplot(df, aes(x = strategy, y = ticker, fill = value)) +
#   geom_tile(aes()) +
#   geom_text(aes(label = round(value, 3)), size = 3) +
#   scale_fill_distiller(palette = "Greens", direction = 1)+
#   labs(x = "Strategy", y = "Index", fill = "Value",
#        title = "Sharpe Ratio by Strategy and Index")
# 
# ggplot(df, aes(x = strategy, y = ticker, fill = value)) +
#   geom_tile() +
#   geom_text(aes(label = round(value, 3)), size = 3) +
#   scale_fill_gradient(low = "#FAFAFA", high = "#5A5A5A") +
#   labs(x = "Strategy", y = "Index", fill = "Value",
#        title = "Sharpe Ratio by Strategy and Index")







