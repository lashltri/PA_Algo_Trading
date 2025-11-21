# Results

Overview of the Evaluation Framework

## Asset-Level Analysis

### Sharpe Ratios Across Assets

Table \@ref(tab:matsharpe) shows the annualized Sharpe ratios for all strategies over the evaluation period 2019-2024.

```{r, echo=FALSE}
library(knitr)
kable(mat_sharpe,
      caption = "(\\#tab:matsharpe) Sharpe Ratio Across Assets (2019-2024)",
      format = 'simple', align = "l")

```

#### US Equity indices

Table \@ref(tab:sharpeus) shows the Sharpe ratios of all strategies applied to the US equity indices (GSPC and IXIC). These Assets exhibit a stronger upward drift during the evaluation period than other indices, which is typical for US assets due to inflation, it is also worth noting that the currency of these assets is USD. For these reasons the data was analysed separately .

```{r, echo=FALSE}
library(knitr)

us_assets <- c("GSPC", "IXIC")

kable(
  mat_sharpe[us_assets, ],
  caption = "(#tab:sharpeus) Sharpe Ratios: US Markets (2019-2024)",
  format = "simple",
  align = "l"
)

```

The volatility adjusted ARMA-GARCH trading strategy performed best (1.13-1.15), even compared to the Buy and Hold benchmark (0.77-0.79). The MCAD strategy also performs well (0.64–0.91). Simple Moving Averages don't stand out positively or negatively in this scenario. The Bollinger Bands strategy yields negative Sharpe ratios for both US indices, indicating that volatility breakout signals were not effective in these markets.

A plausible explanation for the strong performance of the AG strategy in US markets is the combination of upward trends and volatility clustering. 
The GARCH component models periods of elevated and low volatility and allows the strategy to adjust its position size accordingly. Volatility clustering during negative downturns allowed AG to scale exposure down in turbulent periods and increase exposure during calmer, more predictable phases. This effect can be seen in the figure \@ref(fig:usvolagspc) bellow, where the ARMA-GARCH stretegy shows almost linear constant growth.

```{r usvolagspc, echo=FALSE, fig.cap="GSPC ARMA–GARCH strategy: estimated GARCH volatility (red) and strategy position size (grey).", out.width="60%", fig.keep="last", message=FALSE, warning=FALSE, results='hide'}

par(mfrow = c(1,2))

# --- GSPC ---------------------------------------------------------
dat_gspc <- strat_ag_arma_garch(
  price = px_list$GSPC$GSPC.Adjusted,
  model = "arma(1,0) + garch(1,1)",
  return_strat = TRUE
)

par(mar = c(4, 4, 2, 2), mfrow = c(3, 1))

cum_ag <- exp(cumsum(na.omit(BACKT$AG$GSPC))) - 1
cum_bh <- exp(cumsum(na.omit(BACKT$BH$GSPC[index(BACKT$AG$GSPC)]))) - 1

plot(cum_bh, lwd=2,
     main="NG=F: Cumulative Returns (BH)",
     ylab="Cumulative return", xlab="Time")
plot(cum_ag, lwd=2,
     main="NG=F: Cumulative Returns (AG)",
     ylab="Cumulative return", xlab="Time")

plot(dat_gspc$vola, main = "NG=F GARCH volatility estimates")

```

However, the pairwise hypothesis tests based on standardized returns (see Section @ref(assethypothesis)) do not indicate a statistically significant difference between AG and Buy and Hold on US indices once returns are adjusted for time-varying volatility. This reflects the fact that standardization removes precisely the volatility-scaling advantage that AG is designed to exploit.

#### European Equity indices
Table \@ref(tab:sharpeeu) shows the Sharpe ratios for the European equity indices SSMI and GDAXI. Compared to the US indices, both markets exhibit noticeably weaker performance during 2019–2024 in their naitive currencies.

```{r, echo=FALSE}
us_assets <- c("SSMI", "GDAXI")

kable(
  mat_sharpe[us_assets, ],
  caption = "(#tab:sharpeeu) Sharpe Ratios: European Markets",
  format = "simple",
  align = "l")
```

The SSMI shows the lowest Sharpe ratios among all equity indices. A central reason is Switzerland’s persistently low inflation and strong CHF, which leads to weaker trend behaviour. As a result, mainly trend-based strategies such as MA and BB perform poorly. 

The ARMA-GARCH strategy outperforms Buy and Hold for both European assets, but not as noticeably as the MCAD strategy. A possible reason is that MCAD captures minor directional moves while filtering out short-term noise, whereas BB often triggers premature position changesand MA reacts too slowly to weak price movements.

Overall, the performance patterns consistent across both European indices, with the same strategies outperforming and underperforming.

#### Energy commodities
Table \@ref(tab:sharpeenergy) shows the Sharpe ratios for crude oil (CL=F) and natural gas (NG=F). Both markets perform very poorly across almost all trading strategies. The strategies that perform relatively better, as well as those that perform worse, differ between the two assets and do not provide a consistent pattern for the energy category. This suggests that crude oil and natural gas behave fundamentally differently and cannot be treated as a homogeneous asset group.

```{r , echo=FALSE}
energy_assets <- c("CL=F", "NG=F")

kable(
  mat_sharpe[energy_assets, ],
  caption = "(#tab:sharpeenergy) Sharpe Ratios: Energy Commodities",
  format = "simple",
  align = "l")
```

For crude oil, the only positive Sharpe ratio appears under Buy-and-Hold, while all active strategies perform worse. Crude Oil did not exhibit persistent trends or exploitable momentum during the sample period, making slow-moving or threshold-based rules ineffective. The lack of trends can is visualized in the following figure \@ref(fig:clfprice).

```{r clfprice, echo=FALSE, fig.cap="Price and Log-Returns of the SSMI (2019-2024)", out.width="70%", fig.keep="last", message=FALSE, warning=FALSE, results='hide'}

par(mfrow = c(2,1))
plot(STRATS$BH$`CL=F`$price, main = "CL=F Price")
plot(diff(log(STRATS$BH$`CL=F`$price)), main = "CL=F Price")
```
Natural gas, shows a negative Sharpe ratio under Buy and Hold and weak performance across almost all strategies. ARMA-GARCH shows the highest Sharpe ratio, this improvement is driven mainly by reducing exposure during high volatility during the downturn at the end of the sample period. This effect is non significant (see Section @ref(assethypothesis)) probably due to the increased volatility.

Figure @ref(fig:agngf) shows the cumulative returns of the ARMA–GARCH strategy compared to Buy-and-Hold for NG=F. The ARMA–GARCH curve rises more steadily because the strategy reduces its exposure during periods of extreme volatility, avoiding large drawdowns and creating a smoother cumulative return path.

```{r agngf, echo=FALSE, fig.cap="NG=F: ARMA-GARCH vs Buy-and-Hold", out.width="70%", fig.keep="last", message=FALSE, warning=FALSE}

dat_ng <- strat_ag_arma_garch(
  price        = px_list$`NG=F`$`NG=F.Adjusted`,
  model        = "arma(1,0) + garch(1,1)",
  return_strat = TRUE
)

par(mar = c(4, 4, 2, 2), mfrow = c(3, 1))

cum_ag <- exp(cumsum(na.omit(BACKT$AG$`NG=F`))) - 1
cum_bh <- exp(cumsum(na.omit(BACKT$BH$`NG=F`[index(BACKT$AG$`NG=F`)]))) - 1

plot(cum_bh, lwd=2,
     main="NG=F: Cumulative Returns (BH)",
     ylab="Cumulative return", xlab="Time")
plot(cum_ag, lwd=2,
     main="NG=F: Cumulative Returns (AG)",
     ylab="Cumulative return", xlab="Time")

plot(dat_ng$vola, main = "NG=F GARCH volatility estimates")
```




#### Metals

```{r, echo=FALSE}
metal_assets <- c("GC=F", "SI=F", "HG=F")

kable(
  mat_sharpe[metal_assets, ],
  caption = "(#tab:sharpemetals) Sharpe Ratios: Metals",
  format = "simple",
  align = "l")
```

```{r mettrend, echo=FALSE, fig.cap="Price and Log-Returns of the SSMI (2019-2024)", out.width="70%", fig.keep="last", message=FALSE, warning=FALSE, results='hide'}


# --- SSMI ---------------------------------------------------------

par(mfrow = c(2,1))
plot(STRATS$BH$SSMI$price, main = "SSMI Price")
plot(diff(log(STRATS$BH$SSMI$price)), main = "SSMI log-Returns")

```
#### Cryptocurrency

```{r, echo=FALSE}
crypto_assets <- c("BTC-USD")

kable(
  mat_sharpe[crypto_assets, ],
  caption = "(#tab:sharpecrypto) Sharpe Ratios: Cryptocurrency (BTC-USD)",
  format = "simple",
  align = "l")
```

### Max Drawdowns Across Assets

```{r, echo=FALSE}
library(knitr)
kable(mat_drawdown,
      caption = "(\\#tab:matdrawdown) Maximum Drawdowns Across Assets",
      format = 'simple', align = "l")

```

### Statistical Comparison of Strategy Returns (Paired Hypothesis Tests) {#assethypothesis}

However, the pairwise hypothesis tests based on standardized returns (see Section @ref(sec:assethypothesis)) do not indicate a statistically significant difference between AG and Buy and Hold on US indices once returns are adjusted for time-varying volatility. This reflects the fact that standardization removes precisely the volatility-scaling advantage that AG is designed to exploit. Nevertheless, AG remains significantly different from several other active strategies in US markets, confirming that it captures genuine structure in these series.

```{r, echo = FALSE}

plot_p_matrix_sig_soft(ALL_TVALS_DIR, "BH") 

```
```{r, echo = FALSE}

plot_p_matrix_sig_soft(ALL_TVALS_DIR, "MA") 

```
```{r, echo = FALSE}
plot_p_matrix_sig_soft(ALL_TVALS_DIR, "BB")
```

```{r, echo = FALSE}
plot_p_matrix_sig_soft(ALL_TVALS_DIR, "AG")
```

```{r, echo = FALSE}
plot_p_matrix_sig_soft(ALL_TVALS_DIR, "MACD")
```

## Portfolio-Level Analysis

### Portfolio Sharpe Ratios

### Portfolio Drawdown Behaviour

### Statistical Comparison of Strategy Returns (Paired Hypothesis Tests)

```{r, echo = FALSE}
plot_portfolio_matrix_sig_soft(ALL_TVALS_DIR, alpha = 0.10)
```

## Case Study Financial Crisis

Crisis Sub-Sample (2008 Financial Crisis)

## Summary of Findings
