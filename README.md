
# PA_Algo_Trading

This repository contains the full R codebase used for the thesis
"Portfolio Analysis and Algorithmic Trading" by Trinity Lashley.

The project evaluates several rule-based trading strategies across
different asset classes and compares their performance to a Buy and
Hold benchmark under both normal and crisis market conditions.

----------------------------------------------------------------------
PROJECT OVERVIEW
----------------------------------------------------------------------

The objective of this project is to assess the robustness of common
technical trading strategies across market regimes using a transparent
and reproducible backtesting framework.

Strategies implemented:

- Buy and Hold (BH)
- Moving Average crossover (MA)
- Bollinger Bands (BB)
- MACD
- ARMA-GARCH volatility-scaled strategy (AG)

Performance is evaluated for:

- Main evaluation period (mixed market conditions)
- Financial crisis period (2007–2009)

An equally weighted portfolio is included to complement the asset-level
analysis.

----------------------------------------------------------------------
ASSET COVERAGE
----------------------------------------------------------------------

Equity indices:
- SSMI
- S&P 500 (GSPC)
- NASDAQ (IXIC)
- DAX (GDAXI)

Commodities:
- Crude Oil
- Natural Gas
- Gold
- Silver
- Copper

Cryptocurrency:
- Bitcoin (excluded from the crisis evaluation due to data availability)

----------------------------------------------------------------------
METHODOLOGY (SUMMARY)
----------------------------------------------------------------------

1) Data
   - Daily adjusted prices are downloaded using quantmod.
   - Log-returns are computed.
   - Trading days are aligned across assets.

2) Strategy construction
   - Rule-based signals for MA, BB, MACD.
   - ARMA-GARCH volatility-scaled signal for AG.

3) Parameter tuning
   - Rolling window approach for technical filters.
   - Parameters chosen using risk-adjusted performance (Sharpe-based).

4) Backtesting
   - Out-of-sample evaluation.
   - Long / flat positions (no short selling).

5) Evaluation
   - Risk-adjusted metrics (e.g., Sharpe).
   - Drawdowns and additional summary statistics.
   - Paired hypothesis tests on standardized returns.

Assumptions:
- Long-only strategies (long / flat).
- No transaction costs, slippage, or liquidity constraints.
- Daily close-to-close returns.

----------------------------------------------------------------------
KEY FILES (WHAT THEY DO)
----------------------------------------------------------------------

Main entry scripts:

- R/main_mixed_market.R
  Runs the full analysis for the main evaluation period under mixed
  market conditions. (2019-2024)

- R/main_financial_crisis.R
  Runs the same framework for the financial crisis subsample (2007–2009).

Supporting scripts:

- R/strategies.R
  Strategy definitions (BH, MA, BB, MACD, AG signal construction).

- R/backtesting.R
  Applies signals to returns and computes strategy performance.

- R/Tuning.R
  Rolling window parameter tuning logic.

- R/Hypothesis_Testing.R
  Statistical tests based on standardized returns.

- R/Plots.R
  Plotting functions used to reproduce the thesis figures.

ARMA_GARCH folder:

- R/ARMA_GARCH/ARMA_GARCH_Fitting.R
  Supporting code used for manual ARMA and GARCH model order selection
  and diagnostics. Included for transparency and justification.
  Not required to run the main analysis.

- R/ARMA_GARCH/ARMA_GARCH_Forecasts.R
  Functions implementing the ARMA-GARCH volatility-scaled strategy used
  in the backtests.

----------------------------------------------------------------------
REQUIREMENTS
----------------------------------------------------------------------

R packages used include:
- quantmod
- PerformanceAnalytics
- fGarch
- xts
- TTR
- ggplot2
- dplyr
- lubridate

(Install with install.packages(...) as needed.)

----------------------------------------------------------------------
HOW TO RUN
----------------------------------------------------------------------

1) Clone the repository.
2) Open the project in RStudio.
3) Set the working directory to the project root.

Run one of the following:

Main evaluation period:
source("R/main_mixed_market.R")

Financial crisis period:
source("R/main_financial_crisis.R")

----------------------------------------------------------------------
NOTES AND LIMITATIONS
----------------------------------------------------------------------

- Long-only strategies (no short selling).
- No transaction costs or slippage.

----------------------------------------------------------------------
AUTHOR
----------------------------------------------------------------------

Trinity Lashley
ZHAW – Institute of Data Analysis and Process Design (IDP)
Supervisor: Prof. Dr. Marc Wildi

