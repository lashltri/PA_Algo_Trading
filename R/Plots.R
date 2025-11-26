# # P-wert Plots--------------------------------
# library(dplyr)
# library(ggplot2)
# 
# plot_p_matrix_sig <- function(all_tvals, strategy, alpha = 0.10) {
#   require(dplyr); require(ggplot2)
# 
#   # if a list, bind to one data frame
#   if (is.list(all_tvals) && !inherits(all_tvals, "data.frame")) {
#     all_tvals <- dplyr::bind_rows(all_tvals)
#   }
# 
#   d <- all_tvals %>%
#     dplyr::filter(strat_A == strategy) %>%
#     dplyr::rename(opponent = strat_B) %>%
#     dplyr::select(ticker, opponent, p_val, t_val) %>%
#     dplyr::group_by(ticker, opponent) %>%
#     dplyr::summarise(
#       p_val = mean(p_val, na.rm = TRUE),
#       t_val = mean(t_val, na.rm = TRUE),
#       .groups = "drop"
#     ) %>%
#     dplyr::mutate(
#       sig = p_val < alpha,
#       color_group = dplyr::case_when(
#         !sig ~ "nonsig",
#         t_val > 0 ~ "positive",
#         t_val < 0 ~ "negative",
#         TRUE ~ "nonsig"
#       )
#     )
# 
#   # order opponents: BH first, then alphabetical
#   opponents <- unique(d$opponent)
#   opponents <- c("BH", setdiff(sort(opponents), "BH"))
#   d$opponent <- factor(d$opponent, levels = opponents)
# 
#   # order tickers: PORTF first, then alphabetical
#   tickers <- unique(d$ticker)
#   tickers <- c("Portfolio", setdiff(sort(tickers), "Portfolio"))
#   d$ticker <- factor(d$ticker, levels = rev(tickers))  # rev() puts PORTF on top in plot
# 
#   ggplot2::ggplot(d, ggplot2::aes(x = opponent, y = ticker, fill = color_group)) +
#     ggplot2::geom_tile(color = "grey90") +
#     ggplot2::geom_text(
#       ggplot2::aes(label = sprintf("%.3f", p_val),
#                    fontface = ifelse(sig, "bold", "plain")),
#       size = 3, color = "black"
#     ) +
#     ggplot2::scale_fill_manual(
#       values = c(
#         "nonsig"   = "#F6F6F6",
#         "positive" = "#F4A582",
#         "negative" = "#92C5DE"
#       ),
#       name = "Significance",
#       labels = c(
#         "nonsig"   = "not significant",
#         "positive" = "significant (t > 0)",
#         "negative" = "significant (t < 0)"
#       )
#     ) +
#     ggplot2::labs(
#       title = paste0(
#         "Paired t-test (standardized returns)\n",
#         "Significance matrix (p < ", alpha, ") – Strategy ", strategy
#       ),
#       x = "Opponent strategy (B)",
#       y = "Index (Ticker)"
#     ) +
#     ggplot2::theme_minimal(base_size = 11) +
#     ggplot2::theme(
#       panel.grid = element_blank(),
#       axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
#       legend.position = "bottom"
#     )
# }
# 
# 
# 
# plot_p_matrix_sig(ALL_TVALS_DIR_CRISIS, "MA")
# plot_p_matrix_sig(ALL_TVALS_DIR_CRISIS, "BB")
# plot_p_matrix_sig(ALL_TVALS_DIR_CRISIS, "BH")
# plot_p_matrix_sig(ALL_TVALS_DIR_CRISIS, "AG")
# plot_p_matrix_sig(ALL_TVALS_DIR_CRISIS, "MACD")

#------------------------Pvalue plot subsets in one image----------------------
# P-wert Plots--------------------------------
library(dplyr)
library(ggplot2)
library(cowplot)

## 1) Common fill scale (fixed legend definition) -----------------------------

fill_scale_signif <- ggplot2::scale_fill_manual(
  values = c(
    "nonsig"   = "#F6F6F6",
    "positive" = "#F4A582",
    "negative" = "#92C5DE"
  ),
  breaks = c("nonsig", "positive", "negative"),
  labels = c(
    "nonsig"   = "not significant",
    "positive" = "significant (t > 0)",
    "negative" = "significant (t < 0)"
  ),
  drop = FALSE,
  name = "Significance"
)

## 2) Function: creates a plot WITHOUT legend --------------------------------

plot_p_matrix_sig_soft <- function(all_tvals, strategy, alpha = 0.10) {
  require(dplyr); require(ggplot2)
  
  # if a list, bind to one data frame
  if (is.list(all_tvals) && !inherits(all_tvals, "data.frame")) {
    all_tvals <- dplyr::bind_rows(all_tvals)
  }
  
  d <- all_tvals %>%
    dplyr::filter(strat_A == strategy) %>%
    dplyr::rename(opponent = strat_B) %>%
    dplyr::select(ticker, opponent, p_val, t_val) %>%
    dplyr::group_by(ticker, opponent) %>%
    dplyr::summarise(
      p_val = mean(p_val, na.rm = TRUE),
      t_val = mean(t_val, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      sig = p_val < alpha,
      color_group = dplyr::case_when(
        !sig ~ "nonsig",
        t_val > 0 ~ "positive",
        t_val < 0 ~ "negative",
        TRUE ~ "nonsig"
      ),
      color_group = factor(color_group,
                           levels = c("nonsig", "positive", "negative"))
    )
  
  # order opponents: BH first, then alphabetical
  opponents <- unique(d$opponent)
  opponents <- c("BH", setdiff(sort(opponents), "BH"))
  d$opponent <- factor(d$opponent, levels = opponents)
  
  # order tickers: Portfolio first, then alphabetical
  tickers <- unique(d$ticker)
  tickers <- c("Portfolio", setdiff(sort(tickers), "Portfolio"))
  d$ticker <- factor(d$ticker, levels = rev(tickers))
  
  p <- ggplot2::ggplot(d, ggplot2::aes(x = opponent, y = ticker, fill = color_group)) +
    ggplot2::geom_tile(color = "grey90") +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.3f", p_val),
                   fontface = ifelse(sig, "bold", "plain")),
      size = 2.5, color = "black"
    ) +
    fill_scale_signif +
    ggplot2::labs(
      title = paste0(
        "Paired t-test (standardized returns)\n",
        "Significance matrix (p < ", alpha, ") – Strategy ", strategy
      ),
      x = "Opponent strategy (B)",
      y = "Index (Ticker)"
    ) +
    ggplot2::theme_minimal(base_size = 7.5) +
    ggplot2::theme(
      panel.grid       = ggplot2::element_blank(),
      axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position  = "none"
    )
  
  return(p)
}

## 3) Build the 5 plots (no legend in any of them) ---------------------------

# us_assets <- c("GSPC", "IXIC")
# US_TVALS <- ALL_TVALS_DIR[ALL_TVALS_DIR$ticker %in% us_assets, ]
# 
# p_bh   <- plot_p_matrix_sig_soft(US_TVALS, "BH")
# p_ma   <- plot_p_matrix_sig_soft(US_TVALS, "MA")
# p_bb   <- plot_p_matrix_sig_soft(US_TVALS, "BB")
# p_ag   <- plot_p_matrix_sig_soft(US_TVALS, "AG")
# p_macd <- plot_p_matrix_sig_soft(US_TVALS, "MACD")
# 
# # Empty plot so MACD has a partner in the last row
# blank_plot <- ggplot2::ggplot() + ggplot2::theme_void()
# 
# # 2 plots per row
# combined_plots <- cowplot::plot_grid(
#   p_bh,   p_ma,
#   p_bb,   p_ag,
#   p_macd, blank_plot,
#   ncol = 2,
#   align = "hv"
# )

## 4) Create a COMPLETELY FIXED legend --------------------------------------

legend_df <- data.frame(
  opponent    = factor(rep("BH", 3), levels = "BH"),
  ticker      = factor(c("dummy1", "dummy2", "dummy3")),
  color_group = factor(c("nonsig", "positive", "negative"),
                       levels = c("nonsig", "positive", "negative")),
  p_val = c(0.2, 0.01, 0.01),
  sig   = c(FALSE, TRUE, TRUE)
)

legend_plot <- ggplot2::ggplot(
  legend_df,
  ggplot2::aes(x = opponent, y = ticker, fill = color_group)
) +
  ggplot2::geom_tile() +
  fill_scale_signif +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "bottom",
    legend.title    = ggplot2::element_text(size = 8),
    legend.text     = ggplot2::element_text(size = 7.5)
  )

fixed_legend <- cowplot::get_legend(legend_plot)

## 5) Combine everything ------------------------------------------------------

# final_plot <- cowplot::plot_grid(
#   combined_plots,
#   fixed_legend,
#   ncol = 1,
#   rel_heights = c(1, 0.12)
# )
# 
# print(final_plot)



#------------------------ Portfolio Hypothesis test---------------------------
library(dplyr)
library(ggplot2)

plot_portfolio_matrix_sig_soft <- function(all_tvals, alpha = 0.10) {
  require(dplyr); require(ggplot2)
  
  # if a list, bind to one data frame
  if (is.list(all_tvals) && !inherits(all_tvals, "data.frame")) {
    all_tvals <- dplyr::bind_rows(all_tvals)
  }
  
  d <- all_tvals %>%
    dplyr::filter(ticker == "Portfolio") %>%          # only portfolio
    dplyr::select(strat_A, strat_B, p_val, t_val) %>%
    dplyr::group_by(strat_A, strat_B) %>%
    dplyr::summarise(
      p_val = mean(p_val, na.rm = TRUE),
      t_val = mean(t_val, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      sig = p_val < alpha,
      color_group = dplyr::case_when(
        !sig       ~ "nonsig",
        t_val > 0  ~ "positive",
        t_val < 0  ~ "negative",
        TRUE       ~ "nonsig"
      )
    )
  
  # strategies: BH first, then alphabetical, same order on x and y
  strats <- unique(c(d$strat_A, d$strat_B))
  strats <- c("BH", setdiff(sort(strats), "BH"))
  
  d$strat_B <- factor(d$strat_B, levels = strats)        # columns
  d$strat_A <- factor(d$strat_A, levels = rev(strats))   # rows (reversed so BH on top)
  
  ggplot2::ggplot(d, ggplot2::aes(x = strat_B, y = strat_A, fill = color_group)) +
    ggplot2::geom_tile(color = "grey90") +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.3f", p_val),
                   fontface = ifelse(sig, "bold", "plain")),
      size = 3, color = "black"
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "nonsig"   = "#F6F6F6",
        "positive" = "#F4A582",
        "negative" = "#92C5DE"
      ),
      name = "Significance",
      labels = c(
        "nonsig"   = "not significant",
        "positive" = "significant (t > 0)",
        "negative" = "significant (t < 0)"
      )
    ) +
    ggplot2::labs(
      title = paste0(
        "Paired t-test (standardized returns)\n",
        "Portfolio-level significance matrix (p < ", alpha, ")"
      ),
      x = "Strategy B",
      y = "Strategy A"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid = element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
}


#plot_portfolio_matrix_sig_soft(ALL_TVALS_DIR, alpha = 0.10)

#---------------------- Portfolio Hypothesis test crisis -----------------------
library(dplyr)
library(ggplot2)

plot_portfolio_matrix_sig_crisis <- function(all_tvals, alpha = 0.10) {
  require(dplyr); require(ggplot2)
  
  # if a list, bind to one data frame
  if (is.list(all_tvals) && !inherits(all_tvals, "data.frame")) {
    all_tvals <- dplyr::bind_rows(all_tvals)
  }
  
  d <- all_tvals %>%
    dplyr::filter(ticker == "Portfolio") %>%          # only portfolio
    dplyr::select(strat_A, strat_B, p_val, t_val) %>%
    dplyr::group_by(strat_A, strat_B) %>%
    dplyr::summarise(
      p_val = mean(p_val, na.rm = TRUE),
      t_val = mean(t_val, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      sig = p_val < alpha,
      color_group = dplyr::case_when(
        !sig       ~ "nonsig",
        t_val > 0  ~ "positive",
        t_val < 0  ~ "negative",
        TRUE       ~ "nonsig"
      )
    )
  
  # strategies: BH first, then alphabetical, same order on x and y
  strats <- unique(c(d$strat_A, d$strat_B))
  strats <- c("BH", setdiff(sort(strats), "BH"))
  
  d$strat_B <- factor(d$strat_B, levels = strats)        # columns
  d$strat_A <- factor(d$strat_A, levels = rev(strats))   # rows (reversed so BH on top)
  
  ggplot2::ggplot(d, ggplot2::aes(x = strat_B, y = strat_A, fill = color_group)) +
    ggplot2::geom_tile(color = "grey90") +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.3f", p_val),
                   fontface = ifelse(sig, "bold", "plain")),
      size = 3, color = "black"
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "nonsig"   = "#F6F6F6",
        "positive" = "#F4A582",
        "negative" = "#92C5DE"
      ),
      name = "Significance",
      labels = c(
        "nonsig"   = "not significant",
        "positive" = "significant (t > 0)",
        "negative" = "significant (t < 0)"
      )
    ) +
    ggplot2::labs(
      title = paste0(
        "Paired t-test (standardized returns 2005-2009)\n",
        "Portfolio-level significance matrix (p < ", alpha, ")"
      ),
      x = "Strategy B",
      y = "Strategy A"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid = element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
}
