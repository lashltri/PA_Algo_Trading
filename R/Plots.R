

#_____________________________________________________________________________
#--------------------------------BB PLOT------------------------------------
#_____________________________________________________________________________

library(ggplot2)
library(dplyr)
library(patchwork)

plot_bb_2panel <- function(price, signal, mavg, up, dn,
                           title = "BB strategy",
                           from_to = NULL,
                           base_size = 12,
                           heights = c(3, 1),
                           month_breaks = "2 months") {
  
  if (!is.null(from_to)) {
    price  <- price[from_to]
    signal <- signal[from_to]
    mavg   <- mavg[from_to]
    up     <- up[from_to]
    dn     <- dn[from_to]
  }
  
  df <- data.frame(
    date  = as.Date(index(price)),
    price = as.numeric(price),
    mavg  = as.numeric(mavg),
    up    = as.numeric(up),
    dn    = as.numeric(dn),
    POS   = as.numeric(signal)
  ) |>
    na.omit() |>
    arrange(date)
  
  # green ribbon down to minimum LOWER band
  price_min <- min(df$dn, na.rm = TRUE)
  
  rect_df <- df |>
    mutate(
      xmin = date,
      xmax = lead(date, default = dplyr::last(date)),
      ymin = 0,
      ymax = POS
    )
  
  xscale <- scale_x_date(
    limits = range(df$date),
    expand = c(0, 0),
    date_breaks = month_breaks,
    date_labels = "%b %Y"
  )
  
  # legend entries
  cols <- c(
    "Price"           = "#32CD32",
    "SMA"             = "blue",
    "Bollinger Bands" = "grey40",
    "Position"        = "grey40"
  )
  
  # ---------------- PANEL 1: PRICE + BBANDS ----------------
  p_top <- ggplot(df, aes(x = date)) +
    # price ribbon
    geom_ribbon(
      aes(ymin = price_min, ymax = price),
      fill = "#32CD32", alpha = 0.30,
      show.legend = FALSE
    ) +
    # BB channel fill
    geom_ribbon(
      aes(ymin = dn, ymax = up),
      fill = "grey70", alpha = 0.25,
      show.legend = FALSE
    ) +
    geom_line(aes(y = price, colour = "Price"), linewidth = 0.6) +
    geom_line(aes(y = mavg,  colour = "SMA"), linewidth = 0.7) +
    geom_line(aes(y = up,    colour = "Bollinger Bands"),
              linewidth = 0.7, linetype = 2) +
    geom_line(aes(y = dn,    colour = "Bollinger Bands"),
              linewidth = 0.7, linetype = 2) +
    xscale +
    labs(title = title, x = NULL, y = "Price") +
    theme_minimal(base_size = base_size) +
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_line(colour = "grey85"),
      panel.grid.minor = element_blank(),
      axis.text.x      = element_blank(),
      axis.title.x     = element_blank(),
      plot.title       = element_text(hjust = 0.5, size = base_size)
    )
  
  # ---------------- PANEL 2: POSITION ----------------
  p_bot <- ggplot() +
    geom_rect(
      data = rect_df,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "grey70", alpha = 0.6,
      show.legend = FALSE
    ) +
    geom_step(
      data = df,
      aes(x = date, y = POS, colour = "Position"),
      linewidth = 0.6,
      direction = "hv"
    ) +
    geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
    xscale +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
    labs(x = NULL, y = "Position") +
    theme_minimal(base_size = base_size) +
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_line(colour = "grey85"),
      panel.grid.minor = element_blank(),
      axis.text.x      = element_text(angle = 45, hjust = 1)
    )
  
  # ---------------- COMBINE ----------------
  (p_top / p_bot) +
    plot_layout(heights = heights, guides = "collect") &
    scale_colour_manual(name = NULL, values = cols) &
    guides(colour = guide_legend(override.aes = list(linewidth = 1))) &
    theme(
      legend.position   = "bottom",
      legend.box.margin = margin(t = -4, r = 6, b = 0, l = 6),
      legend.margin     = margin(t = 0, b = 0),
      plot.margin       = margin(t = 5, r = 5, b = 0, l = 5)
    )
}



#_____________________________________________________________________________
#--------------------------------MACD PLOT------------------------------------
#_____________________________________________________________________________

library(ggplot2)
library(dplyr)
library(patchwork)

plot_macd_3panel <- function(price, signal, macd_line, signal_line,
                             title = "MACD strategy",
                             from_to = NULL,
                             base_size = 12,
                             heights = c(3, 2, 1),
                             month_breaks = "2 months") {
  
  if (!is.null(from_to)) {
    price       <- price[from_to]
    signal      <- signal[from_to]
    macd_line   <- macd_line[from_to]
    signal_line <- signal_line[from_to]
  }
  
  df <- data.frame(
    date        = as.Date(index(price)),
    price       = as.numeric(price),
    macd        = as.numeric(macd_line),
    signal_line = as.numeric(signal_line),
    hist        = as.numeric(macd_line - signal_line),
    POS         = as.numeric(signal)
  ) |>
    na.omit() |>
    arrange(date)
  
  price_min <- min(df$price, na.rm = TRUE)
  
  rect_df <- df |>
    mutate(
      xmin = date,
      xmax = lead(date, default = dplyr::last(date)),
      ymin = 0,
      ymax = POS
    )
  
  xscale <- scale_x_date(
    limits = range(df$date),
    expand = c(0, 0),
    date_breaks = month_breaks,
    date_labels = "%b %Y"
  )
  
  cols <- c(
    "Price"    = "#32CD32",
    "MACD"     = "blue",
    "Signal"   = "red",
    "Position" = "grey40"
  )
  
  # MACD histogram slightly darker than position blocks
  fills <- c("MACD histogram" = "grey60")
  
  # ---------------- PANEL 1: PRICE ----------------
  p_top <- ggplot(df, aes(x = date)) +
    geom_ribbon(
      aes(ymin = price_min, ymax = price),
      fill = "#32CD32", alpha = 0.30,
      show.legend = FALSE
    ) +
    geom_line(aes(y = price, colour = "Price"), linewidth = 0.7) +
    xscale +
    labs(title = title, x = NULL, y = "Price") +
    theme_minimal(base_size = base_size) +
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_line(colour = "grey85"),
      panel.grid.minor = element_blank(),
      axis.text.x      = element_blank(),
      axis.title.x     = element_blank(),
      plot.title       = element_text(hjust = 0.5, size = base_size)
    )
  
  # ---------------- PANEL 2: MACD ----------------
  p_mid <- ggplot(df, aes(x = date)) +
    geom_col(aes(y = hist, fill = "MACD histogram"), width = 1) +
    geom_line(aes(y = macd, colour = "MACD"), linewidth = 0.7) +
    geom_line(aes(y = signal_line, colour = "Signal"), linewidth = 0.7) +
    xscale +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = base_size) +
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_line(colour = "grey85"),
      panel.grid.minor = element_blank(),
      axis.text.x      = element_blank(),
      axis.title.x     = element_blank()
    )
  
  # ---------------- PANEL 3: POSITION ----------------
  p_bot <- ggplot() +
    geom_rect(
      data = rect_df,
      aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = ymax),
      fill = "grey70", alpha = 0.6,
      show.legend = FALSE
    ) +
    geom_step(
      data = df,
      aes(x = date, y = POS, colour = "Position"),
      linewidth = 0.4, direction = "hv"
    ) +
    geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
    xscale +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
    labs(x = NULL, y = "Position") +
    theme_minimal(base_size = base_size) +
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_line(colour = "grey85"),
      panel.grid.minor = element_blank(),
      axis.text.x      = element_text(angle = 45, hjust = 1)
    )
  
  # ---------------- COMBINE ----------------
  (p_top / p_mid / p_bot) +
    plot_layout(heights = heights, guides = "collect") &
    scale_colour_manual(name = NULL, values = cols) &
    scale_fill_manual(name = NULL, values = fills) &
    guides(
      fill   = guide_legend(override.aes = list(alpha = 0.8)),
      colour = guide_legend(override.aes = list(linewidth = 1))
    ) &
    theme(
      legend.position = "bottom",
      legend.box.margin = margin(t = -4, r = 0, b = -4, l = 0),
      legend.margin     = margin(t = 0, b = 0)
    )
}


#_____________________________________________________________________________
#---------------------------------MA PLOT-------------------------------------
#_____________________________________________________________________________

plot_ma_2panel <- function(price, signal, sma_short, sma_long,
                           title = "MA strategy",
                           from_to = NULL,
                           base_size = 12,
                           heights = c(3, 1),
                           month_breaks = "2 months") {
  
  if (!is.null(from_to)) {
    price     <- price[from_to]
    signal    <- signal[from_to]
    sma_short <- sma_short[from_to]
    sma_long  <- sma_long[from_to]
  }
  
  df <- data.frame(
    date      = as.Date(index(price)),
    price     = as.numeric(price),
    sma_short = as.numeric(sma_short),
    sma_long  = as.numeric(sma_long),
    POS       = as.numeric(signal)
  ) |>
    na.omit() |>
    arrange(date)
  
  price_min <- min(df$price, na.rm = TRUE)
  
  rect_df <- df |>
    mutate(
      xmin = date,
      xmax = lead(date, default = dplyr::last(date)),
      ymin = 0,
      ymax = POS
    )
  
  xscale <- scale_x_date(
    limits = range(df$date),
    expand = c(0, 0),
    date_breaks = month_breaks,
    date_labels = "%b %Y"
  )
  
  cols <- c(
    "Price"     = "#32CD32",
    "SMA short" = "red",
    "SMA long"  = "blue",
    "Position"  = "grey40"
  )
  
  # ---------------- PANEL 1: PRICE + SMAs ----------------
  p_top <- ggplot(df, aes(x = date)) +
    geom_ribbon(
      aes(ymin = price_min, ymax = price),
      fill = "#32CD32", alpha = 0.30,
      show.legend = FALSE
    ) +
    geom_line(aes(y = price, colour = "Price"), linewidth = 0.6) +
    geom_line(aes(y = sma_short, colour = "SMA short"), linewidth = 0.7) +
    geom_line(aes(y = sma_long,  colour = "SMA long"),  linewidth = 0.7) +
    xscale +
    labs(title = title, x = NULL, y = "Price") +
    theme_minimal(base_size = base_size) +
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_line(colour = "grey85"),
      panel.grid.minor = element_blank(),
      axis.text.x      = element_blank(),
      axis.title.x     = element_blank(),
      plot.title       = element_text(hjust = 0.5, size = base_size)
    )
  
  # ---------------- PANEL 2: POSITION ----------------
  p_bot <- ggplot() +
    geom_rect(
      data = rect_df,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "grey70", alpha = 0.6,
      show.legend = FALSE
    ) +
    geom_step(
      data = df,
      aes(x = date, y = POS, colour = "Position"),
      linewidth = 0.4, direction = "hv"
    ) +
    geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
    xscale +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
    labs(x = NULL, y = "Position") +
    theme_minimal(base_size = base_size) +
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_line(colour = "grey85"),
      panel.grid.minor = element_blank(),
      axis.text.x      = element_text(angle = 45, hjust = 1)
    )
  
  # ---------------- COMBINE ----------------
  (p_top / p_bot) +
    plot_layout(heights = heights, guides = "collect") &
    scale_colour_manual(name = NULL, values = cols) &
    guides(colour = guide_legend(override.aes = list(linewidth = 1))) &
    theme(
      legend.position = "bottom",
      legend.box.margin = margin(t = -4, r = 0, b = -4, l = 0),
      legend.margin     = margin(t = 0, b = 0)
    )
}










#_____________________________________________________________________________
#-------------------------Statistical tests------------------------------------
#_____________________________________________________________________________


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
    "positive" = "#A9D5AF",
    "negative" = "#F4C08A"
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
        "positive" = "#A9D5AF",
        "negative" = "#F4C08A"
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
        "positive" = "#A9D5AF",
        "negative" = "#F4C08A"
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
