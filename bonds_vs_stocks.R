library(tidyquant)
library(tidyverse)
library(broom)
library(lubridate)
library(scales)
library(patchwork)
library(plotly)

# =========================================================
# USER SETTINGS & CONSTANTS
# =========================================================
var_mult <- 2.0
deg_free_lower <- 2.5
deg_free_upper <- 50
deg_free_shape <- 3
df_horizon <- 360

start_date <- "1976-08-01"
asset_tickers <- c("VFINX")
macro_ticker_cpi <- "CPIAUCSL"
ticker_bonds <- "GS10"


# =========================================================
# PART 1: DATA FETCHING & PARAMETER CALCULATION
# =========================================================
calculate_market_parameters <- function(start_dt, tickers, cpi_ticker, bond_ticker, v_mult) {
  df_cpi <- tq_get(cpi_ticker, get = "economic.data", from = start_dt) %>%
    mutate(year_month = floor_date(date, "month")) %>%
    select(year_month, cpi_value = price) %>%
    arrange(year_month) %>%
    tidyr::fill(cpi_value, .direction = "down")

  df_assets_monthly <- tq_get(tickers, get = "stock.prices", from = start_dt) %>%
    mutate(year_month = floor_date(date, "month")) %>%
    group_by(symbol, year_month) %>%
    slice_max(order_by = date, n = 1) %>%
    ungroup() %>%
    select(symbol, year_month, price = adjusted)

  df_assets_real <- df_assets_monthly %>%
    left_join(df_cpi, by = "year_month") %>%
    tidyr::drop_na(cpi_value) %>%
    mutate(price_adjusted = (price / cpi_value) * 100) %>%
    select(symbol, year_month, price_adjusted)

  df_weighted_growth <- df_assets_real %>%
    group_by(symbol) %>%
    mutate(
      days_since_start = as.numeric(difftime(year_month, min(year_month), units = "days")),
      t = days_since_start / 365.25,
      log_price = log(price_adjusted)
    ) %>%
    nest() %>%
    mutate(
      model = map(data, ~ lm(log_price ~ t, data = .x)),
      tidy_model = map(model, tidy)
    ) %>%
    unnest(tidy_model) %>%
    filter(term == "t") %>%
    rename(continuous_growth_rate = estimate)

  df_variance <- df_assets_real %>%
    arrange(symbol, year_month) %>%
    group_by(symbol) %>%
    mutate(log_return = log(price_adjusted / lag(price_adjusted))) %>%
    drop_na(log_return) %>%
    summarise(sigma_monthly = sd(log_return), .groups = "drop")

  mu_s_annual <- df_weighted_growth %>%
    filter(symbol == "VFINX") %>%
    pull(continuous_growth_rate)
  mu_s <- mu_s_annual / 12
  sigma_s <- df_variance %>%
    filter(symbol == "VFINX") %>%
    pull(sigma_monthly) * sqrt(v_mult)

  df_gs10 <- tq_get(bond_ticker, get = "economic.data", from = start_dt) %>%
    mutate(year_month = floor_date(date, "month")) %>%
    select(year_month, nominal_yield_pct = price) %>%
    arrange(year_month) %>%
    tidyr::fill(nominal_yield_pct, .direction = "down")

  df_cpi_10yr_forward <- df_cpi %>%
    mutate(year_month = year_month - years(10)) %>%
    select(year_month, cpi_10yr_forward = cpi_value)

  df_bonds_real <- df_gs10 %>%
    left_join(df_cpi, by = "year_month") %>%
    left_join(df_cpi_10yr_forward, by = "year_month") %>%
    drop_na() %>%
    mutate(
      nominal_yield = nominal_yield_pct / 100,
      annualized_forward_inflation = (cpi_10yr_forward / cpi_value)^(1 / 10) - 1,
      real_yield = ((1 + nominal_yield) / (1 + annualized_forward_inflation)) - 1,
      continuous_real_yield = log(1 + real_yield)
    )

  mu_b_annual <- mean(df_bonds_real$continuous_real_yield)
  mu_b <- mu_b_annual / 12

  params <- list(mu_s = mu_s, sigma_s = sigma_s, mu_b = mu_b)
  if (!all(lengths(params) == 1L) || !all(is.finite(unlist(params)))) {
    stop("The market data sources returned incomplete parameters.")
  }

  params
}

load_market_parameters <- function() {
  cache_path <- file.path("data", "market_parameters.rds")
  bundled_path <- file.path("data", "market_parameters.csv")
  fallback <- list(mu_s = 0.0052, sigma_s = 0.055, mu_b = 0.0015)

  if (file.exists(cache_path)) {
    return(readRDS(cache_path))
  }

  if (file.exists(bundled_path)) {
    bundled <- read.csv(bundled_path, stringsAsFactors = FALSE)
    return(list(
      mu_s = bundled$value[bundled$parameter == "mu_s"],
      sigma_s = bundled$value[bundled$parameter == "sigma_s"],
      mu_b = bundled$value[bundled$parameter == "mu_b"]
    ))
  }

  fallback
}

market_params <- load_market_parameters()

refresh_market_parameter_cache <- function() {
  params <- calculate_market_parameters(
    start_date,
    asset_tickers,
    macro_ticker_cpi,
    ticker_bonds,
    var_mult
  )
  dir.create("data", recursive = TRUE, showWarnings = FALSE)
  saveRDS(params, file.path("data", "market_parameters.rds"))
  params
}


# =========================================================
# PART 2: PURE ENGINE FUNCTIONS
# =========================================================
generate_wealth_matrix <- function(w_stocks, w_bonds, mu_s, sigma_s, mu_b,
                                   df_shape, df_lower, df_upper, df_horizon,
                                   W0 = 100, months = 360) {
  sum_both <- w_stocks + w_bonds
  w_stocks <- w_stocks / sum_both
  w_bonds <- w_bonds / sum_both

  t_seq <- 1:months
  tau <- t_seq / df_horizon
  exp_ratio <- (exp(df_shape * tau) - 1) / (exp(df_shape) - 1)
  deg_free_dynamic <- df_lower + (df_upper - df_lower) * exp_ratio

  p_seq <- seq(0.001, 0.999, by = 0.001)
  t_quantiles <- t(sapply(deg_free_dynamic, function(df) qt(p_seq, df = df)))

  sigma_t_s <- sigma_s * sqrt(t_seq)
  mu_s_t <- mu_s * t_seq
  w_bonds_dist <- (W0 * w_bonds) * exp(mu_b * t_seq)
  w_stocks_initial <- W0 * w_stocks

  scaled_quantiles <- sweep(t_quantiles, 1, sigma_t_s, "*")
  exponent_matrix <- sweep(scaled_quantiles, 1, mu_s_t, "+")

  stocks_matrix <- w_stocks_initial * exp(exponent_matrix)
  wealth_matrix <- sweep(stocks_matrix, 1, w_bonds_dist, "+")

  societal_floor <- W0 * 0.15
  ceiling_vector <- wealth_matrix[, 999]

  wealth_matrix <- sweep(wealth_matrix, 1, ceiling_vector, "pmin")
  wealth_matrix <- pmax(wealth_matrix, societal_floor)

  wealth_matrix
}

calculate_trajectory_metrics <- function(dist_matrix) {
  months <- nrow(dist_matrix)

  tibble(
    month = 1:months,
    wealth_p025 = dist_matrix[, 25],
    wealth_p975 = dist_matrix[, 975],
    expected_wealth = apply(dist_matrix, 1, mean)
  )
}

calculate_utility_metrics <- function(dist_matrix, gamma, W0 = 100) {
  months <- nrow(dist_matrix)

  calc_u <- function(w, g) {
    if (g == 1) log(w) else (w^(1 - g)) / (1 - g)
  }
  u_baseline <- calc_u(W0, gamma)
  k_multiplier <- if (gamma == 1) W0 else W0^gamma

  scale_u <- function(u_raw) {
    W0 + k_multiplier * (u_raw - u_baseline)
  }

  expected_scaled_utility <- apply(dist_matrix, 1, function(row) {
    u_dist <- calc_u(row, gamma)
    mean(scale_u(u_dist))
  })

  final_dist <- dist_matrix[months, ]
  expected_final_wealth <- mean(final_dist)

  u_pure_expected_raw <- calc_u(expected_final_wealth, gamma)
  u_pure_expected_scaled <- scale_u(u_pure_expected_raw)
  u_actual_variance_scaled <- expected_scaled_utility[months]

  variance_penalty_scaled <- u_actual_variance_scaled - u_pure_expected_scaled

  list(
    trajectory = tibble(
      month = 1:months,
      expected_scaled_utility = expected_scaled_utility
    ),
    final_stats = list(
      u_pure_expected = u_pure_expected_scaled - W0,
      u_actual_variance = u_actual_variance_scaled - W0,
      variance_penalty = variance_penalty_scaled
    )
  )
}

calculate_allocation_summary <- function(stocks_pct, months, gamma) {
  wealth_matrix <- generate_wealth_matrix(
    w_stocks = stocks_pct / 100,
    w_bonds = (100 - stocks_pct) / 100,
    mu_s = market_params$mu_s,
    sigma_s = market_params$sigma_s,
    mu_b = market_params$mu_b,
    df_shape = deg_free_shape,
    df_lower = deg_free_lower,
    df_upper = deg_free_upper,
    df_horizon = df_horizon,
    W0 = 100,
    months = months
  )

  final_wealth <- wealth_matrix[nrow(wealth_matrix), ]
  utility <- calculate_utility_metrics(wealth_matrix, gamma = gamma)

  tibble(
    Stocks = stocks_pct,
    Bonds = 100 - stocks_pct,
    `Expected wealth` = mean(final_wealth),
    `Median wealth` = median(final_wealth),
    `Downside (2.5%)` = unname(quantile(final_wealth, 0.025)),
    `Upside (97.5%)` = unname(quantile(final_wealth, 0.975)),
    `Risk-adjusted utility` = utility$final_stats$u_actual_variance
  )
}

build_allocation_summary <- function(stock_allocations, months, gamma) {
  purrr::map_dfr(
    stock_allocations,
    calculate_allocation_summary,
    months = months,
    gamma = gamma
  )
}

build_terminal_surface <- function(stock_allocations, months, W0 = 100) {
  terminal_tau <- months / df_horizon
  terminal_ratio <- (exp(deg_free_shape * terminal_tau) - 1) /
    (exp(deg_free_shape) - 1)
  terminal_df <- deg_free_lower +
    (deg_free_upper - deg_free_lower) * terminal_ratio

  terminal_quantiles <- qt(seq(0.001, 0.999, by = 0.001), df = terminal_df)
  stock_growth <- exp(
    market_params$mu_s * months +
      market_params$sigma_s * sqrt(months) * terminal_quantiles
  )
  bond_growth <- exp(market_params$mu_b * months)

  stock_weights <- stock_allocations / 100
  wealth <- outer(W0 * stock_weights, stock_growth) +
    outer(W0 * (1 - stock_weights), rep(bond_growth, length(stock_growth)))
  wealth <- pmax(wealth, W0 * 0.15)

  list(
    allocations = stock_allocations,
    wealth = wealth,
    months = months,
    initial_wealth = W0
  )
}

summarize_terminal_surface <- function(surface, gamma, target_wealth) {
  wealth <- surface$wealth
  W0 <- surface$initial_wealth

  calc_u <- function(w, g) {
    if (g == 1) log(w) else (w^(1 - g)) / (1 - g)
  }
  baseline_utility <- calc_u(W0, gamma)
  utility_multiplier <- if (gamma == 1) W0 else W0^gamma
  scaled_utility <- W0 + utility_multiplier *
    (calc_u(wealth, gamma) - baseline_utility)

  tibble(
    Stocks = surface$allocations,
    Bonds = 100 - surface$allocations,
    `Goal probability` = rowMeans(wealth >= target_wealth),
    `Chance below start` = rowMeans(wealth < W0),
    `Expected wealth` = rowMeans(wealth),
    `Median wealth` = apply(wealth, 1, median),
    `Downside (2.5%)` = apply(wealth, 1, quantile, probs = 0.025),
    `Upside (97.5%)` = apply(wealth, 1, quantile, probs = 0.975),
    `Risk-adjusted utility` = rowMeans(scaled_utility) - W0
  )
}

surface_distribution <- function(surface, stocks_pct) {
  selected_row <- which.min(abs(surface$allocations - stocks_pct))

  tibble(
    Percentile = seq(0.1, 99.9, by = 0.1),
    Wealth = surface$wealth[selected_row, ]
  )
}

build_terminal_distribution <- function(stocks_pct, months) {
  wealth_matrix <- generate_wealth_matrix(
    w_stocks = stocks_pct / 100,
    w_bonds = (100 - stocks_pct) / 100,
    mu_s = market_params$mu_s,
    sigma_s = market_params$sigma_s,
    mu_b = market_params$mu_b,
    df_shape = deg_free_shape,
    df_lower = deg_free_lower,
    df_upper = deg_free_upper,
    df_horizon = df_horizon,
    W0 = 100,
    months = months
  )

  tibble(
    Percentile = seq(0.1, 99.9, by = 0.1),
    Wealth = wealth_matrix[nrow(wealth_matrix), ]
  )
}


# =========================================================
# GLOBAL PLOTTING SETTINGS
# =========================================================
color_wealth_strat1 <- "#00BCD4"
color_wealth_strat2 <- "#9C27B0"

color_utility_strat1 <- "#006064"
color_utility_strat2 <- "#4A148C"

palette_wealth <- c(
  "Strategy 1" = color_wealth_strat1,
  "Strategy 2" = color_wealth_strat2
)
palette_utility <- c(
  "Strategy 1" = color_utility_strat1,
  "Strategy 2" = color_utility_strat2
)


# =========================================================
# PLOTTING FUNCTIONS
# =========================================================
plot_wealth_trajectory <- function(df, scale_type = "log10", colors = palette_wealth) {
  y_lower <- min(df$expected_wealth) * 0.5
  y_upper <- max(df$expected_wealth) * 2.0

  p <- df %>%
    mutate(year = month / 12) %>%
    ggplot(aes(x = year, group = Strategy, color = Strategy, fill = Strategy)) +
    geom_ribbon(
      aes(ymin = wealth_p025, ymax = wealth_p975),
      alpha = 0.15,
      color = NA
    ) +
    geom_line(aes(y = expected_wealth), linewidth = 1.0) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    coord_cartesian(ylim = c(y_lower, y_upper)) +
    labs(
      title = "Projected Wealth Trajectory (95% CI)",
      subtitle = paste("Scale:", str_to_title(scale_type)),
      x = "Years",
      y = "Wealth ($)"
    ) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold"))

  if (scale_type == "log10") {
    p <- p + scale_y_continuous(labels = label_dollar(), trans = "log10")
  } else {
    p <- p + scale_y_continuous(labels = label_dollar())
  }

  ply <- ggplotly(p, tooltip = c("x", "y", "ymin", "ymax"))

  for (i in seq_along(ply$x$data)) {
    name_val <- ply$x$data[[i]]$name
    if (is.character(name_val) && grepl("Strategy", name_val)) {
      clean_name <- stringr::str_extract(name_val, "Strategy [12]")
      ply$x$data[[i]]$name <- clean_name
      ply$x$data[[i]]$legendgroup <- clean_name

      if (!is.null(ply$x$data[[i]]$fill) && ply$x$data[[i]]$fill != "none") {
        ply$x$data[[i]]$showlegend <- FALSE
      }
    }
  }

  ply %>%
    layout(
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15),
      margin = list(t = 50, b = 40)
    ) %>%
    config(displayModeBar = FALSE)
}

plot_utility_trajectory <- function(df, ylim_bounds = NULL, colors = palette_utility) {
  p <- df %>%
    mutate(year = month / 12) %>%
    ggplot(aes(x = year, color = Strategy)) +
    geom_line(aes(y = expected_scaled_utility), linewidth = 0.8) +
    scale_color_manual(values = colors) +
    scale_y_continuous(labels = label_dollar()) +
    labs(
      title = "Expected Utility Trajectory",
      subtitle = "Psychological Value of Wealth",
      x = "Years",
      y = "Psych. Value"
    ) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold"))

  if (!is.null(ylim_bounds)) {
    p <- p + coord_cartesian(ylim = ylim_bounds)
  }

  ggplotly(p) %>%
    layout(
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.3,
        yanchor = "top"
      ),
      margin = list(l = 100, r = 50, t = 50, b = 85)
    ) %>%
    config(displayModeBar = FALSE)
}

plot_utility_waterfall <- function(df_stats, colors = palette_utility) {
  df_stats <- df_stats %>%
    mutate(
      Strategy = factor(Strategy, levels = sort(unique(Strategy), decreasing = TRUE)),
      y_pos = as.numeric(Strategy),
      label_html = sprintf(
        "<b><span style='color:%s'>%s</span></b>",
        colors[as.character(Strategy)],
        Strategy
      ),
      wealth_color = ifelse(u_pure_expected >= 0, "#2ecc71", "#e74c3c"),
      penalty_color = ifelse(variance_penalty >= 0, "#2ecc71", "#e74c3c")
    )

  min_x <- min(0, df_stats$u_pure_expected, df_stats$u_actual_variance)
  max_x <- max(0, df_stats$u_pure_expected, df_stats$u_actual_variance)
  data_span <- max(max_x - min_x, 1)
  wealth_offset <- data_span * 0.16
  variance_offset <- data_span * 0.18
  final_offset <- data_span * 0.14

  p <- ggplot(df_stats) +
    geom_rect(
      aes(
        xmin = 0,
        xmax = u_pure_expected,
        ymin = y_pos + 0.05,
        ymax = y_pos + 0.35,
        fill = wealth_color,
        text = sprintf("Expected Wealth: %+.1f", u_pure_expected)
      ),
      alpha = 0.9
    ) +
    geom_rect(
      aes(
        xmin = u_actual_variance,
        xmax = u_pure_expected,
        ymin = y_pos - 0.35,
        ymax = y_pos - 0.05,
        fill = penalty_color,
        text = sprintf("Variance Penalty: %+.1f", variance_penalty)
      ),
      alpha = 0.85
    ) +
    geom_segment(
      aes(
        x = u_actual_variance,
        xend = u_actual_variance,
        y = y_pos - 0.45,
        yend = y_pos + 0.45,
        color = Strategy
      ),
      linetype = "dashed",
      linewidth = 0.75
    ) +
    geom_text(
      aes(
        x = u_pure_expected + wealth_offset,
        y = y_pos + 0.20,
        label = sprintf("Wealth: %+.1f", u_pure_expected)
      ),
      color = "black",
      size = 2.8,
      hjust = 0
    ) +
    geom_text(
      aes(
        x = ifelse(
          variance_penalty >= 0,
          u_actual_variance + variance_offset,
          u_pure_expected + variance_offset
        ),
        y = y_pos - 0.20,
        label = sprintf("Var Penalty: %+.1f", variance_penalty)
      ),
      color = "black",
      size = 2.8,
      hjust = 0
    ) +
    geom_text(
      aes(
        x = u_actual_variance - final_offset,
        y = y_pos - 0.20,
        label = sprintf("Final: %+.1f", u_actual_variance)
      ),
      color = "black",
      size = 2.8,
      hjust = 1
    ) +
    scale_color_manual(values = colors) +
    scale_fill_identity() +
    scale_x_continuous(labels = label_dollar(), expand = expansion(mult = c(0.05, 0.35))) +
    scale_y_continuous(breaks = df_stats$y_pos, labels = df_stats$label_html) +
    labs(
      title = "Terminal Utility Breakdown",
      subtitle = "Green = Positive Value | Red = Negative Value/Penalty",
      x = "Psych. Value",
      y = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 11),
      axis.text.y = element_text(size = 10),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    )

  ggplotly(p, tooltip = "text") %>%
    layout(
      margin = list(l = 100, r = 50, t = 50, b = 50),
      showlegend = FALSE
    ) %>%
    config(displayModeBar = FALSE)
}
