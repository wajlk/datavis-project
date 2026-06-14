# Investors Helper

An interactive R Shiny flexdashboard for comparing long-term stock and bond allocations under uncertainty. It combines a two-strategy portfolio comparison with a goal-oriented allocation planner.

**Authors:** Adam Baranowski and Mateusz Wilk

**Repository:** https://github.com/wajlk/datavis-project

## Application Tasks

The dashboard helps users:

1. Compare two stock/bond strategies over a selected investment horizon.
2. Inspect expected wealth and the 95% range of possible outcomes over time.
3. See how risk aversion changes the perceived utility of a strategy.
4. Define a real purchasing-power target and compare allocations by their probability of reaching it.
5. Evaluate downside, upside, median wealth, and the chance of finishing below the starting value.
6. Select an allocation from a table and inspect its linked probability and terminal-wealth distribution.

## Data Sources

- [Vanguard 500 Index Fund Investor Shares (`VFINX`)](https://finance.yahoo.com/quote/VFINX/) for historical equity prices.
- [FRED Consumer Price Index (`CPIAUCSL`)](https://fred.stlouisfed.org/series/CPIAUCSL) for inflation adjustment.
- [FRED 10-Year Treasury Constant Maturity Rate (`GS10`)](https://fred.stlouisfed.org/series/GS10) for the bond-return baseline.

The application starts with bundled parameters in [`data/market_parameters.csv`](data/market_parameters.csv), so ordinary interactions do not depend on external network calls. The parameters can be refreshed from the configured sources by running `refresh_market_parameter_cache()`.

## Methodology

Each strategy or allocation is evaluated using 999 percentile scenarios from 0.1% through 99.9%. Historical observations determine the model parameters, while a Student's t-distribution represents heavy-tailed stock outcomes. Bonds use a constant real-growth baseline derived from historical Treasury yields.

These scenarios are deterministic distribution percentiles. They are not a historical backtest and are not random Monte Carlo draws. The dashboard is an educational visualization, not financial advice.

## Interactive Components

The application contains six linked visualization components across two pages:

1. Risk-adjusted utility trajectory.
2. Utility decomposition waterfall.
3. Projected wealth trajectory with uncertainty range.
4. Probability of reaching the selected purchasing-power goal.
5. Selected allocation's terminal-wealth distribution.
6. Interactive allocation decision table.

Controls include time horizon, risk aversion, two stock allocations, strategy comparison, target purchasing power, wealth scale, and histogram range. Selecting a row in the allocation table updates the selected marker and outcome-distribution chart. Planner inputs are debounced and the allocation surface is reused, keeping interactions responsive.

## Assignment Requirements

- IH monogram in [`www/logo.svg`](www/logo.svg).
- Custom visual theme in [`styles.css`](styles.css).
- About/Help page with instructions, methodology, and data sources.
- Six interactive components, including a `DT` datatable.
- Cross-component row selection.
- Portfolio and Goal Planner pages.
- Dynamic Shiny server runtime.

## Run Locally

Install R and the required packages:

```r
install.packages(c(
  "flexdashboard", "shiny", "ggplot2", "plotly", "tidyverse",
  "ggsci", "tidyquant", "broom", "lubridate", "scales",
  "patchwork", "DT", "rmarkdown"
))
```

Clone and run the project:

```bash
git clone https://github.com/wajlk/datavis-project.git
cd datavis-project
Rscript -e 'rmarkdown::run("investors_helper.Rmd")'
```

Alternatively, open `datavisproject.Rproj` in RStudio and click **Run Document**.

## Refresh Historical Parameters

Network access is only required when refreshing the bundled parameters:

```r
source("bonds_vs_stocks.R")
refresh_market_parameter_cache()
```

The refreshed cache is stored locally as `data/market_parameters.rds` and is intentionally excluded from Git.

## Project Structure

```text
investors_helper.Rmd       Dashboard layout, controls, and Shiny server logic
bonds_vs_stocks.R          Data loading, scenario engine, metrics, and plots
data/market_parameters.csv Bundled model parameters
styles.css                 Custom dashboard styling
www/logo.svg               Investors Helper monogram
project.R                  Convenience launcher
```
