library(tidyverse)
library(furrr)
library(AZASRS)
plan(multisession)

# Minimum possible NAV date = '2008-12-31'
value_date = get_value_date()

# Contains rollups, contains ITD IRRs
pestats = build_pe_stats()

# Can aggregate by sum()
nav = get_pm_nav_daily() %>%
  filter(effective_date == value_date)

cash_flow = get_pm_cash_flow_daily() %>%
  filter(effective_date <= value_date)

# All trailing IRRs
n_qtrs = c(1, 4, 12, 20, 40)
trailing_irrs = tibble(n_qtr = n_qtrs) %>%
  mutate(irrs = pmap(.l = list(start_date = "2018-09-30",
                               end_date = value_date,
                               n_qtrs = n_qtr),
                    .f = build_grouped_irrs,
                    pm_fund_portfolio))

