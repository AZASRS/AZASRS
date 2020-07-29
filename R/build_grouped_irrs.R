#' Create rollup of IRR based off of dates
#'
#' @description Build IRR for any rollup and date range, including ITD and cash adjusted
#' @param .data is from clean_nav_cf()
#' @param ... aggregation choices from from pm_fund_info (i.e. pm_fund_portfolio, pm_fund_category, pm_fund_id)
#' @param start_date is the start date of analysis
#' @param end_date is the cutoff date of analysis
#' @param itd is a boolean that determines whether itd (inception to date) is included
#' @param cash_adjusted is a boolean that determines whether cash is adjusted
#' @param nav_daily is the object of get_pm_nav_daily()
#' @param cf_daily is the object of get_pm_cash_flow_daily()
#' @param pm_fund_info is the object of get_pm_fund_info()
#' @return
#' @examples
#' # Example use case
#' nav <- get_pm_nav_daily() %>% dplyr::filter(nav != 0)
#' cf <- get_pm_cash_flow_daily() %>% dplyr::filter(cash_flow != 0)
#' pm_fund_info <- get_pm_fund_info()
#' start_date <- "2019-09-30"
#' end_date <- "2019-12-31"
#' itd <- FALSE
#' cash_adjusted <- FALSE
#' final_data <- build_grouped_irrs(
#'   start_date = start_date, end_date = end_date, itd = itd,
#'   cash_adjusted = cash_adjusted, pm_fund_info = pm_fund_info,
#'   pm_fund_portfolio, pm_fund_category_description
#' )
#' @export
build_grouped_irrs <- function(start_date, end_date, itd, cash_adjusted, nav_daily, cf_daily, pm_fund_info, ...) {
  clean_data <- build_grouped_pm_cash_flow(
    start_date = start_date,
    end_date = end_date,
    itd = itd,
    cash_adjusted = cash_adjusted,
    nav_daily = nav_daily,
    cf_daily = cf_daily,
    pm_fund_info = pm_fund_info,
    ...
  )
  clean_data %>%
    calculate_grouped_irr(...)
}


#' Calculate the rollup IRR
#'
#' @description Calculates the IRR from a list of "cash flow" from clean_nav_cf()
#' @param .data is from clean_nav_cf()
#' @param ... aggregation choices from from pm_fund_info (i.e. pm_fund_portfolio, pm_fund_category, pm_fund_id)
calculate_grouped_irr <- function(.data, ...) {
  .data %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(irr = calc_irr(adjusted_cash_flow, effective_date))
}
