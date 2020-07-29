#' Create rollup of IRR based off of dates
#'
#' @description Build IRR for any rollup and date range, including ITD and cash adjusted
#' @param ... aggregation choices from from pm_fund_info (i.e. pm_fund_portfolio, pm_fund_category, pm_fund_id)
#' @param start_date is the start date of analysis
#' @param end_date is the cutoff date of analysis
#' @param itd is a boolean that determines whether itd (incpetion to date) is included, overrides start_date
#' @param cash_adjusted is a boolean that determines if NAV is cash adjusted
#' @param nav_daily is the object of get_pm_nav_daily()
#' @param cf_daily is the object of get_pm_cash_flow_daily()
#' @param bench_daily_index is the object of get_benchmark_daily_index()
#' @param bench_relationships is the object of get_benchmark_fund_relationship()
#' @param pm_fund_info is the object of get_pm_fund_info()
#' @export
build_grouped_pm_metrics <- function(...,
                                     con = AZASRS_DATABASE_CONNECTION(),
                                     start_date = "2016-12-31",
                                     end_date = get_value_date(con = con),
                                     itd = FALSE,
                                     cash_adjusted = FALSE,
                                     nav_daily = get_pm_nav_daily(con = con),
                                     cf_daily = get_pm_cash_flow_daily(con = con),
                                     bench_daily_index = get_benchmark_daily_index(con = con, benchmark_type = "PVT", return_tibble = TRUE),
                                     bench_relationships = get_benchmark_fund_relationship(con = con, bench_type = "PVT", return_tibble = TRUE),
                                     pm_fund_info = get_pm_fund_info(con = con)) {

  bench_daily = bench_daily_index %>%
    dplyr::as_tibble() %>%
    dplyr::filter(start_date >= start_date, end_date <= end_date) %>%
    dplyr::distinct(benchmark_info_id, effective_date, .keep_all = TRUE) %>%
    dplyr::group_by(benchmark_info_id) %>%
    dplyr::arrange(effective_date) %>%
    dplyr::mutate(index_fv = dplyr::last(index_value) / index_value) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(benchmark_info_id, effective_date)

  clean_data <- build_grouped_pm_cash_flow(...,
    start_date = start_date,
    end_date = end_date,
    itd = itd,
    cash_adjusted = cash_adjusted,
    nav_daily = nav_daily,
    cf_daily = cf_daily,
    bench_daily = bench_daily,
    bench_relationships = bench_relationships,
    pm_fund_info = pm_fund_info
  )



  # Append benchmark data before calculating metrics
  dat <- clean_data %>%
    calculate_grouped_pm_metrics(...) %>%
    dplyr::mutate(itd = itd) %>%
    calc_time_delta(start_date, end_date)

  # If not ITD, then certain metrics do not apply
  if (!itd) {
    dat <- dat %>%
      dplyr::mutate(pme = NA, tvpi = NA, dpi = NA, dva = NA)
  }

  return(dat)
}


#' Calculate the rollup metrics
#'
#' @description Calculates the major financial metrics
#' @param .data is from build_grouped_pm_cash_flow()
#' @param ... aggregation choices from from pm_fund_info (i.e. pm_fund_portfolio, pm_fund_category, pm_fund_id)
calculate_grouped_pm_metrics <- function(.data, ...) {
  .data %>%
    dplyr::group_by(...) %>%
    dplyr::arrange(effective_date) %>%
    dplyr::summarize(
      start_date = min(effective_date, na.rm = TRUE),
      end_date = max(effective_date, na.rm = TRUE),
      pme = -sum(distributions_fv) / sum(contributions_fv),
      irr = calc_irr(adjusted_cash_flow, effective_date),
      irr_fv = calc_irr(cash_flow = adj_cf_fv, dates = effective_date),
      tvpi = calc_tvpi(distributions = distributions, contributions = contributions, nav = nav),
      dpi = calc_dpi(distributions = distributions, contributions = contributions),
      alpha = log(1 + irr_fv),
      bench_irr = -1 + exp(log(1 + irr) - alpha),
      dva = sum(dva),
      nav = dplyr::last(nav),
      # nav = sum(nav),
      cash_flow = sum(cash_flow),
      adjusted_cash_flow = sum(adjusted_cash_flow),
      contributions = sum(contributions),
      distributions = sum(distributions),
      excess = irr - bench_irr
    ) %>%
    dplyr::select(
      ..., pme, irr, tvpi, dpi, bench_irr, dva, nav, cash_flow, contributions, distributions, excess, start_date, end_date
    )
}
