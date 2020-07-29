#' Build a tibble of major private market metrics including: IRR, DPI, TVPI, PME -- Assumes benchmark type is PVT
#'
#' @param ... grouping variables (pm_fund_id, pm_fund_portfolio, etc.)
#' @param con is a database connection object from AZASRS::AZASRS_DATABASE_CONNECTION()
#' @param nav_daily is the object of get_pm_nav_daily()
#' @param cf_daily is the object of get_pm_cash_flow_daily()
#' @param benchmark_daily is the object of build_benchmark_fv_index_factor()
#' @param pmfi is the object of get_pm_fund_info()
#' @param start_date is the start date of analysis
#' @param pcap_date is the cutoff date of analysis
#' @param value_date is the object of get_value_date()
#' @return Returns a tibble with grouping variables and all of their respective metrics
#' @examples
#' build_privm_metrics(pm_fund_portfolio, pm_fund_id)
#' # pm_fund_portfolio pm_fund_id    irr     dpi   tvpi appreciation     pme irr_pcap
#' # <chr>             <chr>       <dbl>   <dbl>  <dbl>        <dbl>   <dbl>    <dbl>
#' # 1 Credit            Fund1     0.0919   0.372   1.15   437251492.   1.05        0.11
#' # 2 Credit            Fund2     0.159    0       1.11   562211000    1.10        NA
#' # 3 Credit            Fund3     0.0247   0       1.10   262390142    0.952       NA
#' # 4 Credit            Fund4     0.0673   0.135   1.17   173259780    1.06        NA
#' @export
build_privm_metrics <- function(...,
                                con = AZASRS_DATABASE_CONNECTION(),
                                nav_daily = get_pm_nav_daily(con = con, return_tibble = FALSE),
                                cf_daily = get_pm_cash_flow_daily(con = con, return_tibble = FALSE),
                                benchmark_daily = build_benchmark_fv_index_factor(con = con, return_tibble = FALSE),
                                pmfi = get_pm_fund_info(con = con, return_tibble = FALSE),
                                start_date = "1900-01-01",
                                value_date = get_value_date(con = con),
                                pcap_date = as.character(lubridate::today())) {
  if (as.Date(pcap_date) < as.Date(value_date)) {
    stop("pcap_date must be greater than or equal to value_date")
  } # ensures potential for pcap after value_date
  if (as.Date(value_date) <= as.Date(start_date)) {
    stop("value_date must be greater than start_date")
  } # ensures potential for at least one nav or cash_flow before value_date

  # Convert group_vars to character vector for joining tables
  group_vars <- dplyr::enquos(...)
  group_vars_char <- c()
  for (i in as.character(dplyr::quos(!!!group_vars))) {
    group_vars_char <- c(group_vars_char, substring(as.character(i), 2))
  }

  # Assumes benchmark type is PVT
  bm_fi <- get_benchmark_fund_relationship(con) %>%
    dplyr::filter(benchmark_type == "PVT") %>%
    dplyr::left_join(pmfi, by = "pm_fund_info_id")

  #### filtering all dates
  nav_daily_filtered <- nav_daily %>%
    dplyr::filter(effective_date >= start_date & effective_date <= pcap_date)

  nav_min_dates <- nav_daily_filtered %>%
    dplyr::group_by(pm_fund_id) %>%
    dplyr::summarize(min_date = min(effective_date, na.rm = TRUE)) %>%
    dplyr::ungroup()

  cf_daily_filtered <- cf_daily %>%
    dplyr::left_join(nav_min_dates, by = "pm_fund_id") %>%
    dplyr::filter(effective_date >= start_date) %>%
    dplyr::filter(effective_date <= pcap_date) %>%
    dplyr::filter(effective_date >= min_date) %>%
    dplyr::select(-min_date)

  bench_daily <- bm_fi %>%
    dplyr::left_join(benchmark_daily, by = "benchmark_info_id") %>%
    dplyr::filter(effective_date >= start_date & effective_date <= pcap_date) %>%
    dplyr::select(pm_fund_id, benchmark_id, effective_date, index_value, index_factor)

  ### benchmark issue!!! this needs each day to count to final right?? this would filter to only same days as join allows
  cf_bench_daily <- cf_daily_filtered %>%
    dplyr::left_join(bench_daily, by = c("pm_fund_id", "effective_date")) %>%
    dplyr::select(pm_fund_id, effective_date, cash_flow, contributions, distributions, index_value, index_factor)

  # get nav values for: first, value_date (or date cutoff specified), and last in date range
  nav_pcap_dates <- nav_daily_filtered %>%
    dplyr::group_by(pm_fund_id) %>%
    dplyr::mutate(beg_date = min(effective_date, na.rm = TRUE), end_date = max(effective_date, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      beg_nav = dplyr::if_else(effective_date == beg_date, nav, 0),
      end_nav = dplyr::if_else(effective_date == end_date, nav, 0),
      val_nav = dplyr::if_else(effective_date == value_date, nav, 0)
    ) %>%
    dplyr::mutate(beg_nav = -1 * abs(beg_nav)) %>%
    dplyr::mutate(
      nav_val = beg_nav + val_nav,
      nav_end = beg_nav + end_nav
    ) %>%
    dplyr::mutate(pcap = dplyr::if_else(nav_val != nav_end & nav_end != 0, 1, 0)) %>%
    dplyr::filter(effective_date == beg_date | effective_date == end_date | effective_date == value_date) %>% # TODO: check these assumptions
    dplyr::select(pm_fund_id, effective_date, pcap, beg_nav, end_nav, val_nav, nav_val, nav_end, beg_date, end_date) %>%
    dplyr::group_by(pm_fund_id) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup() # filters out funds with only one recorded date

  # nav_value_date represents nav at first date and value_date
  nav_val <- nav_pcap_dates %>%
    dplyr::filter(pcap == 0) %>%
    dplyr::rename(nav_cutoff = nav_val) %>%
    dplyr::select(pm_fund_id, effective_date, nav_cutoff, beg_date, end_date) %>%
    dplyr::mutate(pcap = 0)

  # nav_max represents nav at first date and value_date or max date if it exists (pcap)
  nav_end <- nav_pcap_dates %>%
    dplyr::filter(pcap == 1 | effective_date == beg_date) %>%
    dplyr::group_by(pm_fund_id) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup() %>%
    dplyr::rename(nav_cutoff = nav_end) %>%
    dplyr::select(pm_fund_id, effective_date, nav_cutoff, beg_date, end_date) %>%
    dplyr::mutate(pcap = 1)

  nav_vals_by_date <- nav_val %>%
    dplyr::union_all(nav_end) %>%
    dplyr::mutate(cash_flow = 0, contributions = 0, distributions = 0) # to simplify union_all (no NA)

  pcap_funds <- nav_pcap_dates %>%
    dplyr::filter(pcap == 1) %>%
    dplyr::select(pm_fund_id, beg_date, end_date) %>%
    dplyr::distinct()

  no_pcap_funds <- nav_pcap_dates %>%
    dplyr::filter(pcap == 0) %>%
    dplyr::select(pm_fund_id, beg_date, end_date) %>%
    dplyr::distinct()

  # Combine nav & cf_bench --> after join, data with a pcap will be duplicated (must always filter or group by pcap)
  # TODO: drop cash flow if it occurs on same first date of nav
  nav_cf_daily_val <- no_pcap_funds %>%
    dplyr::left_join(cf_bench_daily, by = "pm_fund_id") %>%
    dplyr::mutate(nav_cutoff = 0, pcap = 0) %>%
    dplyr::union_all(nav_val) %>%
    dplyr::filter(effective_date >= beg_date) %>%
    dplyr::filter(effective_date <= value_date) %>%
    dplyr::group_by(pm_fund_id, effective_date, pcap) %>%
    dplyr::summarize(
      cash_flow = sum(cash_flow, na.rm = TRUE),
      nav_cutoff = sum(nav_cutoff, na.rm = TRUE),
      contributions = sum(contributions, na.rm = TRUE),
      distributions = sum(distributions, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      cash_flow = dplyr::if_else(is.na(cash_flow), 0, cash_flow),
      contributions = dplyr::if_else(is.na(contributions), 0, contributions),
      distributions = dplyr::if_else(is.na(distributions), 0, distributions),
      nav_cutoff = dplyr::if_else(is.na(nav_cutoff), 0, nav_cutoff)
    )
  #   tibble::as_tibble() %>%
  #   tidyr::drop_na(effective_date)
  # nav_cf_daily_val[is.na(nav_cf_daily_val)] = 0

  nav_cf_daily_end <- pcap_funds %>%
    dplyr::left_join(cf_bench_daily, by = "pm_fund_id") %>%
    dplyr::mutate(nav_cutoff = 0, pcap = 1) %>%
    dplyr::union_all(nav_end) %>%
    dplyr::filter(effective_date >= beg_date) %>%
    dplyr::filter(effective_date <= end_date) %>%
    dplyr::group_by(pm_fund_id, effective_date, pcap) %>%
    dplyr::summarize(
      cash_flow = sum(cash_flow, na.rm = TRUE),
      nav_cutoff = sum(nav_cutoff, na.rm = TRUE),
      contributions = sum(contributions, na.rm = TRUE),
      distributions = sum(distributions, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      cash_flow = dplyr::if_else(is.na(cash_flow), 0, cash_flow),
      contributions = dplyr::if_else(is.na(contributions), 0, contributions),
      distributions = dplyr::if_else(is.na(distributions), 0, distributions),
      nav_cutoff = dplyr::if_else(is.na(nav_cutoff), 0, nav_cutoff)
    ) # %>%
  #   tibble::as_tibble() %>%
  #   tidyr::drop_na(effective_date)
  # nav_cf_daily_end[is.na(nav_cf_daily_end)] = 0

  nav_cf_daily <- dplyr::union_all(nav_cf_daily_val, nav_cf_daily_end) %>%
    dplyr::mutate(cash_flow_cutoff = cash_flow + nav_cutoff) %>%
    dplyr::left_join(bench_daily, by = c("pm_fund_id", "effective_date")) %>%
    dplyr::mutate(cash_flow = dplyr::if_else(is.na(cash_flow), 0, cash_flow)) # %>% tibble::as_tibble(),
  # by = c('pm_fund_id', 'effective_date')) #%>%
  #   tidyr::drop_na(effective_date)
  # nav_cf_daily[is.na(nav_cf_daily)] = 0


  # PME setup calcs
  # Get last

  final_data <- nav_cf_daily %>%
    dplyr::left_join(pmfi, by = "pm_fund_id")

  # Allow for calc of 'TOTAL PM'
  final_data <- final_data %>% dplyr::mutate(TOTAL = "TOTAL PM")

  # Calculate IRR, DPI, TVPI, Appreciation, DVA
  fund_metrics_prep <- final_data %>%
    dplyr::group_by(pcap, !!!group_vars, effective_date) %>%
    dplyr::summarize(
      cash_flow_cutoff = sum(cash_flow_cutoff, na.rm = TRUE),
      contributions = sum(contributions, na.rm = TRUE),
      distributions = sum(distributions, na.rm = TRUE),
      nav_cutoff = sum(nav_cutoff, na.rm = TRUE),
      cash_flow = sum(cash_flow, na.rm = TRUE),
      last_index_value = sum(last_index_value, na.rm = TRUE),
      index_value = sum(index_value, na.rm = TRUE)
    ) %>%
    tibble::as_tibble()

  fund_metrics <- fund_metrics_prep %>%
    dplyr::group_by(pcap, !!!group_vars) %>%
    dplyr::summarize(
      irr = calc_irr(cash_flow_cutoff, effective_date),
      dpi = calc_dpi(distributions, contributions),
      tvpi = calc_tvpi(distributions, contributions, nav_cutoff),
      appreciation = calc_appreciation(cash_flow = cash_flow, nav = nav_cutoff),
      pme = calc_pme(distributions, contributions, nav_cutoff, last_index_value / index_value),
      dva = calc_dva(cash_flow_cutoff, last_index_value / index_value)
    ) %>%
    dplyr::ungroup()

  fund_metrics_false <- fund_metrics %>%
    dplyr::filter(pcap == 0) %>%
    dplyr::select(-pcap)
  fund_metrics_true <- fund_metrics %>%
    dplyr::filter(pcap == 1) %>%
    dplyr::select(!!!group_vars, irr, -pcap) %>%
    dplyr::rename(irr_pcap = irr)
  fund_metrics_final <- fund_metrics_false %>% dplyr::left_join(fund_metrics_true, by = group_vars_char)
  fund_metrics_final <- fund_metrics_final %>% tibble::as_tibble()

  return(fund_metrics_final)
}
