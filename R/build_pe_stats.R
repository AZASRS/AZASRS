#' @export
build_pe_stats = function(con = AZASRS_DATABASE_CONNECTION(),
                          cash_flow_daily = get_pm_cash_flow_daily(con = con, return_tibble = FALSE),
                          nav_daily = get_pm_nav_daily(con = con, return_tibble = FALSE),
                          pmfi = get_pm_fund_info(con = con, return_tibble = FALSE),
                          start_date = '2018-09-30',
                          end_date = get_value_date(con),
                          itd = TRUE){

  pmfi_tbl = tibble::as_tibble(pmfi)
  relationships = get_benchmark_fund_relationship(con = con, return_tibble = FALSE) %>% dplyr::filter(benchmark_type == 'PVT')
  benchs = get_benchmark_daily_index(con = con)

  ##############
  # NAV / CF by fund
  data_prep = build_nav_cash_flow_combined(pm_fund_id, con = con,
                                           itd = itd,
                                           start_date = start_date,
                                           end_date = end_date) %>%
    dplyr::left_join(pmfi %>% dplyr::select(pm_fund_info_id, pm_fund_id)) %>%
    dplyr::left_join(relationships) %>%
    dplyr::left_join(benchs, by = c('benchmark_info_id', 'effective_date')) %>%
    dplyr::select(pm_fund_id, effective_date, nav_cf, contributions, distributions, index_value) %>%
    tibble::as_tibble() %>%
    dplyr::group_by(pm_fund_id) %>%
    dplyr::mutate(index_factor = dplyr::last(index_value, order_by = effective_date) / index_value) %>%
    dplyr::ungroup()
  ##############

  metrics_rollup_portfolio = data_prep %>%
    dplyr::left_join(pmfi_tbl %>% dplyr::select(pm_fund_id, pm_fund_category_description, pm_fund_portfolio)) %>%
    dplyr::group_by(pm_fund_portfolio, effective_date) %>%
    dplyr::summarize(
      nav_cf_fv = sum(nav_cf * index_factor),
      nav_cf = sum(nav_cf),
      contributions_fv = sum(contributions * index_factor),
      distributions_fv = sum(distributions * index_factor),
      dva = sum((contributions + distributions) * index_factor),
      contributions = sum(contributions),
      distributions = sum(distributions)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(pm_fund_portfolio) %>%
    dplyr::summarize(
      pme = -sum(distributions_fv) / sum(contributions_fv),
      irr = calc_irr(cash_flow = nav_cf, dates = effective_date),
      irr_fv = calc_irr(cash_flow = nav_cf_fv, dates = effective_date),
      tvpi = -sum(distributions) / sum(contributions),
      dva = sum(dva),
      alpha = log(1 + irr_fv),
      bench_irr = -1 + exp(log(1 + irr) - alpha)) %>%
    dplyr::select(pm_fund_portfolio, irr, bench_irr, tvpi, pme, dva) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(pm_fund_category_description = 'TOTAL', pm_fund_id = 'TOTAL')


  # Aggregating category metrics
  metrics_rollup_category = data_prep %>%
    dplyr::left_join(pmfi_tbl %>% dplyr::select(pm_fund_id, pm_fund_category_description, pm_fund_portfolio)) %>%
    dplyr::group_by(pm_fund_portfolio, pm_fund_category_description, effective_date) %>%
    dplyr::summarize(
      nav_cf_fv = sum(nav_cf * index_factor),
      nav_cf = sum(nav_cf),
      contributions_fv = sum(contributions * index_factor),
      distributions_fv = sum(distributions * index_factor),
      dva = sum((contributions + distributions) * index_factor),
      contributions = sum(contributions),
      distributions = sum(distributions)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(pm_fund_portfolio, pm_fund_category_description) %>%
    dplyr::summarize(
      pme = -sum(distributions_fv) / sum(contributions_fv),
      irr = calc_irr(cash_flow = nav_cf, dates = effective_date),
      irr_fv = calc_irr(cash_flow = nav_cf_fv, dates = effective_date),
      tvpi = -sum(distributions) / sum(contributions),
      dva = sum(dva),
      alpha = log(1 + irr_fv),
      bench_irr = -1 + exp(log(1 + irr) - alpha)) %>%
    dplyr::select(pm_fund_portfolio, pm_fund_category_description, irr, bench_irr, tvpi, pme, dva) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(pm_fund_id = 'TOTAL')



  # Caluclating fund metrics
  metrics_rollup_fund = data_prep %>%
    dplyr::left_join(pmfi_tbl %>% dplyr::select(pm_fund_id, pm_fund_category_description, pm_fund_portfolio)) %>%
    dplyr::group_by(pm_fund_portfolio, pm_fund_category_description, pm_fund_id) %>%
    dplyr::summarize(
      irr = calc_irr(cash_flow = nav_cf, dates = effective_date),
      irr_fv = calc_irr(cash_flow = nav_cf*index_factor, dates = effective_date),
      tvpi = -sum(distributions) / sum(contributions),
      dva = sum((contributions + distributions) * index_factor),
      pme = -sum(distributions*index_factor) / sum(contributions*index_factor),
      alpha = log(1 + irr_fv),
      bench_irr = -1 + exp(log(1 + irr) - alpha)
    )  %>%
    dplyr::select(pm_fund_portfolio, pm_fund_category_description, pm_fund_id, irr, bench_irr, tvpi, pme, dva) %>%
    dplyr::ungroup()


  metrics_all = dplyr::bind_rows(metrics_rollup_portfolio, metrics_rollup_category, metrics_rollup_fund)

  return(metrics_all)

}
