#' Get and combine cash flows and nav according to irr.z calc
#'
#' @export
build_nav_cash_flow_combined = function(...,
                                        con = AZASRS_DATABASE_CONNECTION(),
                                        start_date = '2019-03-31',
                                        end_date = get_value_date(con = con),
                                        nav_daily = get_pm_nav_daily(con = con, return_tibble = FALSE),
                                        cash_flow_daily = get_pm_cash_flow_daily(con = con, return_tibble = FALSE),
                                        itd = FALSE,
                                        return_tibble = FALSE){

  # Start date well before any potential start
  if(itd){ start_date = '1899-12-31'}
  pmfi = get_pm_fund_info(con = con, return_tibble = FALSE)

  # Create dummy NAVs for start and end
  nav_start_dummy = nav_daily %>%
    dplyr::select(pm_fund_id, effective_date, nav) %>%
    dplyr::mutate(nav = 0, effective_date = start_date) %>%
    dplyr::group_by(pm_fund_id, effective_date) %>%
    dplyr::summarize(nav = sum(nav, na.rm = TRUE)) %>%
    dplyr::ungroup()

  nav_end_dummy = nav_daily %>%
    dplyr::select(pm_fund_id, effective_date, nav) %>%
    dplyr::mutate(nav = 0, effective_date = end_date) %>%
    dplyr::group_by(pm_fund_id, effective_date) %>%
    dplyr::summarize(nav = sum(nav, na.rm = TRUE)) %>%
    dplyr::ungroup()

  # Get all fund NAV to filter start
  nav_start = nav_daily %>%
    dplyr::filter(nav != 0) %>% # filter to avoid end nav of 0
    dplyr::filter(effective_date == start_date) %>%
    dplyr::mutate(nav = -1*nav) %>% # converts to first "cash flow" for calculations
    dplyr::select(pm_fund_id, effective_date, nav)

  nav_end = nav_daily %>%
    dplyr::filter(nav != 0) %>% # filter to avoid end nav of 0
    dplyr::filter(effective_date == end_date) %>%
    dplyr::select(pm_fund_id, effective_date, nav)

  # Dataframe that has nav of 0 or -nav at start_date & nav or 0 at end_date
  nav = dplyr::union_all(nav_start, nav_end) %>%
    dplyr::union_all(nav_start_dummy) %>%
    dplyr::union_all(nav_end_dummy) %>%
    dplyr::group_by(pm_fund_id, effective_date) %>%
    dplyr::summarize(nav = sum(nav, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(adj_cf = nav)

  # Get all fund CASH FLOW raw, will filter in next step
  if(!itd){
    cash_flow_daily = cash_flow_daily %>%
      dplyr::group_by(pm_fund_id) %>%
      dplyr::filter(min(effective_date, na.rm = TRUE) <= start_date) %>%
      dplyr::ungroup()
  }
  cash_flow = cash_flow_daily %>%
    dplyr::group_by(pm_fund_id) %>%
    dplyr::ungroup() %>%
    dplyr::filter(cash_flow != 0,
                  effective_date < end_date,
                  effective_date >= start_date) %>%
    dplyr::mutate(adj_cf = cash_flow) %>%
    dplyr::select(pm_fund_id, effective_date, adj_cf)

  # Concatenate and summarize
  nav_cf_combo = dplyr::union_all(nav, cash_flow) %>%
    dplyr::group_by(pm_fund_id, effective_date) %>%
    dplyr::summarise(adj_cf = sum(adj_cf)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(pmfi, by = 'pm_fund_id')

  dat = nav_cf_combo %>%
    dplyr::filter(effective_date > '1901-01-01') %>%
    dplyr::filter(adj_cf != 0) %>%
    dplyr::group_by(..., effective_date) %>%
    dplyr::arrange(..., effective_date) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(distributions = dplyr::if_else(adj_cf > 0, adj_cf, 0),
           contributions = dplyr::if_else(adj_cf < 0, adj_cf, 0)) %>%
    dplyr::group_by(pm_fund_id) %>%
    dplyr::mutate(nav = if_else(effective_date == min(effective_date, na.rm = TRUE) | effective_date == max(effective_date, na.rm = TRUE), adj_cf, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(..., effective_date) %>%
    dplyr::summarize(nav_cf = sum(adj_cf, na.rm = TRUE),
                     distributions = sum(distributions, na.rm = TRUE),
                     contributions = sum(contributions, na.rm = TRUE),
                     nav = sum(nav, na.rm = TRUE)) %>%
    dplyr::ungroup()

  if(return_tibble){
    return(dat %>% tibble::as_tibble())
  } else{
    return(dat)
  }
}



