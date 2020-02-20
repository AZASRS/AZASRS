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

  # # Start date well before any potential start
  # if(itd){ start_date = '1900-12-31'}
  #
  # # Get all fund NAV to filter
  # nav = nav_daily %>%
  #   dplyr::filter(nav != 0,
  #                 effective_date <= end_date,
  #                 effective_date >= start_date) %>%
  #   dplyr::group_by(pm_fund_id) %>%
  #   dplyr::filter(effective_date == start_date | effective_date == end_date) %>% # No NAV possible for ITD
  #   dplyr::mutate(max_date = max(effective_date, na.rm = TRUE),
  #                 min_date = min(effective_date, na.rm = TRUE)) %>% # Max / Min possible PER FUND
  #   dplyr::ungroup() %>%
  #   dplyr::mutate(nav = dplyr::if_else(effective_date == min_date, -1*nav, nav)) # Utilizes NAV as a "cash flow" for calculation
  #
  #
  #


  if(itd){
    # ITD only at value date

    #beg of life cash flows up until either cash flows stop or nav
    nav = nav_daily %>%
      dplyr::group_by(...) %>%
      dplyr::filter(nav != 0,
                    effective_date == end_date) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(..., effective_date) %>%
      dplyr::summarize(nav = sum(nav, na.rm = TRUE)) %>%
      dplyr::ungroup()

    cf = cash_flow_daily %>%
      dplyr::filter(effective_date < end_date) %>%
      dplyr::group_by(..., effective_date) %>%
      dplyr::summarize(cash_flow = sum(cash_flow, na.rm = TRUE)) %>%
      dplyr::ungroup()

    # Prep for union
    nav = nav %>%
      dplyr::mutate(cash_flow = 0)
    cf = cf %>%
      dplyr::mutate(nav = 0)

    nav_cf_combo = dplyr::union_all(nav, cf)

  } else{

    # Remove funds who either start in the middle of the date range or do not reach the end
    nav_in_range = nav_daily %>%
      dplyr::group_by(...) %>%
      dplyr::mutate(nav_start = min(effective_date, na.rm = TRUE),
             nav_end = max(effective_date, na.rm = TRUE)) %>%
      dplyr::filter(nav_start <= start_date) %>%
      dplyr::filter(nav_end >= end_date) %>%
      dplyr::ungroup()

    nav = nav_in_range %>%
      dplyr::filter(effective_date == start_date | effective_date == end_date) %>%
      dplyr::group_by(..., effective_date) %>%
      dplyr::summarize(nav = sum(nav, na.rm = TRUE)) %>%
      dplyr::mutate(nav = dplyr::if_else(effective_date == start_date, -1*nav, nav)) %>%
      dplyr::ungroup()

    cf_date_filter = nav %>%
      dplyr::group_by(...) %>%
      dplyr::summarize(min_date = min(effective_date, na.rm = TRUE),
                       max_date = max(effective_date, na.rm = TRUE)) %>%
      dplyr::ungroup()

    cf = cash_flow_daily %>%
      dplyr::left_join(cf_date_filter) %>%
      dplyr::filter(effective_date >= min_date, effective_date < max_date) %>%
      dplyr::group_by(..., effective_date) %>%
      dplyr::summarize(cash_flow = sum(cash_flow, na.rm = TRUE)) %>%
      dplyr::ungroup()

    # Prep for union
    nav = nav %>%
      dplyr::mutate(cash_flow = 0)
    cf = cf %>%
      dplyr::mutate(nav = 0)

    nav_cf_combo = dplyr::union_all(nav, cf)
  }

  dat = nav_cf_combo %>%
    dplyr::group_by(..., effective_date) %>%
    dplyr::summarize(nav_cf = sum(nav, na.rm = TRUE) + sum(cash_flow, na.rm = TRUE)) %>%
    dplyr::arrange(..., effective_date) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(distributions = dplyr::if_else(nav_cf > 0, nav_cf, 0),
           contributions = dplyr::if_else(nav_cf < 0, nav_cf, 0)) %>%
    dplyr::group_by(...) %>%
    dplyr::mutate(nav = if_else(effective_date == min(effective_date, na.rm = TRUE) | effective_date == max(effective_date, na.rm = TRUE), nav_cf, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(..., effective_date) %>%
    dplyr::summarize(nav_cf = sum(nav_cf, na.rm = TRUE),
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



