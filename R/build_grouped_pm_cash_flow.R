#' Build PM "Cash Flow" and utilizing NAV to simulate where necessary
#'
#' @description Combine cash flow and NAV data to create a "cash flow" for private market calculations
#' @param .data is from clean_nav_cf()
#' @param ... aggregation choices from from pm_fund_info (i.e. pm_fund_portfolio, pm_fund_category, pm_fund_id)
#' @param start_date is the start date of analysis
#' @param end_date is the cutoff date of analysis, typically a value date
#' @param itd is a boolean that determines whether itd (incpetion to date) is included, overrides start date
#' @param cash_adjusted is a boolean that determines if cash is adjusted NAV should be used
#' @param nav_daily is the object of get_pm_nav_daily()
#' @param cf_daily is the object of get_pm_cash_flow_daily()
#' @param bench_daily is the object of get_benchmark_daily_index()
#' @param bench_relationships is the object of get_benchmark_fund_relationship()
#' @param pm_fund_info is the object of get_pm_fund_info()
#' @export
build_grouped_pm_cash_flow <- function(...,
                                       con = AZASRS_DATABASE_CONNECTION(),
                                       start_date = "2019-06-30",
                                       end_date = get_value_date(con = con),
                                       itd = FALSE,
                                       cash_adjusted = FALSE,
                                       nav_daily = get_pm_nav_daily(con = con),
                                       cf_daily = get_pm_cash_flow_daily(con = con),
                                       bench_daily = get_benchmark_daily_index(con = con, benchmark_type = "PVT", return_tibble = TRUE),
                                       bench_relationships = get_benchmark_fund_relationship(con = con, bench_type = "PVT", return_tibble = TRUE),
                                       pm_fund_info = get_pm_fund_info(con = con)) {

  # Test to see whether or not data needs to be rolled up
  IS_NOT_AGGREGATED <- test_is_not_rollup(...)

  # ITD failsafe - ensure start_date is before earliest possible pm_fund_cash_flow
  if (itd) {
    start_date <- "2004-06-30"
  }

  nav_daily <- nav_daily %>% dplyr::filter(nav != 0)
  cf_daily <- cf_daily %>% dplyr::filter(cash_flow != 0)

  nav_prep <- nav_daily %>%
    filter_nav_on_dates(start_date = start_date, end_date = end_date, itd = itd) %>%
    append_nav_has_reported(end_date = end_date) %>%
    convert_start_date_nav_to_negative(start_date = start_date)

  cf_prep <- cf_daily %>%
    filter_cf_between_dates(start_date = start_date, end_date = end_date, itd = itd)

  nav_cf <- merge_nav_and_cf(nav_prep, cf_prep, end_date = end_date, cash_adjusted = cash_adjusted, pm_fund_info = pm_fund_info) %>%
    # filter_dates(start_date = start_date, end_date = end_date, itd = itd, ...) %>%
    clean_nav_cf(pm_fund_info = pm_fund_info) %>%
    dplyr::mutate(nav = dplyr::if_else(effective_date == start_date, -1 * nav, nav))

  # Filter out funds that are not active for the full start - end period.
  # Not applicable to aggregated funds, would filter out important data
  if (IS_NOT_AGGREGATED) {
    if (!itd) {
      nav_cf <- nav_cf %>%
        dplyr::group_by(pm_fund_info_id) %>%
        dplyr::filter(min(effective_date) <= start_date) %>%
        dplyr::filter(max(effective_date) >= end_date) %>%
        dplyr::ungroup()
    }
  }

  # Join benchmark info and adjust calculations before grouping
  joined_data <- nav_cf %>%
    dplyr::left_join(bench_relationships, by = "pm_fund_info_id") %>%
    dplyr::left_join(bench_daily, by = c("benchmark_info_id", "effective_date"))

  calculated_data <- joined_data %>%
    dplyr::mutate(
      contributions = dplyr::if_else(cash_flow < 0, cash_flow, 0),
      distributions = dplyr::if_else(cash_flow > 0, cash_flow, 0),
      adj_cf_fv = adjusted_cash_flow * index_fv,
      contributions_fv = dplyr::if_else(adj_cf_fv < 0, adj_cf_fv, 0),
      distributions_fv = dplyr::if_else(adj_cf_fv > 0, adj_cf_fv, 0)
    ) %>%
    dplyr::group_by(..., effective_date) %>%
    dplyr::summarize(
      adj_cf_fv = sum(adj_cf_fv),
      dva = sum(adjusted_cash_flow * index_fv),
      contributions_fv = sum(contributions_fv),
      distributions_fv = sum(distributions_fv),
      contributions = sum(contributions),
      distributions = sum(distributions),
      adjusted_cash_flow = sum(adjusted_cash_flow),
      nav = sum(nav),
      cash_flow = sum(cash_flow)
    ) %>%
    dplyr::ungroup()
}




# All of the following functions simply only serve the use case to calculate the grouped IRR above.

#' Filter Private market NAV on dates
#'
#' @description Finds start / end date NAV and can utilize ITD
#' @param .data is from get_pm_nav_daily()
#' @param start_date is a string (format yyyy-dd-mm)
#' @param end_date is a string (format yyyy-dd-mm)
#' @param itd TRUE / FALSE for itd (overwrites start_date if TRUE)
filter_nav_on_dates <- function(.data, start_date, end_date, itd) {
  if (itd) {
    # ITD does not require a start NAV (covered by cash flow)
    # ITD may require an ending nav (if it is not closed)
    .data %>%
      dplyr::filter(effective_date == end_date)
  } else {
    # Both are captured if not ITD, however, some may  not have reported yet
    .data %>%
      dplyr::filter(effective_date == start_date | effective_date == end_date)
  }
}


#' Append column if NAV has reported at end date
#'
#' @description Looks for NAV at end_date and appends TRUE / FALSE column
#' @param .data is from get_pm_nav_daily() after filter_nav_on_dates()
#' @param end_date is a string (format yyyy-dd-mm)
append_nav_has_reported <- function(.data, end_date) {
  .data %>%
    dplyr::group_by(pm_fund_id) %>%
    dplyr::mutate(has_reported = any(effective_date == end_date)) %>%
    dplyr::ungroup() %>%
    dplyr::select(has_reported, dplyr::everything())
}


#' Convert first NAV to negative
#'
#' @description Simulates cash flow at start_date from NAV
#' @param .data is from get_pm_nav_daily() after append_nav_has_reported()
#' @param start_date is a string (format yyyy-dd-mm)
#' @param itd TRUE / FALSE for itd (overwrites start_date if TRUE)
convert_start_date_nav_to_negative <- function(.data, start_date) {
  # The -1*nav at the start date stands in as a cash flow
  .data %>%
    dplyr::group_by(pm_fund_id) %>%
    dplyr::mutate(nav = dplyr::if_else(
      effective_date == start_date,
      -1 * nav,
      nav
    )) %>%
    dplyr::ungroup()
}


#' Collect cash flow between dates
#'
#' @description Includes start and end dates, allows for ITD
#' @param .data is from get_pm_cash_flow_daily()
#' @param start_date is a string (format yyyy-dd-mm)
#' @param end_date is a string (format yyyy-dd-mm)
#' @param itd TRUE / FALSE for itd (overwrites start_date if TRUE)
filter_cf_between_dates <- function(.data, start_date, end_date, itd) {
  if (itd) {
    .data %>%
      dplyr::filter(effective_date <= end_date)
  } else {
    .data %>%
      dplyr::filter(
        effective_date >= start_date,
        effective_date <= end_date
      )
  }
}


#' Merge NAV and CF
#'
#' @description Merging both NAV and CF and handle complexity of rollup aggregation
#' @param .nav_data is from get_pm_nav_daily() through convert_start_date to negative()
#' @param .cf_data is from get_pm_cash_flow_daily() through filter_cf_between_dates
#' @param end_date is a string (format yyyy-dd-mm)
#' @param cash_adjusted is TRUE / FALSE to allow for cash adjusted NAV at next quarter
#' @param pm_fund_info from get_pm_fund_info()
merge_nav_and_cf <- function(.nav_data, .cf_data, end_date, cash_adjusted, pm_fund_info) {

  # Add a has_reported field to use as a filter later on
  .nav_data <- .nav_data %>% dplyr::mutate(cash_flow = 0)
  .cf_data <- .cf_data %>% dplyr::mutate(nav = 0)

  # Not cash adjusted means to ONLY use funds that have NAV at end_date
  if (!cash_adjusted) {
    .nav_data <- .nav_data %>%
      dplyr::filter(has_reported)

    funds_reported <- .nav_data %>%
      dplyr::select(pm_fund_id) %>%
      dplyr::pull()

    .cf_data <- .cf_data %>%
      dplyr::filter(pm_fund_id %in% funds_reported)

    return(dplyr::bind_rows(.nav_data, .cf_data))
  } else {
    # Find out which funds haven't reported
    funds_not_reported_data <- .nav_data %>%
      dplyr::filter(!has_reported)

    # Pull only names not reported
    funds_not_reported_names <- funds_not_reported_data %>%
      dplyr::select(pm_fund_id) %>%
      dplyr::pull()

    # Filter names to extract cash flows
    # Replace all dates with end date to simulate end_date NAV
    # Reverse signs to signal increase in NAV "contributions become distributions in a sense"
    # Bind rows to previous NAV to sum cash flow on top of start_date NAV (reversed sign because it was flipped earlier in pipeline)
    cf_as_nav <- .cf_data %>%
      dplyr::filter(pm_fund_id %in% funds_not_reported_names) %>%
      dplyr::mutate(nav = -1 * cash_flow) %>%
      dplyr::select(-cash_flow) %>%
      dplyr::mutate(effective_date = lubridate::as_date(end_date)) %>%
      dplyr::bind_rows(funds_not_reported_data %>% dplyr::mutate(effective_date = lubridate::as_date(end_date), nav = -1 * nav)) %>%
      dplyr::group_by(pm_fund_id, effective_date) %>%
      dplyr::summarize(nav = sum(nav)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(cash_flow = 0) %>%
      dplyr::left_join(pm_fund_info, by = "pm_fund_id")


    # append cash adjusted nav to not reported nav
    .nav_data <- .nav_data %>% bind_rows(cf_as_nav)
    return(dplyr::bind_rows(.nav_data, .cf_data))
  }
}


#' Filter dates from NAV / CF joined
#'
#' @description NAV / CF joined from merge_nav_and_cf() needs to filter out dates IF not aggregated and not ITD. This avoids filtering out important data from roll up calculations
#' @param .data is from merge_nav_and_cf()
#' @param start_date is a string (format yyyy-dd-mm)
#' @param end_date is a string (format yyyy-dd-mm)
#' @param itd is TRUE / FALSE for inception to date
#' @param ... aggregation choices from from pm_fund_info (i.e. pm_fund_portfolio, pm_fund_category, pm_fund_id)
filter_dates <- function(.data, start_date, end_date, itd, ...) {
  # Filter out funds that are not active for the full start - end period.
  # Not applicable to aggregated funds, would filter out important data
  if (test_is_not_rollup(...)) {
    if (!itd) {
      .data <- .data %>%
        dplyr::group_by(pm_fund_id) %>%
        dplyr::filter(min(effective_date) <= start_date) %>%
        dplyr::filter(max(effective_date) >= end_date) %>%
        dplyr::ungroup()
    }
  }
  return(.data)
}


#' Cleans NAV / CF filtered by date data to sum up by pm_fund_id
#'
#' @description NAV / CF joined from filter_dates()
#' @param .data is from filter_dates()
#' @param pm_fund_info from get_pm_fund_info()
clean_nav_cf <- function(.data, pm_fund_info) {
  .data %>%
    dplyr::group_by(pm_fund_id) %>%
    dplyr::mutate(adjusted_cash_flow = nav + cash_flow) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(pm_fund_id, effective_date) %>%
    dplyr::summarize(
      adjusted_cash_flow = sum(adjusted_cash_flow),
      nav = sum(nav),
      cash_flow = sum(cash_flow)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(pm_fund_info, by = "pm_fund_id")
}
