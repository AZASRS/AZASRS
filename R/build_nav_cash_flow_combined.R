#' @description Get and combine cash flows and nav according to irr.z calc
#' @param ... grouping variables (pm_fund_id, pm_fund_portfolio, etc.)
#' @param con is a database connection object from AZASRS::AZASRS_DATABASE_CONNECTION()
#' @param start_date is the start date of analysis
#' @param end_date is the cutoff date of analysis
#' @param nav_daily is the object of get_pm_nav_daily()
#' @param cash_flow_daily is the object of get_pm_cash_flow_daily()
#' @param itd is a boolean that determines whether itd (inception to date) is included
#' @param cash_adjusted is a boolean that determines whether cash is adjusted
#' @param return_tibble is a boolean that determines whether or not a tibble is returned instead
#' @return Returns a tibble or database with nav and cash flow values
#' @export
build_nav_cash_flow_combined <- function(...,
                                         con = AZASRS_DATABASE_CONNECTION(),
                                         start_date = "2019-06-30",
                                         end_date = get_value_date(con = con),
                                         nav_daily = get_pm_nav_daily(con = con, return_tibble = TRUE),
                                         cash_flow_daily = get_pm_cash_flow_daily(con = con, return_tibble = TRUE),
                                         itd = FALSE,
                                         cash_adjusted = FALSE,
                                         return_tibble = TRUE) {

  # Determine need to adjust NAV
  tmp_valdate <- get_value_date(con)
  if (cash_adjusted) {
    # Funds that have an end_date NAV
    end_date_nav <- nav_daily %>%
      dplyr::filter(effective_date == end_date)

    # Funds with a valdate but not an end_date NAV
    no_end_date_nav <- nav_daily %>%
      dplyr::filter(effective_date == tmp_valdate) %>%
      dplyr::select(pm_fund_id) %>%
      dplyr::anti_join(end_date_nav, by = "pm_fund_id")

    sum_cash_flow_after_valdate <- no_end_date_nav %>%
      dplyr::left_join(cash_flow_daily %>% dplyr::filter(effective_date >= tmp_valdate), by = "pm_fund_id") %>%
      dplyr::group_by(pm_fund_id) %>%
      dplyr::summarize(sum_cash_flow = sum(-1 * cash_flow, na.rm = TRUE))

    # Replacing nav_daily
    nav_daily <- sum_cash_flow_after_valdate %>%
      dplyr::left_join(nav_daily %>% dplyr::filter(effective_date == tmp_valdate), by = "pm_fund_id") %>%
      dplyr::mutate(
        nav = nav + sum_cash_flow,
        effective_date = end_date
      ) %>%
      dplyr::select(-sum_cash_flow) %>%
      dplyr::bind_rows(nav_daily %>% filter(nav != 0))
  }

  if (itd) {
    # ITD only at value date

    # beg of life cash flows up until either cash flows stop or nav
    nav <- nav_daily %>%
      dplyr::group_by(...) %>%
      dplyr::filter(
        nav != 0,
        effective_date == end_date
      ) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(..., effective_date) %>%
      dplyr::summarize(nav = sum(nav, na.rm = TRUE)) %>%
      dplyr::ungroup()

    cf <- cash_flow_daily %>%
      dplyr::filter(effective_date < end_date) %>%
      dplyr::group_by(..., effective_date) %>%
      dplyr::summarize(cash_flow = sum(cash_flow, na.rm = TRUE)) %>%
      dplyr::ungroup()

    # Prep for union
    nav <- nav %>%
      dplyr::mutate(cash_flow = 0)
    cf <- cf %>%
      dplyr::mutate(nav = 0)

    nav_cf_combo <- dplyr::bind_rows(nav, cf)
  } else {

    # Remove funds who either start in the middle of the date range or do not reach the end
    nav_in_range <- nav_daily %>%
      dplyr::group_by(...) %>%
      dplyr::mutate(
        nav_start = min(effective_date, na.rm = TRUE),
        nav_end = max(effective_date, na.rm = TRUE)
      ) %>%
      dplyr::filter(nav_start <= start_date) %>%
      dplyr::filter(nav_end >= end_date) %>%
      dplyr::ungroup()

    nav <- nav_in_range %>%
      dplyr::filter(effective_date == start_date | effective_date == end_date) %>%
      dplyr::group_by(..., effective_date) %>%
      dplyr::summarize(nav = sum(nav, na.rm = TRUE)) %>%
      dplyr::mutate(nav = dplyr::if_else(effective_date == start_date, -1 * nav, nav)) %>%
      dplyr::ungroup()

    cf_date_filter <- nav %>%
      dplyr::group_by(...) %>%
      dplyr::summarize(
        min_date = min(effective_date, na.rm = TRUE),
        max_date = max(effective_date, na.rm = TRUE)
      ) %>%
      dplyr::ungroup()

    cf <- cash_flow_daily %>%
      dplyr::left_join(cf_date_filter) %>%
      dplyr::filter(effective_date >= min_date, effective_date < max_date) %>%
      dplyr::group_by(..., effective_date) %>%
      dplyr::summarize(cash_flow = sum(cash_flow, na.rm = TRUE)) %>%
      dplyr::ungroup()

    # Prep for union
    nav <- nav %>%
      dplyr::mutate(cash_flow = 0)
    cf <- cf %>%
      dplyr::mutate(nav = 0)

    nav_cf_combo <- dplyr::union_all(nav, cf)
  }

  dat <- nav_cf_combo %>%
    dplyr::group_by(..., effective_date) %>%
    dplyr::summarize(nav_cf = sum(nav, na.rm = TRUE) + sum(cash_flow, na.rm = TRUE)) %>%
    dplyr::arrange(..., effective_date) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      distributions = dplyr::if_else(nav_cf > 0, nav_cf, 0),
      contributions = dplyr::if_else(nav_cf < 0, nav_cf, 0)
    ) %>%
    dplyr::group_by(...) %>%
    dplyr::mutate(nav = if_else(effective_date == min(effective_date, na.rm = TRUE) | effective_date == max(effective_date, na.rm = TRUE), nav_cf, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(..., effective_date) %>%
    dplyr::summarize(
      nav_cf = sum(nav_cf, na.rm = TRUE),
      distributions = sum(distributions, na.rm = TRUE),
      contributions = sum(contributions, na.rm = TRUE),
      nav = sum(nav, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  if (return_tibble) {
    return(dat %>% tibble::as_tibble())
  } else {
    return(dat)
  }
}
