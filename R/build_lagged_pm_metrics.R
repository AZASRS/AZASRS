#' @description
#' @param ... aggregation choices from from pm_fund_info (i.e. pm_fund_portfolio, pm_fund_category, pm_fund_id)
#' @param con is a database connection object from AZASRS::AZASRS_DATABASE_CONNECTION()
#' @param start_date is the start date of analysis
#' @param end_date is the cutoff date of analysis, the object of get_value_date()
#' @param time_delta is the change in time where the default is "quarters"
#' @param n_qtrs determines the number of quarters, where the default is 4
#' @param itd is a boolean that determines if itd (inception to date) is included
#' @param return_calcs is a boolean that determines if calculations are returned
#' @return
#' @export
build_lagged_pm_metrics <- function(...,
                                    con = AZASRS_DATABASE_CONNECTION(),
                                    start_date = "2016-12-31",
                                    end_date = get_value_date(con = con),
                                    time_delta = "quarters",
                                    n_qtrs = 4,
                                    itd = FALSE,
                                    return_calcs = TRUE) {

  # benchmark_lookup is a tibble to match for DVA & PME calcs
  # benchmark_lookup = tibble::tibble(pm_fund_portfolio = c("Credit", "PE",   "RE"), benchmark_id = c("ODCE",   "ODCE", "LSTA+250"))
  test_exists <- dplyr::enquos(...)
  if (is.null(test_exists$benchmark_lookup)) {
    benchmark_lookup <- default_benchmark_lookup
  }

  min_nav_date <- tbl_pm_fund_nav_daily(con) %>%
    dplyr::select(effective_date) %>%
    dplyr::summarize(effective_date = min(effective_date, na.rm = TRUE)) %>%
    dplyr::pull()

  # bench_tbl = build_benchmark_fv_index_factor(...,
  #                                             con = con,
  #                                             start_date = min_nav_date,
  #                                             value_date = end_date,
  #                                             return_tibble = FALSE) %>%
  #   dplyr::select(-benchmark_info_id, -index_factor) %>%
  #   tibble::as_tibble()
  #
  # bench = benchmark_lookup %>%
  #   dplyr::left_join(bench_tbl)

  if (itd) {
    dates_df <- tibble::tibble(start_date = lubridate::as_date("1900-12-31"), end_date = lubridate::as_date(end_date), itd = TRUE)
  } else {
    dates_df <- build_lagged_date_range_df(
      con = con, start_date = start_date, end_date = end_date,
      time_delta = time_delta, n_qtrs = n_qtrs
    ) %>%
      dplyr::mutate(itd = FALSE)
  }

  dat <- dates_df %>%
    dplyr::group_by(start_date, end_date) %>%
    dplyr::mutate(nav_cash_flow = list(build_nav_cash_flow_combined(...,
      con = con,
      start_date = start_date,
      end_date = end_date,
      itd = itd,
      return_tibble = TRUE
    )))

  # if(itd){
  #   dat_itd_prep = build_nav_cash_flow_combined(...,
  #                                          con = con,
  #                                          start_date = start_date,
  #                                          end_date = end_date,
  #                                          itd = itd,
  #                                          return_tibble = TRUE) %>%
  #     dplyr::left_join(bench)
  #
  #   dat_itd_prep_end = dat_itd_prep %>%
  #     dplyr::group_by(...) %>%
  #     dplyr::filter(effective_date == min(effective_date, na.rm = TRUE)) %>%
  #     dplyr::select(..., last_index_value = index_value)
  #
  #   dat_itd = dat_itd_prep %>%
  #     dplyr::left_join(dat_itd_prep_end) %>%
  #     dplyr::mutate(index_factor = last_index_value / index_value) %>%
  #     dplyr::group_by(...) %>%
  #     dplyr::arrange(effective_date) %>%
  #     dplyr::summarize(irr = calc_irr(cash_flow = nav_cf, dates = effective_date),
  #                      tvpi = calc_tvpi(distributions, contributions, nav),
  #                      dpi = calc_dpi(distributions, contributions),
  #                      appreciation = calc_appreciation(contributions+distributions, nav),
  #                      dva = calc_dva(contributions+distributions, index_factor),
  #                      pme = calc_pme(distributions, contributions, nav, index_factor))
  #
  #   min_max_dates = get_pm_nav_daily(con = con, return_tibble = FALSE) %>%
  #     dplyr::filter(effective_date %in% qtr_dates) %>%
  #     dplyr::group_by(...) %>%
  #     dplyr::summarize(start_date = min(effective_date, na.rm = TRUE), end_date = max(effective_date, na.rm = TRUE)) %>%
  #     dplyr::ungroup() %>%
  #     tibble::as_tibble()
  #
  #   dat_itd_final = dat_itd %>%
  #     dplyr::left_join(min_max_dates) %>%
  #     dplyr::left_join(benchmark_lookup) %>%
  #     dplyr::mutate(itd = TRUE)
  #
  # }



  tmp_irr_calc <- function(df) {
    deselect_cols <- paste(c("nav_cf", "distributions", "contributions", "nav", "index_value", "index_factor"), collapse = "|")
    deselect_cols_lower_level <- paste(c("effective_date", "nav_cf", "distributions", "contributions", "nav", "index_value", "index_factor"), collapse = "|")

    dat_prep <- df %>%
      dplyr::group_by_at(names(df)[-grep(deselect_cols, names(df))]) %>%
      dplyr::summarize(
        nav_cf = sum(nav_cf, na.rm = TRUE),
        distributions = sum(distributions, na.rm = TRUE),
        contributions = sum(contributions, na.rm = TRUE),
        nav = sum(nav, na.rm = TRUE)
      )

    dat <- dat_prep %>%
      dplyr::group_by_at(names(dat_prep)[-grep(deselect_cols_lower_level, names(dat_prep))]) %>%
      dplyr::arrange(effective_date) %>%
      dplyr::summarize(
        irr = calc_irr(cash_flow = nav_cf, dates = effective_date),
        tvpi = calc_tvpi(distributions, contributions, nav),
        dpi = calc_dpi(distributions, contributions),
        appreciation = calc_appreciation(contributions + distributions, nav)
      )

    return(dat)
  }

  #   dat_with_bench = dat_prep %>%
  #     dplyr::left_join(bench) # variable from parent function
  #
  #   dat_with_bench_end = dat_with_bench %>%
  #     dplyr::group_by_at(names(dat_with_bench)[-grep(deselect_cols_lower_level, names(dat_with_bench))]) %>%
  #     dplyr::filter(effective_date == max(effective_date, na.rom = TRUE)) %>%
  #     dplyr::ungroup() %>%
  #     dplyr::select(benchmark_id, index_value) %>%
  #     dplyr::distinct(benchmark_id, index_value) %>%
  #     dplyr::rename(last_index_value = index_value)
  #
  #   dat_with_bench_factor = dat_with_bench %>%
  #     dplyr::left_join(dat_with_bench_end, by = 'benchmark_id') %>%
  #     dplyr::mutate(index_factor = last_index_value / index_value)
  #
  #   dat = dat_with_bench_factor %>%
  #     dplyr::group_by_at(names(dat_with_bench_factor)[-grep(deselect_cols_lower_level, names(dat_with_bench_factor))]) %>%
  #     dplyr::arrange(effective_date) %>%
  #     dplyr::summarize(irr = calc_irr(cash_flow = nav_cf, dates = effective_date),
  #                      tvpi = calc_tvpi(distributions, contributions, nav),
  #                      dpi = calc_dpi(distributions, contributions),
  #                      appreciation = calc_appreciation(contributions+distributions, nav),
  #                      dva = calc_dva(contributions+distributions, index_factor),
  #                      pme = calc_pme(distributions, contributions, nav, index_factor))
  #
  #   return(dat)
  # }

  dat <- dat %>%
    dplyr::group_by(start_date, end_date) %>%
    dplyr::mutate(irr = purrr::pmap(
      .l = list(nav_cash_flow),
      .f = tmp_irr_calc
    )) %>%
    dplyr::select(-nav_cash_flow) %>%
    tidyr::unnest(cols = c(irr)) %>%
    dplyr::mutate(
      lagged_period = round(as.integer(end_date - start_date) / 365, 2),
      lagged_period = as.character(dplyr::if_else(lagged_period >= 1, round(lagged_period), lagged_period)),
      lagged_period = dplyr::if_else(
        lagged_period == "0.25", "3 Months",
        dplyr::if_else(
          lagged_period == "0.50" | lagged_period == "0.5", "6 Months",
          paste(lagged_period, "Year")
        )
      ),
      lagged_period = dplyr::if_else(itd == TRUE, "ITD", lagged_period)
    ) %>%
    dplyr::ungroup()

  # if(itd){
  #   dat = dplyr::bind_rows(dat, dat_itd_final)
  # }

  return(dat)
}
