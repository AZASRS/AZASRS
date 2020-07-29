
#' Test ... for rollup
#'
#' @description TRUE is NOT a rollup, FALSE is a rollup (i.e. grouped beyond fund_id)
#' @param ... should be a column title from pm_fund_info (or multiple)
#' @export
test_is_not_rollup <- function(...) {
  # Helper function for determing whether or not data is rolled up
  # A TRUE response means that it will not be rolled up
  any(c("pm_fund_id", "pm_fund_description", "pm_fund_common_name") %in% c(
    rlang::enquos(...) %>%
      purrr::map(rlang::quo_name) %>%
      unlist()
  ))
}

#' Calculate previous year and quarter
#'
#' @description Subracts years and quarters to simplify calculations
#' @param start_date is a string (format yyyy-dd-mm)
#' @param years is the number of years to subract (integer)
#' @param qtrs is the number of quarters to subract (integer)
#' @export
calc_previous_year_qtr <- function(end_date = get_value_date(), years = 0, qtrs = 0) {
  end_date <- lubridate::as_date(end_date)
  previous_year <- end_date - lubridate::years(years)
  previous_year_qtr <- lubridate::"%m-%"(previous_year, months(3 * qtrs))
  previous_year_qtr <- lubridate::round_date(previous_year_qtr, unit = "quarter") - lubridate::days(1)
  return(previous_year_qtr)
}


#' Calculate a future quarter date
#'
#' @description Calculate any quarter in the future (via # of quarters)
#' @param start_date is a string (format yyyy-dd-mm)
#' @param n is the number of quarters to add
#' @export
calc_add_qtrs <- function(start_date, n) {
  lubridate::`%m+%`(
    lubridate::parse_date_time(start_date, c("ymd", "mdy", "dmy")) %>% lubridate::ceiling_date("quarters", change_on_boundary = T),
    months(n * 3)
  ) %>%
    lubridate::ymd() - lubridate::days(1)
}


#' @export
filled_list_of_dates <- function(start_date = "1969-12-31", end_date = get_value_date(), time_delta = "quarters") {
  quarter_ends <- c("-03-31", "-06-30", "-09-30", "-12-31") # ensure dates land on proper NAV dates

  if (is.na(!match(substring(start_date, 5), quarter_ends)) | is.na(!match(substring(end_date, 5), quarter_ends))) {
    warning(paste0("Your start_date or end_date needs to end in one of the following ", quarter_ends))

    start_date <- lubridate::as_date(start_date)
    end_date <- lubridate::as_date(end_date) + lubridate::days(1) # add 1 to include end_date in results
    date_seq <- seq(start_date, end_date, by = time_delta)
    final_dates <- tibble::tibble(date = date_seq) %>%
      dplyr::mutate(date = lubridate::round_date(date, unit = "quarters") - lubridate::days(1)) %>% # round dates to fix
      dplyr::arrange(date) %>%
      dplyr::rename(effective_date = date)
  }

  start_date <- lubridate::as_date(start_date)
  end_date <- lubridate::as_date(end_date) + lubridate::days(1) # add 1 to include end_date in results
  date_seq <- seq(start_date, end_date, by = time_delta)
  final_dates <- tibble::tibble(date = date_seq) %>%
    dplyr::mutate(date = lubridate::round_date(date, unit = "quarters") - lubridate::days(1)) %>% # round dates to fix
    dplyr::arrange(date) %>%
    dplyr::rename(effective_date = date)

  return(final_dates)
}

#' @export
default_benchmark_lookup <- tibble::tibble(
  pm_fund_portfolio = c("Credit", "PE", "RE"),
  benchmark_id = c("ODCE", "R2K-ACWI", "LSTA+250")
)


#' Find most recent FYTD compared to end_date
#'
#' @description Function returns fiscal year to date start date and number of quarters
#' @param end_date 'yyyy-mm-dd' where start_date and n_qtrs will be calculated
#' @export
calc_fytd_metadata <- function(end_date) {
  if (stringr::str_detect(end_date, "-06-30")) {
    start_date <- calc_add_qtrs(end_date, -4)
    n_qtrs <- 4
  } else if (stringr::str_detect(end_date, "-09-30")) {
    start_date <- calc_add_qtrs(end_date, -1)
    n_qtrs <- 1
  } else if (stringr::str_detect(end_date, "-12-31")) {
    start_date <- calc_add_qtrs(end_date, -2)
    n_qtrs <- 2
  } else if (stringr::str_detect(end_date, "-03-31")) {
    start_date <- calc_add_qtrs(end_date, -3)
    n_qtrs <- 3
  } else {
    stop("get_fytd_metadata error: your date does not match a quarter end")
  }
  return(list(start_date = start_date, n_qtrs = n_qtrs))
}


#' Find most recent CYTD compared to end_date
#'
#' @description Function returns calendar year to date start date and number of quarters
#' @param end_date 'yyyy-mm-dd' where start_date and n_qtrs will be calculated
#' @export
calc_cytd_metadata <- function(end_date) {
  if (stringr::str_detect(end_date, "-12-31")) {
    start_date <- calc_add_qtrs(end_date, -4)
    n_qtrs <- 4
  } else if (stringr::str_detect(end_date, "-03-31")) {
    start_date <- calc_add_qtrs(end_date, -1)
    n_qtrs <- 1
  } else if (stringr::str_detect(end_date, "-06-30")) {
    start_date <- calc_add_qtrs(end_date, -2)
    n_qtrs <- 2
  } else if (stringr::str_detect(end_date, "-09-30")) {
    start_date <- calc_add_qtrs(end_date, -3)
    n_qtrs <- 3
  } else {
    stop("get_fytd_metadata error: your date does not match a quarter end")
  }
  return(list(start_date = start_date, n_qtrs = n_qtrs))
}

#' @export
tibble_to_zoo_list <- function() {
  print("hi")
}


#' Calculate time difference
#'
#' @description Quarters, Years, etc. need to be counted
#' @param start_date is the first date
#' @param end_date is the end date
calc_time_delta <- function(.data, start_date, end_date) {
  # First calculate number of days
  .data %>%
    dplyr::mutate(
      period = round(as.integer(difftime(end_date, start_date, units = "days")) / 365, 2),
      period = dplyr::case_when(
        period == 0.00 ~ "0 Years",
        period == 0.25 ~ "1 Quarter",
        period == 0.50 ~ "6 Months",
        period == 1.00 ~ "1 Year",
        period == 3.00 ~ "3 Years",
        period == 5.00 ~ "5 Years",
        period == 7.00 ~ "7 Years",
        period == 10.00 ~ "10 Years",
        TRUE ~ paste(as.character(round(period, 2)), " Years")
      )
    )
}
