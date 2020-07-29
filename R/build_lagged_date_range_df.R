#' @description Creates a dataframe of a range of dates
#'
#' @param con is a database connection object from AZASRS::AZASRS_DATABASE_CONNECTION()
#' @param start_date is the start date of analysis
#' @param end_date is the cutoff date of analysis, the object of get_value_date()
#' @param time_delta is the change in time where the default is "quarters"
#' @param n_qtrs determines the number of quarters, where the default is 4
#' @return
#' @export
build_lagged_date_range_df <- function(con = AZASRS_DATABASE_CONNECTION(), start_date = "2017-12-31", end_date = get_value_date(con = con), time_delta = "quarters", n_qtrs = 4) {
  dat <- filled_list_of_dates(start_date = start_date, end_date = end_date, time_delta = time_delta) %>%
    dplyr::mutate(start_date = dplyr::lag(effective_date, n_qtrs)) %>% # number of quarters, 4 = 1yr
    dplyr::rename(end_date = effective_date) %>%
    tidyr::drop_na(start_date) %>%
    dplyr::select(start_date, end_date)
  return(dat)
}
