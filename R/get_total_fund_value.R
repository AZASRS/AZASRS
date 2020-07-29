#' Get total fund value
#'
#' @description Finds the most recent fund value for TOTAL FUND. Can be filtered to select by individual date.
#' @param value_date grabs the date from the get_value_data() function
#' @param con is a database connection object from AZASRS::AZASRS_DATABASE_CONNECTION()
#' @return Value from ssbt_composite_book_of_record_daily by date selected (default = valdate).
#' @examples
#' get_total_fund_value(value_date = "2018-06-30")
#' # 39656200393
#' @export
get_total_fund_value <- function(value_date = get_value_date(), con = AZASRS_DATABASE_CONNECTION()) {
  dat <- tbl_ssbt_composite_book_of_record_daily(con) %>%
    dplyr::filter(effective_date == value_date) %>%
    dplyr::left_join(tbl_ssbt_composite_info(con), by = "ssbt_composite_info_id") %>%
    dplyr::filter(ssbt_composite_id == "ASRSA001") %>%
    dplyr::select(ending_market_value) %>%
    dplyr::pull()

  return(dat)
}
