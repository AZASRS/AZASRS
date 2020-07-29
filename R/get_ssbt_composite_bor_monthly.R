#' Get SSBT composite book of record monthly
#'
#' @description Gets composite book of record monthly
#' @param con is a database connection object from AZASRS::AZASRS_DATABASE_CONNECTION()
#' @param return_tibble is a boolean that determines whether or not a tibble is returned instead
#' @return Returns a tibble with all composite data.
#' @examples
#' get_ssbt_composite_bor_monthly()
#' # A tibble: 69 x 11
#' # ssbt_composite_in~ effective_date   beginning_market~ ending_market_v~ net_cash_flow monthly_return
#' #   <int>               <chr>              <dbl>            <dbl>         <dbl>        <dbl>
#' #    1               2019-04-30          40252843769.      40913563817.    -79339045.    NA
#' #    1               2019-05-31          40913563817.      39862718836.    -308823057.   NA
#' # … with 67 more rows, and 5 more variables: fiscal_ytd_return <dbl>, ssbt_composite_id <chr>,
#' #   ssbt_composite_description <chr>, ssbt_composite_short_description <chr>, benchmark_info_id <int>
#' @export
get_ssbt_composite_bor_monthly <- function(con = AZASRS_DATABASE_CONNECTION(), return_tibble = TRUE) {
  dat <- tbl_ssbt_composite_book_of_record_monthly(con) %>%
    dplyr::left_join(tbl_ssbt_composite_info(con), by = "ssbt_composite_info_id")

  if (return_tibble) {
    return(dat %>% tibble::as_tibble())
  } else {
    return(dat)
  }
}
