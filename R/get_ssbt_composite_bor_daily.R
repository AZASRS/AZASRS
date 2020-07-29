#' Get SSBT composite book of record daily
#'
#' @description Gets composite book of record daily
#' @param con is a database connection object from AZASRS::AZASRS_DATABASE_CONNECTION()
#' @return Returns a tibble with all composite data but can be filtered.
#' @param return_tibble is a boolean that determines whether or not a tibble is returned instead
#' @examples
#' get_ssbt_composite_bor_daily()
#' # A tibble: 8,728 x 10
#' # ssbt_composite_in~ effective_date   beginning_market~ ending_market_v~ net_cash_flow daily_return ssbt_composite_~
#' #   <int>               <chr>              <dbl>            <dbl>         <dbl>        <dbl>           <chr>
#' #    1               2018-06-30           39655974310.      39656200393.     0         0.0000057       ASRSA001
#' #    1               2018-07-02           39633650853      39565538581     17053151    -0.00215        ASRSA001
#' # … with 8,726 more rows, and 3 more variables: ssbt_composite_description <chr>,
#' #  ssbt_composite_short_description <chr>, benchmark_info_id <int>
#' @export
get_ssbt_composite_bor_daily <- function(con = AZASRS_DATABASE_CONNECTION(), return_tibble = TRUE) {
  dat <- tbl_ssbt_composite_book_of_record_daily(con) %>%
    dplyr::left_join(tbl_ssbt_composite_info(con), by = "ssbt_composite_info_id")

  if (return_tibble) {
    return(dat %>% tibble::as_tibble())
  } else {
    return(dat)
  }
}
