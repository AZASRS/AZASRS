
#' Query ssbt_composite_info table from database
#' @description Gets all ssbt composite information.
#' @param con is a database connection object from AZASRS::AZASRS_DATABASE_CONNECTION()
#' @param return_tibble is a boolean that determines whether or not a tibble is returned instead
#' @return Returns a tibble or database object
#' @examples
#' get_ssbt_composite_info()
#' # A tibble: 36 x 5
#' # ssbt_composite_info… ssbt_composite_id ssbt_composite_description  ssbt_composite_short_des… benchmark_info_…
#' # <int>                <chr>               <chr>                <chr>            <int>
#' #  1                 ASRSA001           TOTAL PLAN            Total Fund          NA
#' #  2                 ASRSA007           TOTAL EQUITY          Equity              NA
#' #  3                 ASRSA007           TOTAL PUBLIC EQUITY   Public Equity       NA
#' # ... with 33 more rows
#' @export
get_ssbt_composite_info <- function(con = AZASRS_DATABASE_CONNECTION(), return_tibble = TRUE) {
  dat <- tbl_ssbt_composite_info(con)

  if (return_tibble) {
    return(dat %>% tibble::as_tibble())
  } else {
    return(dat)
  }
}
