
#' @description Gets pm_nav_daily table from database
#' @param con is a database connection object from AZASRS::AZASRS_DATABASE_CONNECTION()
#' @param return_tibble is a boolean that determines whether or not a tibble is returned instead
#' @return Returns a tibble or database object
#' @examples
#' get_pm_nav_daily()
#' # A tibble: 8,324 x 28
#' # pm_fund_info_id effective_date    nav     pm_fund_id    pm_fund_descrip…  pm_fund_common_… vintage commit unfunded
#' # <int>                <date>        <dbl>       <chr>     <chr>               <chr>              <int>  <int>    <int>
#' #  1                 2019-06-30      8.60e6      Hgh19     AP Mezzanine Pa…    HPS Mezz 2019      2019   6.00e8   3.95e8
#' #  1                 2019-09-30      8.51e7      Hgh19     AP Mezzanine Pa…    HPS Mezz 2019      2019   6.00e8   3.95e8
#' #  1                 2019-12-31      1.03e8      Hgh19     AP Mezzanine Pa…    HPS Mezz 2019      2019   6.00e8   3.95e8
#'
#' # … with 8,321 more rows, and 19 more variables: legacy <chr>, specialist <chr>, invest_end <date>,
#' #   term_end <date>, extension <dbl>, ext_time <dbl>, ext_used <dbl>, fee_cat <chr>, consultant <chr>,
#' #   adv_board <lgl>, obsvr <lgl>, fund_size_m <dbl>, closed <chr>, pm_fund_category <chr>,
#' #   pm_fund_category_description <chr>, pm_fund_portfolio <chr>, pm_fund_sponsor <chr>, pm_fund_city <chr>,
#' #   pm_fund_sector <chr>
#' @export
get_pm_nav_daily <- function(con = AZASRS_DATABASE_CONNECTION(), return_tibble = TRUE) {
  dat <- tbl_pm_fund_nav_daily(con = con) %>%
    dplyr::left_join(tbl_view_all_pm_fund_info(con = con), by = "pm_fund_info_id")

  if (return_tibble) {
    return(dat %>%
      tibble::as_tibble() %>%
      dplyr::mutate(effective_date = lubridate::as_date(effective_date)))
  } else {
    return(dat)
  }
}
