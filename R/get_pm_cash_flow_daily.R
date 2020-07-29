#' Daily private market cash flows
#'
#' @description pm_cash_flow_daily table from database joined with metadata
#' @param con is a database connection object from AZASRS::AZASRS_DATABASE_CONNECTION()
#' @param return_tibble is a boolean that determines whether or not a tibble is returned instead
#' @return Returns a tibble or database object.
#' @examples
#' get_pm_cash_flow_daily()
#' # A tibble: 11,586 x 30
#' # pm_fund_info_id effective_date cash_flow contributions distributions pm_fund_id pm_fund_descrip..
#' #  <int>            <date>           <dbl>     <dbl>         <dbl>   <chr>      <chr>
#' #    1            2019-06-17       -8.80e6   -8800000          0     Hgh19       AP Mezzanine Pa…
#' #    1            2019-08-09       -1.70e6   -1700000          0     Hgh19       AP Mezzanine Pa…
#' #    1            2019-08-29       -2.00e7   -20035179.      F 0     Hgh19       AP Mezzanine Pa…
#' # ... with  11,583 more rows, and 23 more variables: pm_fund_common_name <chr>, vintage <int>, commit <int>,
#' #   unfunded <int>, legacy <chr>, specialist <chr>, invest_end <date>, term_end <date>, extension <dbl>,
#' #   ext_time <dbl>, ext_used <dbl>, fee_cat <chr>, consultant <chr>, adv_board <lgl>, obsvr <lgl>,
#' #   fund_size_m <dbl>, closed <chr>, pm_fund_category <chr>, pm_fund_category_description <chr>,
#' #   pm_fund_portfolio <chr>, pm_fund_sponsor <chr>, pm_fund_city <chr>, pm_fund_sector <chr>
#' @export
get_pm_cash_flow_daily <- function(con = AZASRS_DATABASE_CONNECTION(), return_tibble = TRUE) {
  dat <- tbl_pm_fund_cash_flow_daily(con = con) %>%
    dplyr::mutate(
      contributions = ifelse(cash_flow < 0, cash_flow, 0),
      distributions = ifelse(cash_flow > 0, cash_flow, 0)
    ) %>%
    dplyr::left_join(tbl_view_all_pm_fund_info(con = con), by = "pm_fund_info_id")

  if (return_tibble) {
    return(dat %>%
      tibble::as_tibble() %>%
      dplyr::mutate(effective_date = lubridate::as_date(effective_date)))
  } else {
    return(dat)
  }
}
