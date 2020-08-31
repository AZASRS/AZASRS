#' Get all account_futures
#'
#' @description Getting all account futures data
#' @param con is a database connection object from AZASRS::AZASRS_DATABASE_CONNECTION()
#' @param return_tibble is a boolean that determines whether or not a tibble is returned instead
#' @return Returns a tibble or SQL result with all pm_fund_info metadata.
#' @export
get_account_futures <- function(con = AZASRS_DATABASE_CONNECTION(), return_tibble = TRUE) {

  dat <- tbl_account_futures(con = con) %>%
    dplyr::left_join(tbl_account_info(con = con) %>%
                       dplyr::select(account_info_id, account_id), by = 'account_info_id')

  if (return_tibble) {
    return(dat %>% tibble::as_tibble())
  } else {
    return(dat)
  }
}
