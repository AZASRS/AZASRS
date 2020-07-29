
get_benchmark_daily_return <- function(con = AZASRS_DATABASE_CONNECTION(), return_tibble = TRUE) {
  dat <- tbl_benchmark_daily_return(con) %>%
    dplyr::left_join(tbl_benchmark_info(con), by = "benchmark_info_id")
  if (return_tibble == TRUE) {
    dat <- dat %>% tibble::as_tibble()
  }
  return(dat)
}


get_benchmark_monthly_return <- function(con = AZASRS_DATABASE_CONNECTION(), return_tibble = TRUE) {
  dat <- tbl_benchmark_monthly_return(con) %>%
    dplyr::left_join(tbl_benchmark_info(con), by = "benchmark_info_id")
  if (return_tibble == TRUE) {
    dat <- dat %>% tibble::as_tibble()
  }
  return(dat)
}


get_account_bor_daily <- function(con = AZASRS_DATABASE_CONNECTION(), return_tibble = TRUE) {
  dat <- tbl_account_book_of_record_daily(con) %>%
    dplyr::left_join(get_account_info(con, return_tibble = FALSE), by = "account_info_id")
  if (return_tibble == TRUE) {
    dat <- dat %>% tibble::as_tibble()
  }
  return(dat)
}


get_account_bor_monthly <- function(con = AZASRS_DATABASE_CONNECTION(), return_tibble = TRUE) {
  dat <- tbl_account_book_of_record_monthly(con) %>%
    dplyr::left_join(get_account_info(con, return_tibble = FALSE), by = "account_info_id")
  if (return_tibble == TRUE) {
    dat <- dat %>% tibble::as_tibble()
  }
  return(dat)
}
