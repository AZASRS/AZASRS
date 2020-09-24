test_that("get_account_futures returns expected results", {
  dat <- get_account_futures()
  expected_names <- c(
    "account_info_id", "period_end_date", "maturity_date", "security_name_full",
    "security_name", "base_current_notional_value", "base_margin_variation", "account_id")
  expect_equal(colnames(dat), expected_names)
  expect_equal(as.character(lapply(dat, class)), c("integer", "Date", "Date", "character", "character", "numeric", "numeric", "character"))

  expect_true(dplyr::count(dat) %>% dplyr::pull(n) >= 1)
})
