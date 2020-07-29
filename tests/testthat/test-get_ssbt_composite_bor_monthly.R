test_that("get_ssbt_composite_bor_monthly() matches test data", {
  ssbt_composite_bor_monthly <- get_ssbt_composite_bor_monthly() %>% tibble::as_tibble()
  expected_names <- c(
    "ssbt_composite_info_id", "effective_date", "beginning_market_value",
    "ending_market_value", "net_cash_flow", "monthly_return",
    "fiscal_ytd_return", "ssbt_composite_id", "ssbt_composite_description",
    "ssbt_composite_short_description", "benchmark_info_id"
  )

  expect_equal(colnames(ssbt_composite_bor_monthly), expected_names)
  expect_equal(as.character(lapply(ssbt_composite_bor_monthly, class)), c(
    "integer", "Date", "numeric", "numeric", "numeric", "numeric", "numeric", "character", "character",
    "character", "integer"
  ))

  expect_true(dplyr::count(ssbt_composite_bor_monthly) %>% dplyr::pull(n) > 60)
})
