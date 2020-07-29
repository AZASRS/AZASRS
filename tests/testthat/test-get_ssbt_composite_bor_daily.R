test_that("get_ssbt_composite_bor_daily() matches test data", {
  ssbt_composite_bor_daily <- get_ssbt_composite_bor_daily() %>% tibble::as_tibble()
  expected_names <- c(
    "ssbt_composite_info_id", "effective_date", "beginning_market_value",
    "ending_market_value", "net_cash_flow", "daily_return",
    "ssbt_composite_id", "ssbt_composite_description", "ssbt_composite_short_description",
    "benchmark_info_id"
  )

  expect_equal(colnames(ssbt_composite_bor_daily), expected_names)
  expect_equal(as.character(lapply(ssbt_composite_bor_daily, class)), c(
    "integer", "Date", "numeric", "numeric", "numeric", "numeric", "character", "character", "character",
    "integer"
  ))

  expect_true(dplyr::count(ssbt_composite_bor_daily) %>% dplyr::pull(n) > 6963)
})
