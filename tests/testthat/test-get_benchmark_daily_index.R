test_that("get_benchmark_daily_index() matches test data", {
  benchmark_daily_index <- get_benchmark_daily_index() %>% tibble::as_tibble()
  expected_names <- c("benchmark_info_id", "effective_date", "index_value", "benchmark_id")

  expect_equal(colnames(benchmark_daily_index), expected_names)
  expect_true(dplyr::count(benchmark_daily_index) %>% dplyr::pull(n) > 6963)
})
