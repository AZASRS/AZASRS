test_that("get_benchmark_fund_relationship() matches test data", {
  benchmark_fund_relationship <- get_benchmark_fund_relationship() %>% tibble::as_tibble()
  expected_names <- c("pm_fund_info_id", "benchmark_info_id", "benchmark_type")

  expect_equal(colnames(benchmark_fund_relationship), expected_names)
  expect_equal(as.character(lapply(benchmark_fund_relationship, class)), c("integer", "integer", "character"))
  expect_true(dplyr::count(benchmark_fund_relationship) %>% dplyr::pull(n) > 200)
})
