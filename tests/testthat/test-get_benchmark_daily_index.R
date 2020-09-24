##Check:
##Column names are the same
##Column datatypes are the same
##there are at least x amount of rows
test_that("get_benchmark_daily_index() matches test data", {
  benchmark_daily_index <- get_benchmark_daily_index() %>% tibble::as_tibble()
  expected_names <- c("benchmark_info_id", "effective_date", "index_value", "benchmark_id")

  expect_equal(colnames(benchmark_daily_index), expected_names)
  expect_true(dplyr::count(benchmark_daily_index) %>% dplyr::pull(n) > 6963)
})

#Check:
#when all_bench_types = TRUE check number of rows is > bench_type = "PVT"

test_that("get_benchmark_daily_index() all bench types greater than just PVT", {
  ALL_bench_daily_index <- get_benchmark_daily_index(all_bench_types = TRUE) %>% tibble::as_tibble()
  PVT_daily_index <- get_benchmark_daily_index(benchmark_type = "PVT") %>% tibble::as_tibble()

  n_all <- dplyr::count(ALL_bench_daily_index) %>% dplyr::pull(n)
  n_pvt <- dplyr::count(PVT_daily_index) %>% dplyr::pull(n)

  expect_true(n_all > n_pvt)
})
