##Check:
##Column names are the same
##Column datatypes are the same
##there are at least x amount of rows
test_that("get_benchmark_fund_relationship() matches test data", {
  benchmark_fund_relationship <- get_benchmark_fund_relationship() %>% tibble::as_tibble()
  expected_names <- c("pm_fund_info_id", "benchmark_info_id", "benchmark_type")

  expect_equal(colnames(benchmark_fund_relationship), expected_names)
  expect_equal(as.character(lapply(benchmark_fund_relationship, class)), c("integer", "integer", "character"))
  expect_true(dplyr::count(benchmark_fund_relationship) %>% dplyr::pull(n) > 200)
})


#Check:
#when get_all_benchmark_types = TRUE check number of rows is > bench_type = "PVT"

test_that("get_benchmark_fund_relationship() all bench types greater than just PVT", {
  ALL_fund_relationship <- get_benchmark_fund_relationship(get_all_benchmark_types = TRUE) %>%
    tibble::as_tibble()

  PVT_fund_relationship <- get_benchmark_fund_relationship(bench_type = "PVT") %>%
    tibble::as_tibble()

 n_all <- dplyr::count(ALL_fund_relationship) %>% dplyr::pull(n)

 n_pvt <- dplyr::count(PVT_fund_relationship) %>% dplyr::pull(n)

 expect_true(n_all > n_pvt)

})
