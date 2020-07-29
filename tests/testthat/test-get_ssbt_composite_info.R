test_that("get_ssbt_composite_info() matches test data", {
  ssbt_composite_info <- get_ssbt_composite_info() %>% tibble::as_tibble()
  expected_names <- c(
    "ssbt_composite_info_id", "ssbt_composite_id", "ssbt_composite_description",
    "ssbt_composite_short_description", "benchmark_info_id"
  )

  expect_equal(colnames(ssbt_composite_info), expected_names)
  expect_equal(as.character(lapply(ssbt_composite_info, class)), c("integer", "character", "character", "character", "integer"))

  expect_true(dplyr::count(ssbt_composite_info) %>% dplyr::pull(n) > 30)
})
