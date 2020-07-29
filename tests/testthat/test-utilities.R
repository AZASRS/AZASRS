test_that("is_not_rollup detects rollup", {
  expect_true(test_is_not_rollup(pm_fund_id))
  expect_false(test_is_not_rollup(pm_fund_portfolio, pm_fund_category))
})

test_that("expect_previous_year_qtr matches", {
  prev_date <- calc_previous_year_qtr(end_date = "2019-12-31", years = 1, qtrs = 1)
  expect_equal(prev_date, lubridate::as_date("2018-09-30"))
})

test_that("calc_add_qtrs adds and subracts quarters", {
  # qtr start dates
  expect_equal(calc_add_qtrs("2019-12-31", 1), lubridate::as_date("2020-03-31"))
  expect_equal(calc_add_qtrs("2019-12-31", -2), lubridate::as_date("2019-06-30"))

  # non-qtr start dates
  # zero should round up
  expect_equal(calc_add_qtrs("2019-12-17", 0), lubridate::as_date("2019-12-31"))
  expect_equal(calc_add_qtrs("2019-07-01", 0), lubridate::as_date("2019-09-30"))

  # minus 1 rounds down
  expect_equal(calc_add_qtrs("2019-12-17", -1), lubridate::as_date("2019-09-30"))

  expect_equal(calc_add_qtrs("2020-02-20", -2), lubridate::as_date("2019-09-30"))
})

test_that("filled_list_of_dates matches", {
  df <- filled_list_of_dates(start_date = "2018-12-31", end_date = "2020-03-31")
  my_dates <- df$effective_date
  expect_equal(my_dates, structure(c(17896, 17986, 18077, 18169, 18261, 18352), class = "Date"))
})

test_that("calc_fytd_metadata matches", {
  date_data <- calc_fytd_metadata("2018-09-30")
  expect_equal(as.character(date_data$start_date), "2018-06-30")
  expect_equal(date_data$n_qtrs, 1)
})

test_that("calc_cytd_metadata matches", {
  date_data <- calc_cytd_metadata("2018-09-30")
  expect_equal(as.character(date_data$start_date), "2017-12-31")
  expect_equal(date_data$n_qtrs, 3)
})

test_that("default_benchmark_lookup matches", {
  from_func <- default_benchmark_lookup
  from_tib <- tibble::tibble(
    pm_fund_portfolio = c("Credit", "PE", "RE"),
    benchmark_id = c("ODCE", "R2K-ACWI", "LSTA+250")
  )
  expect_equal(from_func, from_tib)
})
