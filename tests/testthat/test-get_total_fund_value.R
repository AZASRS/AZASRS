test_that("get_total_fund_value() is numeric and greater than 30billion for certain date", {
  value_date <- "03-28-2019"
  total_fund_value <- get_total_fund_value(value_date)

  expect_equal(as.character(lapply(total_fund_value, class)), "numeric")
  expect_true(total_fund_value > 30000000000)
})
