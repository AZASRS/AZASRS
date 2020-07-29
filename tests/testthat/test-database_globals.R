# Database globals return proper data structures

test_that("Database connection", {
  con <- AZASRS_DATABASE_CONNECTION()
  expect_equal(con@info$dbms.name, "Microsoft SQL Server")
  AZASRS_DATABASE_DISCONNECT(con)
})

test_that("Database disconnect", {
  con <- AZASRS_DATABASE_CONNECTION()
  expect_equal(con@info$dbms.name, "Microsoft SQL Server")
  dis_con <- AZASRS_DATABASE_DISCONNECT(con)
  expect_true(dis_con)
})

test_that("Proper value date (valdate)", {
  valdate <- get_value_date()
  expect_equal(valdate, "2019-12-31")
})

test_that("Next quarter is one after valdate", {
  next_quarter <- get_next_quarter()
  expect_equal(next_quarter, "2020-03-31")
})

test_that("PM Fund Info columns have not changed", {
  tbl_pm_fund_info <- tbl_pm_fund_info() %>% tibble::as_tibble()
  colnames_tbl_pm_fund_info <- colnames(tbl_pm_fund_info)
  expected_colnames_tbl_pm_fund_info <- c(
    "pm_fund_info_id", "pm_fund_id", "pm_fund_description", "pm_fund_common_name",
    "pm_fund_portfolio_id", "pm_fund_category_id", "pm_fund_category_description_id",
    "pm_fund_sponsor_id", "pm_fund_city_id", "pm_fund_sector_id",
    "vintage", "commit", "unfunded", "legacy", "specialist", "invest_end",
    "term_end", "extension", "ext_time", "ext_used", "fee_cat", "consultant",
    "adv_board", "obsvr", "fund_size_m", "closed"
  )
  expect_equal(colnames_tbl_pm_fund_info, expected_colnames_tbl_pm_fund_info)
})
