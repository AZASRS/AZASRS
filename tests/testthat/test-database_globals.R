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
  expect_equal(valdate, "2020-03-31")
})

test_that("Next quarter is one after valdate", {
  next_quarter <- get_next_quarter()
  expect_equal(next_quarter, "2020-06-30")
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



###START TESTS FOR ALL TBLS IN DATABASE_GLOBALS###
##Check:
##Column names are the same
##Column datatypes are the same
##there are at least x amount of rows
test_that("Check account_asset_class",{check_database_tbl_matches(tbl_account_asset_class,
                                        expected_names = c("account_asset_class_id",
                                                            "account_asset_class"),
                                                          types = c("integer", "character"),
                                                           greater_than_rows = 1)})


test_that("Check account_book_of_record_daily",{check_database_tbl_matches(tbl_account_book_of_record_daily,
                                              expected_names = c("account_info_id",
                                                                  "effective_date",
                                                                 "beginning_market_value",
                                                                 "ending_market_value",
                                                                 "net_cash_flow",
                                                                 "daily_return"),
                                                 types = c("integer", "Date",
                                                           "numeric", "numeric", "numeric",
                                                           "numeric"),
                                                  greater_than_rows = 0)})

test_that("Check account_book_of_record_daily",{check_database_tbl_matches(tbl_account_book_of_record_monthly,
                                                                           expected_names = c("account_info_id",
                                                                                              "effective_date",
                                                                                              "beginning_market_value",
                                                                                              "ending_market_value",
                                                                                              "net_cash_flow",
                                                                                              "monthly_return",
                                                                                              "fiscal_ytd_return"),
                                                                           types = c("integer", "Date",
                                                                                     "numeric", "numeric", "numeric",
                                                                                     "numeric", "numeric"),
                                                                           greater_than_rows = 0)})


test_that("Check account category",{check_database_tbl_matches(tbl_account_category,
                                                                  expected_names = c("account_category_id",
                                                                                     "account_category"),
                                                                  types = c("integer", "character"),
                                                                  greater_than_rows = 1)})


test_that("Check account info",{check_database_tbl_matches(tbl_account_info,
                                                               expected_names = c("account_info_id",
                                                                                  "account_id",
                                                                                  "account_asset_class_id",
                                                                                  "account_portfolio_id",
                                                                                  "account_sub_portfolio_id",
                                                                                  "account_category_id",
                                                                                  "account_sponsor_id",
                                                                                  "quarter_lagged",
                                                                                  "inception",
                                                                                  "defunding",
                                                                                  "exp_excess",
                                                                                  "exp_te"),
                                                               types = c("integer", "character", "integer",
                                                                         "integer", "integer", "integer",
                                                                         "integer", "logical", "Date",
                                                                         "Date", "numeric", "numeric"),
                                                               greater_than_rows = 0)})



test_that("Check account_info_benchmark_info",{check_database_tbl_matches(tbl_account_info_benchmark_info,
                                                                  expected_names = c("account_info_id",
                                                                                     "benchmark_info_id",
                                                                                     "benchmark_type_id"),
                                                                  types = c("integer", "integer", "integer"),
                                                                  greater_than_rows = 1)})


# test_that("Check account_info_pm_fund_info",{check_database_tbl_matches(tbl_account_info_pm_fund_info,
#                                                                           expected_names = c("account_info_id",
#                                                                                              "pm_fund_info_id"),
#                                                                           types = c("integer", "integer"),
#                                                                           greater_than_rows = 1)})



test_that("Check account_portfolio",{check_database_tbl_matches(tbl_account_portfolio,
                                                                        expected_names = c("account_portfolio_id",
                                                                                           "account_portfolio"),
                                                                        types = c("integer", "character"),
                                                                        greater_than_rows = 1)})



# test_that("Check account_sponsor",{check_database_tbl_matches(tbl_account_sponsor,
#                                                                 expected_names = c("account_sponsor_id",
#                                                                                    "account_sponsor"),
#                                                                 types = c("integer", "character"),
#                                                                 greater_than_rows = 1)})


test_that("Check account_sub_portfolio",{check_database_tbl_matches(tbl_account_sub_portfolio ,
                                                              expected_names = c("account_sub_portfolio_id",
                                                                                 "account_sub_portfolio"),
                                                              types = c("integer", "character"),
                                                              greater_than_rows = 1)})



test_that("Check benchmark_daily_index",{check_database_tbl_matches(tbl_benchmark_daily_index,
                                                                    expected_names = c("benchmark_info_id",
                                                                                       "effective_date",
                                                                                       "index_value"),
                                                                    types = c("integer", "Date", "numeric"),
                                                                    greater_than_rows = 1)})


# test_that("Check benchmark_daily_return",{check_database_tbl_matches(tbl_benchmark_daily_return,
#                                                                     expected_names = c("benchmark_info_id",
#                                                                                        "effective_date",
#                                                                                        "daily_return"),
#                                                                     types = c("integer", "Date", "numeric"),
#                                                                     greater_than_rows = 1)})


test_that("Check benchmark_info",{check_database_tbl_matches(tbl_benchmark_info,
                                                                     expected_names = c("benchmark_info_id",
                                                                                        "benchmark_id",
                                                                                        "benchmark_description",
                                                                                        "benchmark_symbol_name"),
                                                                     types = c("integer", "character",
                                                                               "character", "character"),
                                                                     greater_than_rows = 1)})


# test_that("Check benchmark_monthly_return",{check_database_tbl_matches(tbl_benchmark_monthly_return,
#                                                              expected_names = c("benchmark_info_id",
#                                                                                 "effective_date",
#                                                                                 "monthly_return",
#                                                                                 "fiscal_ytd_return"),
#                                                              types = c("integer", "Date",
#                                                                        "numeric", "numeric"),
#                                                              greater_than_rows = 1)})


test_that("Check benchmark_type",{check_database_tbl_matches(tbl_benchmark_type,
                                                                       expected_names = c("benchmark_type_id",
                                                                                          "benchmark_type"),
                                                                       types = c("integer", "character"),
                                                                       greater_than_rows = 1)})



test_that("Check constants",{check_database_tbl_matches(tbl_constants,
                                                             expected_names = c("value_date",
                                                                                "next_quarter"),
                                                             types = c("character", "character"),
                                                             greater_than_rows = 0)})



test_that("Check pm_fund_cash_flow_daily",{check_database_tbl_matches(tbl_pm_fund_cash_flow_daily,
                                                        expected_names = c("pm_fund_info_id",
                                                                           "effective_date",
                                                                           "cash_flow"),
                                                        types = c("integer", "Date",
                                                                  "numeric"),
                                                        greater_than_rows = 1)})

test_that("Check pm_fund_category",{check_database_tbl_matches(tbl_pm_fund_category,
                                                                      expected_names = c("pm_fund_category_id",
                                                                                         "pm_fund_category"),
                                                                      types = c("integer", "character"),
                                                                      greater_than_rows = 1)})


test_that("Check pm_fund_category_description",{check_database_tbl_matches(tbl_pm_fund_category_description,
                                                               expected_names = c("pm_fund_category_description_id",
                                                                                  "pm_fund_category_description"),
                                                               types = c("integer", "character"),
                                                               greater_than_rows = 1)})


test_that("Check pm_fund_city",{check_database_tbl_matches(tbl_pm_fund_city,
                                                                           expected_names = c("pm_fund_city_id",
                                                                                              "pm_fund_city"),
                                                                           types = c("integer", "character"),
                                                                           greater_than_rows = 1)})


test_that("Check pm_fund_info",{check_database_tbl_matches(tbl_pm_fund_info,
                                                           expected_names = c("pm_fund_info_id", "pm_fund_id",
                                                                              "pm_fund_description", "pm_fund_common_name",
                                                                              "pm_fund_portfolio_id", "pm_fund_category_id",
                                                                              "pm_fund_category_description_id","pm_fund_sponsor_id",
                                                                              "pm_fund_city_id", "pm_fund_sector_id",
                                                                              "vintage", "commit", "unfunded", "legacy",
                                                                              "specialist", "invest_end", "term_end",
                                                                              "extension", "ext_time", "ext_used",
                                                                              "fee_cat", "consultant", "adv_board",
                                                                              "obsvr", "fund_size_m", "closed"),
                                                           types = c("integer", "character", "character", "character",
                                                                     "integer", "integer", "integer", "integer", "integer",
                                                                     "integer", "integer", "integer", "integer", "character",
                                                                     "character", "Date", "Date", "numeric", "numeric",
                                                                     "numeric", "character", "character", "logical",
                                                                     "logical", "numeric", "character"),
                                                           greater_than_rows = 1)})


test_that("Check pm_fund_info_benchmark_info",{check_database_tbl_matches(tbl_pm_fund_info_benchmark_info,
                                                           expected_names = c("pm_fund_info_id",
                                                                              "benchmark_info_id",
                                                                              "benchmark_type_id"),
                                                           types = c("integer", "integer",
                                                                     "integer"),
                                                           greater_than_rows = 1)})


test_that("Check pm_fund_nav_daily",{check_database_tbl_matches(tbl_pm_fund_nav_daily,
                                                                          expected_names = c("pm_fund_info_id",
                                                                                             "effective_date",
                                                                                             "nav"),
                                                                          types = c("integer", "Date",
                                                                                    "numeric"),
                                                                          greater_than_rows = 1)})


test_that("Check pm_fund_portfolio",{check_database_tbl_matches(tbl_pm_fund_portfolio,
                                                                expected_names = c("pm_fund_portfolio_id",
                                                                                   "pm_fund_portfolio"),
                                                                types = c("integer", "character"),
                                                                greater_than_rows = 1)})


test_that("Check pm_fund_sector",{check_database_tbl_matches(tbl_pm_fund_sector,
                                                                expected_names = c("pm_fund_sector_id",
                                                                                   "pm_fund_sector"),
                                                                types = c("integer", "character"),
                                                                greater_than_rows = 1)})


test_that("Check pm_fund_sponsor",{check_database_tbl_matches(tbl_pm_fund_sponsor,
                                                             expected_names = c("pm_fund_sponsor_id",
                                                                                "pm_fund_sponsor"),
                                                             types = c("integer", "character"),
                                                             greater_than_rows = 1)})


test_that("Check ssbt_composite_book_of_record_daily",{check_database_tbl_matches(tbl_ssbt_composite_book_of_record_daily,
                                                              expected_names = c("ssbt_composite_info_id",
                                                                                 "effective_date",
                                                                                 "beginning_market_value",
                                                                                 "ending_market_value",
                                                                                 "net_cash_flow",
                                                                                 "daily_return"),
                                                              types = c("integer", "Date",
                                                                        "numeric", "numeric",
                                                                        "numeric", "numeric"),
                                                              greater_than_rows = 1)})


test_that("Check ssbt_composite_book_of_record_monthly",{check_database_tbl_matches(tbl_ssbt_composite_book_of_record_monthly,
                                                                                  expected_names = c("ssbt_composite_info_id",
                                                                                                     "effective_date",
                                                                                                     "beginning_market_value",
                                                                                                     "ending_market_value",
                                                                                                     "net_cash_flow",
                                                                                                     "monthly_return",
                                                                                                     "fiscal_ytd_return"),
                                                                                  types = c("integer", "Date",
                                                                                            "numeric", "numeric",
                                                                                            "numeric", "numeric",
                                                                                            "numeric"),
                                                                                  greater_than_rows = 1)})



test_that("Check ssbt_composite_info ",{check_database_tbl_matches(tbl_ssbt_composite_info,
                                                                                  expected_names = c("ssbt_composite_info_id",
                                                                                                     "ssbt_composite_id",
                                                                                                     "ssbt_composite_description",
                                                                                                     "ssbt_composite_short_description",
                                                                                                     "benchmark_info_id"),
                                                                                  types = c("integer", "character",
                                                                                            "character", "character",
                                                                                            "integer"),
                                                                                  greater_than_rows = 1)})


test_that("Check ssbt_composite_info_account_info",{check_database_tbl_matches(tbl_ssbt_composite_info_account_info,
                                                             expected_names = c("ssbt_composite_info_id",
                                                                                "account_info_id"),
                                                             types = c("integer", "integer"),
                                                             greater_than_rows = 1)})


# test_that("Check ssbt_composite_info_benchmark_info",{check_database_tbl_matches(tbl_ssbt_composite_info_benchmark_info,
#                                                                                expected_names = c("ssbt_composite_info_id",
#                                                                                                   "benchmark_info_id",
#                                                                                                   "benchmark_type_id"),
#                                                                                types = c("integer", "integer",
#                                                                                          "integer"),
#                                                                                greater_than_rows = 1)})



test_that("Check view_all_pm_fund_info",{check_database_tbl_matches(tbl_view_all_pm_fund_info,
                                                                                 expected_names = c("pm_fund_info_id",
                                                                                                    "pm_fund_id",
                                                                                                    "pm_fund_description",
                                                                                                    "pm_fund_common_name",
                                                                                                    "vintage", "commit", "unfunded",
                                                                                                    "legacy", "specialist",
                                                                                                    "invest_end", "term_end",
                                                                                                    "extension", "ext_time",
                                                                                                    "ext_used", "fee_cat",
                                                                                                    "consultant", "adv_board",
                                                                                                    "obsvr", "fund_size_m",
                                                                                                    "closed", "pm_fund_category",
                                                                                                    "pm_fund_category_description",
                                                                                                    "pm_fund_portfolio","pm_fund_sponsor",
                                                                                                    "pm_fund_city", "pm_fund_sector"),
                                                                                 types = c("integer", "character", "character",
                                                                                           "character", "integer", "integer",
                                                                                           "integer", "character", "character",
                                                                                           "Date", "Date", "numeric", "numeric",
                                                                                           "numeric", "character", "character",
                                                                                           "logical", "logical", "numeric",
                                                                                           "character", "character","character",
                                                                                           "character", "character", "character",
                                                                                           "character"),
                                                                                 greater_than_rows = 1)})
####ENDING TESTS FOR ALL TBLS IN DATABASE GLOBALS####


