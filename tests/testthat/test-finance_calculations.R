context("test-finance_calculations")

test_that("calc_irr() returns expected IRR", {
  # "cash_flow" is modified version for IRR
  cash_flow <- c(-100, 50, 10, -200, 250)
  dates <- lubridate::as_date(c("2018-12-31", "2019-01-03", "2019-02-09", "2019-02-28", "2019-03-31"))
  irr <- calc_irr(cash_flow, dates)
  expect_equal(irr, 0.08828409, tolerance = 1e-6)
})


test_that("calc_tvpi() returns expected TVPI", {
  contributions <- c(-100, -50, -10, -200, -50)
  distributions <- c(20, 40, 20, 30, 40)
  nav <- c(300, 0, 0, 0, 600)
  tvpi <- calc_tvpi(distributions, contributions, nav)
  expect_equal(tvpi, 1050 / 410, tolerance = 1e-6)
})


test_that("calc_dpi() returns expected DPI", {
  contributions <- c(-100, -50, -10, -200, -50)
  distributions <- c(20, 40, 20, 30, 40)
  dpi <- calc_dpi(distributions, contributions)
  expect_equal(dpi, 0.3658537, tolerance = 1e-6)
})


test_that("calc_appreciation() returns expected appreciation", {
  cash_flow <- c(-100, 10, 20, 10, 160)
  nav <- c(-500, 0, 0, 0, 0, 0, 700)
  appreciation <- calc_appreciation(cash_flow, nav)
  appreciation_expected <- 300
  expect_equal(appreciation, appreciation_expected, tolerance = 1e-6)
})


test_that("calc_pme() returns expected PME", {
  contributions <- c(-100, -50, -10, -200, -50)
  distributions <- c(20, 40, 20, 30, 40)
  nav <- c(300, 0, 0, 0, 600)
  fv_index_factors <- c(1.14, 1.11, 1.08, 1.04, 1)
  pme <- calc_pme(distributions, contributions, nav, fv_index_factors)
  expect_equal(pme, 2.418435, tolerance = 1e-6)
})


test_that("calc_dva() returns expected DVA", {
  cash_flow <- c(-100, -50, 100, 150, -50)
  fv_index_factors <- c(1.14, 1.11, 1.08, 1.04, 1)
  dva <- calc_dva(cash_flow, fv_index_factors)
  expect_equal(dva, 44.5, tolerance = 1e-6)
})
