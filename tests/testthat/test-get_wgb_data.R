testthat::test_that("get yield data from wgb.com works as expected", {

  test_yield <- get_wgb_data(country_iso = c("ZA"),
                             metric = c("yield_10"),
                             start_date = "2020-08-15",
                             end_date = "2020-08-25",
                             frequency = "daily",
                             include_forecast = FALSE)

  expected_yield <- tibble::tibble(country_iso = "ZA",
                                   date = seq(as.Date("2020-08-15"), as.Date("2020-08-25"), by = "day"),
                                   metric = "yield_10",
                                   value = c(9.170, 9.170, 9.270, 9.290, 9.295, 9.290, 9.265, 9.265, 9.265, 9.315, 9.290))

  # forecasts will change over time so we omit this test

  testthat::expect_equal(test_yield, expected_yield)

})

testthat::test_that("get rating data from wgb.com works as expected", {

  test_rating <- get_wgb_data(country_iso = c("US"),
                              metric = c("rating"),
                              start_date = "2020-01-01",
                              end_date = "2020-07-31",
                              frequency = "monthly",
                              include_forecast = FALSE)

  dt_month <- dateR::get_eom_dates(seq(as.Date("2020-01-01"), as.Date("2020-07-31"), by = "month"))
  expected_rating <- data.frame(country_iso = "US",
                                date = dt_month,
                                metric = "rating",
                                value = rep("AA+", 7))

  testthat::expect_equal(test_rating, expected_rating)

})

testthat::test_that("get cds data from wgb.com works as expected", {

  test_cds <- get_wgb_data(country_iso = c("GB"),
                           metric = c("cds_5"),
                           start_date = "2020-08-15",
                           end_date = "2020-08-25",
                           frequency = "daily",
                           include_forecast = FALSE)

  expected_cds <- tibble::tibble(country_iso = "GB",
                                 date = seq(as.Date("2020-08-15"), as.Date("2020-08-25"), by = "day"),
                                 metric = "cds_5",
                                 value = c(22.6945, 22.6945, 22.5504, 23.4134, 22.6497, 21.4602, 22.3471, 22.0403, 22.0403, 22.9317, 19.8470))

  testthat::expect_equal(test_cds, expected_cds)

})

testthat::test_that("get data for vector of countries and metrics from wgb.com works as expected", {

  test_multi <- get_wgb_data(country_iso = c("US", "ZA"),
                             metric = c("yield_5", "cds_5"),
                             start_date = "2020-08-20",
                             end_date = "2020-08-25",
                             frequency = "daily",
                             include_forecast = FALSE)

  expected_multi <- tibble::tibble(country_iso = c(rep("US", 12), rep("ZA", 10)),
                                   date = c(rep(seq(as.Date("2020-08-20"), as.Date("2020-08-25"), by = "day"), 2),
                                            as.Date(c("2020-08-20", "2020-08-21", "2020-08-24", "2020-08-25")),
                                            seq(as.Date("2020-08-20"), as.Date("2020-08-25"), by = "day")),
                                   metric = c(rep("cds_5", 6), rep("yield_5", 6), rep("cds_5", 4), rep("yield_5", 6)),
                                   value = c(18.5, 18.5, 18.5, 18.5, 18.5, 18.5,
                                             0.272, 0.268, 0.268, 0.268, 0.282, 0.296,
                                             295.03, 292.96, 283.16, 287.35,
                                             7.435, 7.37, 7.37, 7.37, 7.4, 7.385))

  testthat::expect_equal(test_multi, expected_multi)

})
