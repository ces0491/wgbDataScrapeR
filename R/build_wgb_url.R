#' Create the URLs for government bond yield data pages
#'
#' @param country string indicating the name of a country
#' @param yld_metric string indicating the yield metric name
#' @param include_forecast logical indicating whether yield forecasts are also required
#'
#' @return tbl_df with columns metric and url both with class character
#'
build_yld_url <- function(country, yld_metric, include_forecast) {

  assertR::assert_true(all(stringr::str_detect(yld_metric, "yield")), "logic error - non yield metric found")

  yield_url_list <- list()

  for (ym in yld_metric) {

    yrs <- stringr::str_extract(ym, '\\d+')
    maturity <- glue::glue("{yrs}-years")

    yield_url <- glue::glue("http://www.worldgovernmentbonds.com/bond-historical-data/{country}/{maturity}")

    yield_url_list[[ym]] <- yield_url

    if (include_forecast) {
      forc_url <- glue::glue("http://www.worldgovernmentbonds.com/bond-forecast/{country}/{maturity}")
      yld_forc_nm <- glue::glue("{ym}_forecast")
      yield_url_list[[yld_forc_nm]] <- forc_url
    }

  }

  yield_url_tbl <- tibble::enframe(yield_url_list, name = "metric", value = "url") %>%
    tidyr::unnest(url)

  yield_url_tbl

}

#' Create the URLs for the pages containing the data on worldgovernmentbonds.com
#'
#' @param country character vector with the names of countries
#' @param metric character vector with the names of required metrics
#' @param include_forecast logical
#'
#' @return a tbl_df with columns country, metric and url as character classes
#' @export
#'
build_wgb_url <- function(country, metric, include_forecast) {

  if (include_forecast) {
    yld_metric <- stringr::str_subset(metric, "yield")
    assertR::assert_true(length(yld_metric) > 0, "attempting to retrieve yield forecast without specifying a yield metric")

    yld_forc_nm <- glue::glue("{yld_metric}_forecast")
    reqd_metric <- c(metric, yld_forc_nm)
  } else {
    reqd_metric <- metric
  }

  country_list <- list()

  for (cntry in country) {

    rating_tbl <- tibble::tibble(metric = "rating", url = glue::glue("http://www.worldgovernmentbonds.com/credit-rating/{cntry}"))
    cds_tbl <- tibble::tibble(metric = "cds_5", url = glue::glue("http://www.worldgovernmentbonds.com/cds-historical-data/{cntry}/5-years")) # only 5y cds available on wgb.com

    if (any(stringr::str_detect(metric, "yield"))) {
      yld_metric <- stringr::str_subset(metric, "yield")
      yld_tbl <- build_yld_url(cntry, yld_metric, include_forecast)
      url_tbl <- dplyr::bind_rows(rating_tbl, cds_tbl, yld_tbl)
    } else {
      url_tbl <- url_tbl <- dplyr::bind_rows(rating_tbl, cds_tbl)
    }

    country_list[[cntry]] <- url_tbl

  }

  wgb_url_tbl <- country_list %>%
    tibble::enframe(name = "country", value = "url") %>%
    tidyr::unnest(url) %>%
    dplyr::filter(metric %in% reqd_metric)

  wgb_url_tbl
}
