# TODO
# add function to get yield curve at a given date
# consistently handle data where weekends aren't available

#' Get data from worldgovernmentbonds.com as a list
#'
#' @param pjs_session phantom.js session
#' @param url_tbl object of class tbl_df containing columns url and metric
#'
#' @return a list of data.frames
#' @export
#'
get_wgb_data_list <- function(pjs_session, url_tbl) {

  assertR::assert_present(names(url_tbl), c("metric", "url"))

  urls <- url_tbl$url
  wgb_data_list <- list()

  for (url in urls) {

    u <- which(urls == url)
    progress <- round(u/length(urls), 2) * 100
    print(glue::glue("Attempting to retrieve {url_tbl$metric[[u]]} data for {url_tbl$country[[u]]} from World Government Bonds.com"))

    if (url_tbl$metric[[u]] == "rating") {
      pjs_session$go(url_tbl$url[[u]])
      wgb_data <- get_wgb_rating_data(pjs_session, agency = "S&P") # for now only S&P support
    }

    if (url_tbl$metric[[u]] == "cds_5") {
      pjs_session$go(url_tbl$url[[u]])
      wgb_data <- get_wgb_chart_data(pjs_session, metric = "cds_5") # wgb.com only has 5y cds
    }

    if (stringr::str_detect(url_tbl$metric[[u]], "yield") & stringr::str_detect(url_tbl$metric[[u]], "forecast", negate = TRUE)) {
      yld_metric <- stringr::str_subset(url_tbl$metric[[u]], "yield")
      yld_no_forc <- yld_metric[stringr::str_detect(yld_metric, "forecast", negate = TRUE)] # yield data not containing forecast
      pjs_session$go(url_tbl$url[[u]])
      wgb_data <- get_wgb_chart_data(pjs_session, metric = yld_no_forc)
    }

    if (stringr::str_detect(url_tbl$metric[[u]], "forecast")) {
      forc_metric <- url_tbl$metric[[u]]
      pjs_session$go(url_tbl$url[[u]])
      wgb_data <- get_yld_forecast_data(pjs_session, metric = forc_metric)

      wgb_data <- wgb_data %>%
        dplyr::rename(date = forecast_date) %>%
        dplyr::select(-retrieve_date)
    }

    print(glue::glue("{progress}% complete"))

    wgb_data_list[[url]] <- wgb_data

  }

  wgb_data_list

}

#' Get data from worldgovernmentbonds.com
#'
#' @param country_iso character vector containing 2 letter country iso codes
#' @param metric string indicating whether you'd like to return sovereign rating, cds or yield data. sovereign rating defaults to S&P
#' @param start_date start date
#' @param end_date end date
#' @param frequency string indicating the periodicity of data to retrieve - one of daily, weekly, monthly, quarterly or annual
#' @param include_forecast logical indicating whether forecasts should be included for the yield metrics - default to FALSE
#'
#' @return tbl_df with columns country iso, date, metric and value
#'
#' @importFrom magrittr %>%
#' @export
#'
get_wgb_data <- function(country_iso,
                         metric,
                         start_date,
                         end_date,
                         frequency = c("daily", "weekly", "monthly", "quarterly", "annual"),
                         include_forecast = FALSE) {

  country <- country_clean(country_iso)

  metric <- check_metric(metric)

  if("rating" %in% metric & length(metric) > 1) {
    # wgb_data <- wgb_data_list

    stop("You're attempting to retrieve a tibble of ratings data which is of class character and other data of class numeric. These data
         will not combine in the value output column. Please rather use get_wgb_data_list to return data with different value classes")
  }

  url_tbl <- build_wgb_url(country, metric, include_forecast)

  pjs_conn <- webScrapeR::connect_session(url = "http://www.worldgovernmentbonds.com")
  pjs_session <- pjs_conn$session

  wgb_data_list <- get_wgb_data_list(pjs_session, url_tbl)

  pjs_conn$pjs_process$kill()

  wgb_data_tbl <- wgb_data_list %>%
    tibble::enframe(name = "url", value = "scraped_data") %>%
    dplyr::left_join(url_tbl, by = "url") %>%
    dplyr::select(-metric)

  assertR::assert_present(names(wgb_data_tbl), c("country", "url", "scraped_data"))

  if (include_forecast) {
    end_date <- dateR::get_eom_dates(as.Date(end_date) + 730)
  }

  wgb_data <- wgb_data_tbl %>%
    tidyr::unnest(scraped_data) %>%
    dplyr::mutate(country_iso = countrycode::countrycode(country, "country.name", "iso2c")) %>%
    dplyr::group_by(country, metric) %>%
    dplyr::filter(date >= start_date,
                  date <= end_date) %>%
    dplyr::ungroup() %>%
    dplyr::select(country_iso, date, metric, value) %>%
    dplyr::distinct() %>%
    dplyr::arrange(country_iso, metric, date) %>%
    dateR::to_period(., period = frequency)

  wgb_data

}


