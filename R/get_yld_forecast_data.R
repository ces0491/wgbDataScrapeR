#' get forecasts for yield data
#'
#' @param pjs_session phantom.js session
#' @param metric string indicating the yield metric
#'
#' @return tbl_df with columns forecast_date of class \code{Date}, retrieve_date of class \code{Date}, metric of class \code{character} and value of class \code{numeric}
#'
get_yld_forecast_data <- function(pjs_session, metric) {

  raw_forecast_data <- webScrapeR::scrape_table(pjs_session, xpath = '//*[@id="post-91"]/div/div/div[2]/div[2]/table')

  names(raw_forecast_data) <- raw_forecast_data[2,]
  forc_data <- raw_forecast_data[3, -1]

  forcast_tbl <- tibble::tibble(forc_data) %>%
    tidyr::gather(., date, value) %>%
    dplyr::mutate(date = paste0(date, " 1")) %>%
    dplyr::mutate(forecast_date = as.Date(date, format = "%b %Y %d")) %>%
    dplyr::mutate(forecast_date = dateR::get_eom_dates(forecast_date)) %>%
    dplyr::mutate(retrieve_date = Sys.Date()) %>%
    dplyr::mutate(value = stringr::word(value, 1, sep = "%")) %>%
    dplyr::mutate(value = as.numeric(value)) %>%
    dplyr::mutate(metric = metric) %>%
    dplyr::select(forecast_date, retrieve_date, metric, value)

  forcast_tbl
}
