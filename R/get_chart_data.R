#' get data from WGB charts
#'
#' needed for cds and yield data
#'
#' @param pjs_session remote driver object
#' @param metric string indicating the metric of interest
#'
#' @return df containing date, metric and value
#'
get_wgb_chart_data <- function(pjs_session, metric) {

  scraped_chart_df <- webScrapeR::scrape_chart_ts(pjs_session)

  data_df <- scraped_chart_df %>%
    dplyr::mutate(metric = !!metric) %>%
    dplyr::rename(value = y) %>%
    dplyr::select(date, metric, value)

  data_df

}
