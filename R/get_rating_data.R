#' get sovereign ratings data from WGB
#'
#' @param pjs_session phantom.js session
#' @param agency string indicating the ratings agency
#'
#' @return data.frame containing columns date of class \code{Date}, metric of class \code{character} and value of class \code{character}
#'
get_wgb_rating_data <- function(pjs_session, agency) {

  #get table and extract data
  raw_ratings_data <- webScrapeR::scrape_table(pjs_session, xpath = '//*[@id="post-100"]/div/div/div[2]/div[15]/table')

  ratings_df <- raw_ratings_data %>%
    dplyr::mutate(Date = as.Date(Date, "%d %b %Y")) %>%
    tidyr::gather(agency, rating, -Date) %>%
    dplyr::mutate(rating = ifelse(rating == "", NA, rating))

  date_seq <- data.frame(Date = seq(min(ratings_df$Date), max(ratings_df$Date), by = "day"))

  ratings_tbl <- ratings_df %>%
    dplyr::filter(agency %in% !!agency) %>%
    dplyr::right_join(date_seq, by = "Date") %>%
    dplyr::arrange(Date) %>%
    tidyr::fill(rating, .direction = "down") %>%
    tidyr::fill(rating, .direction = "up") %>% # we can fill up because ratings are only displayed at dates where there has been a change.
    dplyr::mutate(metric = "rating") %>%
    dplyr::rename(date = Date) %>%
    dplyr::rename(value = rating) %>%
    dplyr::select(date, metric, value)

  ratings_tbl
}
