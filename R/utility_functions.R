#' standardise country names from 2 letter country iso
#'
#' @param country_iso character vector containing iso2c
#'
#' @return character vector of full country names in lower case with hyphen separator
#'
country_clean <- function(country_iso) {

  reqd_country <- unique(countrycode::countrycode(country_iso, "iso2c", "country.name"))

  av_country_file <- "wgb_countries.rds"
  src_dir <- system.file("extdata", package = "wgbDataScrapeR")
  src <- paste(src_dir, av_country_file, sep = "/")

  av_country <- readRDS(src)
  reqd_country <- tolower(reqd_country)
  av_country <- tolower(av_country)

  assertR::assert_present(av_country, reqd_country)

  reqd_country <- gsub(" ", "-", reqd_country)

  reqd_country
}

#' check that the supplied metric is supported
#'
#' @param metric character vector of metrics - options are 'rating', 'cds_5' and 'yield_' with a given maturity in years, e.g. 'yield_10'
#'
#' @return character vector of permissible metrics as a subset of supplied metrics
#'
check_metric <- function(metric) {

  yld_metric <- metric[grep("^yield_", metric)]

  if(length(yld_metric) > 0) {
    unav_metric <- setdiff(metric, c(yld_metric, "rating", "cds_5"))
  } else {
    unav_metric <- setdiff(metric, c("rating", "cds_5"))
  }

  if(length(unav_metric) > 0) {
    unav_str <- paste(unav_metric, collapse = "','")
    unav_str <- paste("('", unav_str, "')", sep = "")

    message(paste0("You've attempted to specify a metric/s that is not currently supported: ", unav_str,
                   " They will be dropped. Options are: ('rating', 'cds_5' and 'yield_' with a given maturity in years)"))

    metric <- setdiff(metric, unav_metric)
  }

  metric
}
