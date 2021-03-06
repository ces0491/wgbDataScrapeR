% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_wgb_url.R
\name{build_yld_url}
\alias{build_yld_url}
\title{Create the URLs for government bond yield data pages}
\usage{
build_yld_url(country, yld_metric, include_forecast)
}
\arguments{
\item{country}{string indicating the name of a country}

\item{yld_metric}{string indicating the yield metric name}

\item{include_forecast}{logical indicating whether yield forecasts are also required}
}
\value{
tbl_df with columns metric and url both with class character
}
\description{
Create the URLs for government bond yield data pages
}
