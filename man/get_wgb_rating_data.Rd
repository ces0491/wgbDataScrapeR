% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_rating_data.R
\name{get_wgb_rating_data}
\alias{get_wgb_rating_data}
\title{get sovereign ratings data from WGB}
\usage{
get_wgb_rating_data(pjs_session, agency)
}
\arguments{
\item{pjs_session}{phantom.js session}

\item{agency}{string indicating the ratings agency}
}
\value{
data.frame containing columns date of class \code{Date}, metric of class \code{character} and value of class \code{character}
}
\description{
get sovereign ratings data from WGB
}
