% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/walk_to_pt_and_combine_scen.R
\name{walk_to_pt_and_combine_scen}
\alias{walk_to_pt_and_combine_scen}
\title{Add walk to PT}
\usage{
walk_to_pt_and_combine_scen(SYNTHETIC_TRIPS)
}
\arguments{
\item{trip_set}{list of data frames, trips from all scenarios}
}
\value{
data frame, all trips from all scenarios
}
\description{
Adds a short walk stage to any PT trip if required.
Combines list of scenarios into one data frame
}
