% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grid_to_df.R
\name{grid_to_df}
\alias{grid_to_df}
\title{NetCDF to data frame}
\usage{
grid_to_df(arr, grid = NA, dropna = TRUE, timedim = NA)
}
\arguments{
\item{arr}{A 2D or 3D array}

\item{grid}{A character string specifying the grid}

\item{dropna}{A boolean specifying wether rows where the data variable is NA should
be dropped. Recommended to be set to \code{TRUE} for large global arrays where much
of the grid is NA (e.g., over ocean).}

\item{timedim}{An integer specifying which dimension represents time for 3D arrays.}
}
\value{
A data frame with columns \code{lon} and \code{lat}.
}
\description{
Reads a NetCDF file into a data frame with longitude and latitude
information
}
