% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_et.R
\name{convert_et}
\alias{convert_et}
\title{Convert evapotranspiration to mm}
\usage{
convert_et(et_e, tc, elv = 0, return_df = FALSE)
}
\arguments{
\item{et_e}{A numeric value or vector specifying vapotranspiration in energy units (W m-2)}

\item{tc}{A numeric value or vector specifying temperature in degrees Celsius}

\item{elv}{A numeric value or vector specifying elevation above sea level (m). Defaults to 0.}

\item{return_df}{A logical specifying whether a data frame (single column for ET) should be returned.}
}
\value{
A numeric value or vector, or, if \code{return_df = TRUE}, a data frame (tibble) with ET
values in mass units (mm).
}
\description{
Converts evapotranspiration (ET) measurements given in energy units (here W m-2)
to mass units (here mm water, corresponding to kg m-2). Adopted from SPLASH (Davis et al., 2017 GMD).
}
