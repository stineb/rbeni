#' Get days since
#'
#' Constructs a vector of days since a given date
#'
#' @param fromyear An integer specifying the start year
#' @param len An integer specifying the length of the returned vector
#' @param startmoy An integer specifying the month of year (between 1 and 12)
#' of the first entry of the returned vector. Defaults to 1.
#' @param startdoy An integer specifying the day of year (between 1 and 365)
#' of the first entry of the returned vector. Defaults to 1.
#' @param by A character string specifying the "spacing" between dates.
#' Defaults to \code{"months"}.
#' @return A vector of integers
#' @export
#'
get_days_since <- function( fromyear, len, startmoy=1, startdoy=1, by="months", origin=ymd("2001-01-01") ){
	## returns a vector of integers representing days since 1 Jan 1970
  ## 'freq' is a character, compatible with 'by' from the seq() function, e.g. "months"
  startdate <- lubridate::ymd( paste0( as.character(fromyear), "-", sprintf( "%02d", startmoy),"-01" ) ) + days(startdoy - 1)
  dates <- seq( from=startdate, length.out = len, by=by )
  origin_days_since_1970_01_01 <- unclass(origin)
  writedates <- floor(unclass(dates) - origin_days_since_1970_01_01)
  return(writedates)
}
