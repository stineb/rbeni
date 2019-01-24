get_days_since <- function( fromyear, len, startmoy=1, startdoy=1, freq="months" ){
	## returns a vector of integers representing days since 1 Jan 1970
  ## 'freq' is a character, compatible with 'by' from the seq() function, e.g. "months"
  startdate <- as.POSIXct( as.Date( paste( as.character(fromyear), "-", sprintf( "%02d", startmoy),"-01", sep="" ) ) + startdoy - 1 )
  dates <- seq( from=startdate, length.out = len, by=freq )
  writedates <- floor(unclass(dates)/86400)
  return(writedates)
}