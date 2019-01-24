init_dates_dataframe <- function( yrstart, yrend, startmoy=1, startdoy=1, freq="days", endmoy=12, enddom=31, noleap=FALSE ){

  require(dplyr)
  require(lubridate)

  startdate <- ymd( paste0( as.character(yrstart), "-", sprintf( "%02d", startmoy), "-01" ) ) + days( startdoy - 1 )
  enddate   <- ymd( paste0( as.character(yrend  ), "-", sprintf( "%02d", endmoy  ), "-", sprintf( "%02d", enddom  ) ) )    

  ddf <-  tibble( date=seq( from = startdate, to = enddate, by = freq ) ) %>% 
          mutate( ndayyear = ifelse( (year(date) %% 4) == 0, 366, 365  ) ) %>%
          mutate( year_dec = year(date) + (yday(date) - 1) / ndayyear ) %>% 
          select( -ndayyear )

  if (noleap) ddf <- ddf %>% filter( !( month(date)==2 & mday(date)==29 ) )

  return( ddf )

}