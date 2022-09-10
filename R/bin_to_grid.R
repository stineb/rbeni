#' Bin data to rectangular grid
#'
#' Bin data into a rectangular geographical grid by longitude and latitude.
#'
#' @param df A data frame containing columns
#' \code{lon} and \code{lat}
#' @param from_lon left edge of grid (degrees longitude E)
#' @param from_lat bottom edge of grid (degrees latitude N)
#' @param to_lon right edge of grid (degrees longitude E)
#' @param to_lat top edge of grid (degrees latitude N)
#' @param dlon grid spacing in longitude (degrees)
#' @param dlat grid spacing in latitude (degrees)
#' @param do_nest A logical specifying whether data frame should be nested
#' @param do_summarise A logical specifying whether data frame should be aggregated by a function
#' @return A data frame where all numeric columns are aggregated by mean (na.rm = TRUE)
#'
#' @export
#'
#' @examples
#'
bin_to_grid <- function(df, from_lon, from_lat, to_lon, to_lat, dlon, dlat, do_nest = FALSE, do_summarise = TRUE){

  lon_breaks <- seq(from = from_lon, to = to_lon, by = dlon)
  lat_breaks <- seq(from = from_lat, to = to_lat, by = dlat)

  df <- df %>%
    ungroup() %>%
    mutate(ilon = cut(lon,
                      breaks = lon_breaks),
           ilat = cut(lat,
                      breaks = lat_breaks)) %>%
    mutate(lon_lower = as.numeric( sub("\\((.+),.*", "\\1", ilon)),
           lon_upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", ilon) ),
           lat_lower = as.numeric( sub("\\((.+),.*", "\\1", ilat) ),
           lat_upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", ilat) )) %>%
    mutate(lon_mid = (lon_lower + lon_upper)/2,
           lat_mid = (lat_lower + lat_upper)/2) %>%

    ## create cell name to associate with climate input
    dplyr::select(-ilon, -ilat, -lon_lower, -lon_upper, -lat_lower, -lat_upper, -lon, -lat)

  if (do_nest){
    
    df_agg <- df %>%
      rename(lon = lon_mid, lat = lat_mid) %>% 
      group_by(lon, lat) %>%
      nest()
    
  } else if (do_summarise){
    
    df_agg <- df %>%
      group_by(lon_mid, lat_mid) %>%
      summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
      rename(lon = lon_mid, lat = lat_mid)
    
  }
  
  return(df_agg)

}
