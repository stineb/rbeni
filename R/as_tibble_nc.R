#' Convert NetCDF contents to a data frame.
#'
#' Converts the output of \code{"read_nc_onefile()"} into a data frame (tibble)
#'
#' @param out An object returned by the function \link{read_nc_onefile}.
#' @return A tidy data frame with a row for each gridcell. Contains columns
#' \code{"lon"} (longitude), \code{"lat"} (latitude), and \code{"data"}
#' (containing all time steps' data for each gridcell.)
#' @export
#'
as_tibble_nc <- function(out){

  require(dplyr)
  require(tidyr)

  # filn <- "/alphadata01/bstocker/sofun/trunk/output_nc/DE-Spw.d.netrad.nc"
  #
  # ## read nc file
  # out <- read_nc_onefile(filn)

  ## expand lon-lat-time grid -> produces flat data frame
  df <- expand.grid(out$lon, out$lat, out$time) %>%
    as.matrix() %>%
    as_tibble() %>%
    setNames(c("lon", "lat", "time"))

  varnams <- out[["varnams"]]

  ## get all data as columns in a data frame
  df_data <- purrr::map_dfc(as.list(varnams), ~as_tibble_byvar(., out)) %>%
    setNames(varnams)

  ## combine lon, lat, time, and data columns
  df <- df %>%
    bind_cols(df_data)

  ## nest time
  df <- df %>%
    dplyr::group_by(lon, lat) %>%
    tidyr::nest()


  return(df)
}

as_tibble_byvar <- function(varnam, out){
  data <- out$vars[[varnam]] %>% as.vector()
  df <- tibble( varnam = data )
  return(df)
}
