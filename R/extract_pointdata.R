#' Extract point data fron NetCDF file
#'
#' Extracts point data fron NetCDF file, given a geographic location (longitude,
#' latitude).
#'
#' @param filename A numeric specifying the longitude of the point for which data is to be read.
#' @param df_lonlat A data frame containing columns \code{"lon"} (longitude in degrees) and \code{"lat"}
#' (latitude in degrees)
#' @param get_time A logical specifying whether time values are to be read and combined with nested
#' data frames.
#' @return A data frame with columns \code{"lon"} and \code{"lat"} and a separate column for data.
#' @export
#'
extract_pointdata_allsites <- function( filename, df_lonlat, get_time = FALSE ){

  ## load file using the raster library
  #print(paste("Creating raster brick from file", filename))
  if (!file.exists(filename)) rlang::abort(paste0("File not found: ", filename))
  rasta <- raster::brick(filename)

  df_lonlat <- raster::extract(
    rasta,
    sp::SpatialPoints(dplyr::select(df_lonlat, lon, lat)), # , proj4string = rasta@crs
    sp = TRUE
  ) %>%
    as_tibble() %>%
    tidyr::nest(data = c(-lon, -lat)) %>%
    right_join(df_lonlat, by = c("lon", "lat")) %>%
    mutate( data = purrr::map(data, ~dplyr::slice(., 1)) ) %>%
    dplyr::mutate(data = purrr::map(data, ~t(.))) %>%
    dplyr::mutate(data = purrr::map(data, ~as_tibble(.)))

  if (get_time){
    timevals <- raster::getZ(rasta)
    df_lonlat <- df_lonlat %>%
      mutate( data = purrr::map(data, ~bind_cols(., tibble(date = timevals))))
  }

  return(df_lonlat)
}
