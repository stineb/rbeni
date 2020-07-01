#' Extract point data fron NetCDF file
#'
#' Extracts point data fron NetCDF file, given a geographic location (longitude,
#' latitude).
#'
#' @param filename A numeric specifying the longitude of the point for which data is to be read.
#' @param df_lonlat A data frame containing columns \code{"lon"} (longitude in degrees) and \code{"lat"}
#' (latitude in degrees)
#' @param time A logical specifying whether file has a time dimension.
#' @return A data frame with columns \code{"lon"} and \code{"lat"} and a separate column for data.
#' @export
#'
extract_pointdata_allsites <- function( filename, df_lonlat, time = FALSE ){

  ## load file using the raster library
  #print(paste("Creating raster brick from file", filename))
  if (!file.exists(filename)) rlang::abort(paste0("File not found: ", filename))
  if (time){
    rasta <- raster::brick(filename) 
  } else {
    rasta <- raster::raster(filename)
  }

  df <- raster::extract(
    rasta,
    sp::SpatialPoints(dplyr::select(df_lonlat, lon, lat)), # , proj4string = rasta@crs
    sp = TRUE
    ) %>% 
    as_tibble()
  
  if (time){
    df <- df %>%
      tidyr::nest(data = c(-lon, -lat)) %>%
      right_join(df_lonlat, by = c("lon", "lat")) %>%
      mutate( data = purrr::map(data, ~dplyr::slice(., 1)) ) %>%
      dplyr::mutate(data = purrr::map(data, ~t(.))) %>%
      dplyr::mutate(data = purrr::map(data, ~as_tibble(.)))

    timevals <- raster::getZ(rasta)
    df <- df %>%
      mutate( data = purrr::map(data, ~bind_cols(., tibble(date = timevals))))
  }

  return(df)
}
