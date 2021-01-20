#' Extracts point data from a raster file
#'
#' Extracts point data from a raster file (e.g. a NetCDF file)
#'
#' @param df  A data frame containing columns \code{lon} and \code{lat}
#' specifying longitude and latitude values of points for which data is
#' to be extracted (points organised by rows).
#' @param filn A character string specifying the path to a raster file that can be read with
#' \code{raster::brick} or \code{raster::raster}.
#' @return A data frame of the same number of rows as \code{df} with extracted data
#' nested in column \code{data}.
#' @export
#'
extract_nc <- function(df, filn, get_time = FALSE){

  rasta <- raster::brick(filn)

  df <- raster::extract(
    rasta,
    sp::SpatialPoints(dplyr::select(df, lon, lat)), # , proj4string = rasta@crs
    sp = TRUE
  ) %>%
    as_tibble() %>%
    tidyr::nest(data = c(-lon, -lat)) %>%
    right_join(df, by = c("lon", "lat")) %>%
    mutate( data = purrr::map(data, ~dplyr::slice(., 1)) ) %>%
    dplyr::mutate(data = purrr::map(data, ~t(.))) %>%
    dplyr::mutate(data = purrr::map(data, ~as_tibble(.)))

  ## xxx todo: use argument df = TRUE in the extract() function call in order to
  ## return a data frame directly (and not having to rearrange the data afterwards)
  ## xxx todo: implement the GWR method for interpolating using elevation as a
  ## covariate here.

  if (get_time){
    timevals <- raster::getZ(rasta)
    df <- df %>%
      mutate( data = purrr::map(data, ~bind_cols(., tibble(date = timevals))))
  }

  return(df)
}
