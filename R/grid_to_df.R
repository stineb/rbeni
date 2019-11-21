#' NetCDF to data frame
#'
#' Reads a NetCDF file into a data frame with longitude and latitude
#' information
#'
#' @param arr A 2D or 3D array
#' @param grid A character string specifying the grid
#' @param dropna A boolean specifying wether rows where the data variable is NA should
#' be dropped. Recommended to be set to \code{TRUE} for large global arrays where much
#' of the grid is NA (e.g., over ocean).
#' @param timedim An integer specifying which dimension represents time for 3D arrays.
#' @return A data frame with columns \code{lon} and \code{lat}.
#' @export
#'
grid_to_df <- function(arr, grid = NA, dropna = TRUE, timedim = NA){

  if (is.na(timedim)){
    if (length(dim(arr))==3){
      ## assuming that time is the last dimension
      rlang::warn("Assuming that time is the last dimension")
      hastime <- TRUE
    } else {
      hastime <- FALSE
    }
  } else {
    hastime <- TRUE
  }

  if (is.na(grid)){

    if (dim(arr)[1] == 720 && dim(arr)[2]==360){
      grid <- "halfdeg"
    } else if (dim(arr)[1] == 360 && dim(arr)[2]==720){
      arr <- t(arr)
      grid <- "halfdeg"
    } else if (dim(arr)[1] == 360 && dim(arr)[2]==180){
      grid <- "1x1deg"
    } else if (dim(arr)[1] == 180 && dim(arr)[2]==360){
      arr <- t(arr)
      grid <- "1x1deg"
    }
  }

  ## arr is a 2D matrix (grid)
  if (grid=="halfdeg"){
    lon <- seq(from = -179.75, to = 179.75, by = 0.5)
    lat <- seq(from = -89.75, to = 89.75, by = 0.5)
  } else if (grid=="1x1deg"){
    lon <- seq(from = -179.75, to = 179.75, by = 1)
    lat <- seq(from = -89.75, to = 89.75, by = 1)
  } else {
    rlang::warn("Dimensions not identified")
    lon <- seq(dim(arr)[1])
    lat <- seq(dim(arr)[2])
  }

  if (hastime){

    if (is.na(timedim)){
      time <- seq(dim(arr)[3])
    } else {
      time <- seq(dim(arr)[timedim])
    }
    df <- expand.grid(lon, lat, time) %>%
      setNames(c("lon", "lat", "time")) %>%
      as_tibble()

  } else {

    df <- expand.grid(lon, lat) %>%
      setNames(c("lon", "lat")) %>%
      as_tibble()

  }

  # add data variable as column
  df <- df %>%
    dplyr::bind_cols(tibble(myvar = as.vector(arr)))

  if (dropna){
    if (verbose) print("Dropping NAs ...")
    df <- df %>%
      tidyr::drop_na(myvar)
  }

  ## nest data per gridcell
  if (hastime){
    if (verbose) print("Nesting data ...")
    df <- df %>%
      dplyr::group_by(lon, lat) %>%
      tidyr::nest()

  }

  # # add lon and lat index
  # if (do_get_ilon_ilat){
  #   if (verbose) print("Adding indices ...")
  #   df <- df %>%
  #     dplyr::mutate(idx = 1:n()) %>%
  #     dplyr::group_by(idx) %>%
  #     tidyr::nest() %>%
  #     dplyr::mutate(out_ilon_ilat = purrr::map(data, ~get_ilon_ilat( .$lon, .$lat, lon, lat )))
  # }

  # # write to file
  # if (!is.na(filn)){
  #   if (verbose) print(paste("Saving to file", filn, "..."))
  #   save(df, file = filn)
  #   rm("df")
  # } else {
  #   return(df)
  # }

  return(df)

}

get_ilon_ilat <- function(lon, lat, lon_vec, lat_vec){

  ## get index to read data only for this index
  ilon <- which.min(abs(lon - lon_vec))
  ilat <- which.min(abs(lat - lat_vec))

  df <- tibble(lon=lon, lat=lat, ilon=ilon, ilat=ilat)

  return(df)
}
