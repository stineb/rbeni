#' NetCDF to data frame
#'
#' Reads a NetCDF file into a data frame with longitude and latitude
#' information
#'
#' @param obj Either character string specifying the NetCDF file path to be read or a
#' object returned by function \link{read_nc_onefile()}.
#' @param varnam A character string specifying the variable name in the NetCDF file.
#' @param do_get_ilon_ilat A boolean specifying whether longitude and latitude
#' indices are added to the data frame. Defaults to \code{FALSE}.
#' @param dropna A boolean specifying wether rows where the data variable is NA should
#' be dropped. Recommended to be set to \code{TRUE} for large global arrays where much
#' of the grid is NA (e.g., over ocean).
#' @param filn A character string specifying the file name to which the data frame is
#' written. If \code{filn = NA} (defaults), the data frame is returned by the function,
#' otherwise, \code{NULL} is returned.
#' @param verbose A boolean specifying whether progress messages should be written to
#' prompt.
#' @return A data frame if \code{filn == NA}, otherwise nothing is returned.
#' @export
#'
nc_to_df <- function(obj, varnam, do_get_ilon_ilat = FALSE, dropna = FALSE, filn = NA, verbose = FALSE){

  hastime <- FALSE

  if (is.character(obj)){

    if (verbose) print(paste("Reading ", obj, "..."))

    # a character is provided by 'obj' -> read file into rbeni-nc object
    nc <- read_nc_onefile(obj)

  } else if (is.element("vars", ls(obj)) && is.element("lat", ls(obj)) && is.element("lon", ls(obj))){

    # an rbeni-nc object is provided by 'obj'
    nc <- obj
    rm("obj")

    if (is.element("time", ls(nc))){
      hastime <- TRUE
    }
    if (length(dim(nc$vars[[1]]))==2){
      hastime <- FALSE
    }

  }

  # expand to data frame
  if (verbose) print("Expanding data ...")
  if (hastime){

    df <- expand.grid(nc$lon, nc$lat, nc$time) %>%
      setNames(c("lon", "lat", "time")) %>%
      as_tibble()

  } else {

    df <- expand.grid(nc$lon, nc$lat) %>%
      setNames(c("lon", "lat")) %>%
      as_tibble()

  }

  # add data variable as column
  df <- df %>%
    dplyr::bind_cols(tibble(myvar = as.vector(nc$vars[[1]])))

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

  # add lon and lat index
  if (do_get_ilon_ilat){
    if (verbose) print("Adding indices ...")
    df <- df %>%
      dplyr::mutate(idx = 1:n()) %>%
      dplyr::group_by(idx) %>%
      tidyr::nest() %>%
      dplyr::mutate(out_ilon_ilat = purrr::map(data, ~get_ilon_ilat( .$lon, .$lat, obj$lon, obj$lat )))
  }

  # write to file
  if (!is.na(filn)){
    if (verbose) print(paste("Saving to file", filn, "..."))
    save(df, file = filn)
    rm("df")
  } else {
    return(df)
  }
}

get_ilon_ilat <- function(lon, lat, lon_vec, lat_vec){

  ## get index to read data only for this index
  ilon <- which.min(abs(lon - lon_vec))
  ilat <- which.min(abs(lat - lat_vec))

  df <- tibble(lon=lon, lat=lat, ilon=ilon, ilat=ilat)

  return(df)
}
