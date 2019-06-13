#' NetCDF to data frame
#'
#' Reads a NetCDF file into a data frame with longitude and latitude
#' information
#'
#' @param ncfilnam A character string specifying the NetCDF file to be read.
#' @param varnam A character string specifying the variable name in the NetCDF file.
#' @param do_get_ilon_ilat A boolean specifying whether longitude and latitude
#' indices are added to the data frame. Defaults to \code{FALSE}.
#' @return A data frame
#' @export
#'
nc_to_df <- function(ncfilnam, varnam, do_get_ilon_ilat=FALSE){

  ## get land mask (data frame with lon, lat, ilon, and ilat for each land cell)
  nc <- ncdf4::nc_open(ncfilnam)
  {
    sink(paste0(ncfilnam, ".txt"))
    print(nc)
    sink()
    unlink(paste0(ncfilnam, ".txt"))
  }

  out <- list(
    lon = ncdf4::ncvar_get(nc, nc$dim$lon$name),
    lat = ncdf4::ncvar_get(nc, nc$dim$lat$name)
  )

  df <- expand.grid(out$lon, out$lat) %>%
    as.matrix() %>%
    as_tibble() %>%
    setNames(c("lon", "lat"))

  myvar <- ncdf4::ncvar_get(nc, varnam)
  ncdf4::nc_close(nc)

  df <- df %>%
    bind_cols(tibble(myvar = as.vector(myvar)))

  if (do_get_ilon_ilat){
    df <- df %>%
      mutate(idx = 1:n()) %>%
      group_by(idx) %>%
      nest() %>%
      mutate(out_ilon_ilat = purrr::map(data, ~get_ilon_ilat( .$lon, .$lat, out$lon, out$lat )))
  }

  return(df)
}

get_ilon_ilat <- function(lon, lat, lon_vec, lat_vec){

  ## get index to read data only for this index
  ilon <- which.min(abs(lon - lon_vec))
  ilat <- which.min(abs(lat - lat_vec))

  df <- tibble(lon=lon, lat=lat, ilon=ilon, ilat=ilat)

  return(df)
}
