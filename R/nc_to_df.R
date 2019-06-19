#' NetCDF to data frame
#'
#' Reads a NetCDF file into a data frame with longitude and latitude
#' information
#'
#' @param nc Either character string specifying the NetCDF file path to be read or a
#' object returned by function \link{read_nc_onefile()}.
#' @param varnam A character string specifying the variable name in the NetCDF file.
#' @param do_get_ilon_ilat A boolean specifying whether longitude and latitude
#' indices are added to the data frame. Defaults to \code{FALSE}.
#' @return A data frame
#' @export
#'
nc_to_df <- function(obj, varnam, do_get_ilon_ilat=FALSE){

  if (is.character(obj)){

    ## get land mask (data frame with lon, lat, ilon, and ilat for each land cell)
    nc <- ncdf4::nc_open(obj)
    {
      sink(paste0(obj, ".txt"))
      print(nc)
      sink()
      unlink(paste0(obj, ".txt"))
    }

    ## get names of longitude and latitude dimensions
    dimnames <- ls(nc$dim)
    if (!("lon" %in% dimnames)){
      if ("LON" %in% dimnames){
        lonname <- "LON"
      } else if ("longitude" %in% dimnames){
        lonname <- "longitude"
      } else if ("Longitude" %in% dimnames){
        lonname <- "Longitude"
      }
    } else {
      lonname <- "lon"
    }
    if (!("lat" %in% dimnames)){
      if ("LAT" %in% dimnames){
        latname <- "LAT"
      } else if ("latitude" %in% dimnames){
        latname <- "latitude"
      } else if ("Latitude" %in% dimnames){
        latname <- "Latitude"
      }
    } else {
      latname <- "lat"
    }

    out <- list(
      lon = ncdf4::ncvar_get(nc, nc$dim[[lonname]]$name),
      lat = ncdf4::ncvar_get(nc, nc$dim[[latname]]$name)
    )

    df <- expand.grid(out$lon, out$lat) %>%
      as.matrix() %>%
      as_tibble() %>%
      setNames(c("lon", "lat"))

    myvar <- ncdf4::ncvar_get(nc, varnam)
    ncdf4::nc_close(nc)

    df <- df %>%
      bind_cols(tibble(myvar = as.vector(myvar)))

  } else if (is.element("vars", ls(obj)) && is.element("lat", ls(obj)) && is.element("lon", ls(obj))){

    df <- expand.grid(obj$lon, obj$lat) %>%
      as.matrix() %>%
      as_tibble() %>%
      setNames(c("lon", "lat"))

    df <- df %>%
      bind_cols(tibble(myvar = as.vector(obj$vars[[1]])))

  }


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
