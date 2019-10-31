#' Reads a NetCDF file
#'
#' Reads the full content of a NetCDF file, given only the file name
#'
#' @param filn A character string specifying the full path of the NetCDF
#' file to be read.
#' @return A list, containing \code{"lon"} (vector of longitudes of
#' gridcell mid-points), \code{"lat"} (vector of latitudes of gridcell
#' mid-points), \code{"time"} (vector of lubridate::ymd dates),
#' \code{"varnams"} (a vector of all variable names as strings), and a
#' named (nested) list of the data arrays (lon x lat x time) for each
#' variable.
#' @export
#'
read_nc_onefile <- function(filn){

  require(dplyr)

  nc <- ncdf4::nc_open(filn)
  # Save the print(nc) dump to a text file
  {
    sink(paste0(filn, ".txt"))
    print(nc)
    sink()
    unlink(paste0(filn, ".txt"))
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


  if (is.null(nc$dim$time)){
    ## no time dimension
    out <- list(
      lon = ncdf4::ncvar_get(nc, nc$dim[[lonname]]$name),
      lat = ncdf4::ncvar_get(nc, nc$dim[[latname]]$name)
      )

  } else {
    ## with time dimension
    out <- list(
      lon = ncdf4::ncvar_get(nc, nc$dim[[lonname]]$name),
      lat = ncdf4::ncvar_get(nc, nc$dim[[latname]]$name),
      time = ncdf4::ncvar_get(nc, nc$dim$time$name)
      )

    ## convert to date
    if (nc$dim$time$units=="days since 2001-1-1 0:0:0"){

      out$time <- conv_noleap_to_ymd(out$time, origin = lubridate::ymd("2001-01-01"))

    } else if (nc$dim$time$units=="days since 2000-01-01"){

      time_origin <- lubridate::ymd("2000-01-01")
      out$time <- lubridate::days(out$time) + time_origin

    } else if (nc$dim$time$units=="days since 2001-01-01"){

      time_origin <- lubridate::ymd("2001-01-01")
      out$time <- lubridate::days(out$time) + time_origin

    } else if (nc$dim$time$units=="days since 1900-1-1"){

      time_origin <- lubridate::ymd("1900-01-01")
      out$time <- lubridate::days(out$time) + time_origin

    } else {

      print("units of time not recognized")

    }
  }

  # get variables
  varnams <- ls(nc$var)
  getvar <- function(varnam){
    #tibble( !!varnam := ncdf4::ncvar_get(nc, varnam) )
    out <- list()
    out[[varnam]] <- ncdf4::ncvar_get(nc, varnam)
  }
  vars <- purrr::map(as.list(varnams), ~getvar(.)) %>%
    setNames(varnams)

  nc$var[[varnams[1]]]$units

  out[["vars"]] <- vars
  out[["varnams"]] <- varnams

  return(out)
}
