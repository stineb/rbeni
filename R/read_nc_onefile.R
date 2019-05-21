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
  }

  out <- list(
    lon = ncdf4::ncvar_get(nc, nc$dim$lon$name),
    lat = ncdf4::ncvar_get(nc, nc$dim$lat$name),
    time = ncdf4::ncvar_get(nc, nc$dim$time$name)
  )

  if (nc$dim$time$units=="days since 2001-1-1 0:0:0"){
    out$time <- conv_noleap_to_ymd(out$time, origin = lubridate::ymd("2001-01-01"))
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
