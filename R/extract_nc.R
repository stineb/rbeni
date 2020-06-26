#' Extracts a variable from an nc object
#'
#' Extracts a variable from an nc object
#'
#' @param obj a list returned from a \code{rbeni::read_nc_onefile()} function call.
#' @param var a character specifying the variable name to be extracted.
#' @return A list, containing \code{"lon"} (vector of longitudes of
#' gridcell mid-points), \code{"lat"} (vector of latitudes of gridcell
#' mid-points), \code{"time"} (vector of lubridate::ymd dates),
#' \code{"varnams"} (a vector of all variable names as strings), and a
#' named (nested) list of the data arrays (lon x lat x time) for each
#' variable.
#' @export
#'
extract_nc <- function(obj, var){

  obj$vars <- obj$vars[var]
  obj$varnams <- obj$varnams[which(obj$varnams %in% var)]

  return(obj)
}
