#' Slice an nc object
#'
#' Extracts a single time step from an rbeni-nc object
#'
#' @param obj a list returned from a \code{rbeni::read_nc_onefile()} function call.
#' @param tstep an integer or vector of integers specifying which time step(s) (index starting at 1) to be extracted.
#' @return A list, containing \code{"lon"} (vector of longitudes of
#' gridcell mid-points), \code{"lat"} (vector of latitudes of gridcell
#' mid-points), \code{"time"} (vector of lubridate::ymd dates),
#' \code{"varnams"} (a vector of all variable names as strings), and a
#' named (nested) list of the data arrays (lon x lat x time) for each
#' variable.
#' @export
#'
slice_nc <- function(obj, tstep){

  obj$time <- obj$time[tstep]

  extract_tstep <- function(arr, tstep){
    arr[,,tstep]
  }
  obj$vars <- purrr::map(obj$vars, ~extract_tstep(., tstep))

  return(obj)
}
