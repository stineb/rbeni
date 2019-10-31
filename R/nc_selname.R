#' Subsets by variable
#'
#' Subsets an nc object by variable name
#'
#' @param nc An object returned by \link{read_nc_onefile}
#' @return An nc object.
#' @export
#'
#'
nc_selname <- function(nc, var){

  ncout <- nc
  ncout$varnams <- var
  ncout$vars <- ncout$vars[which(names(nc$vars)==var)]
  return(ncout)

}
