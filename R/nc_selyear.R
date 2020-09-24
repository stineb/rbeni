#' Subsets by years
#'
#' Subsets an nc object by years
#'
#' @param nc An object returned by \link{read_nc_onefile}
#' @param years An integer or vector of integers specifying the years to be selected.
#' @return An nc object.
#' @export
#'
#'
nc_selyear <- function(nc, years){

  require(dplyr)

  reduce_arr <- function(arr, idxs){
    if (length(dim(arr))==3){
      arr <- arr[,,idxs]
    } else {
      rlang::abort("nc_selyear: aborting. number of dimensions must be three. using third dimension as time.")
    }
    return(arr)
  }

  if ("POSIXt" %in% class(nc$time) || "Date" %in% class(nc$time)){
    idx_select <- lubridate::year(nc$time) %in% years %>%
      which()
  } else {
    idx_select <- nc$time %in% years %>%
      which()
  }

  nc$time <- nc$time[idx_select]
  if (length(ls(nc$vars))>1){
    rlang::warn("nc_selyear(): Taking only first variable to subset years of available variables:\n")
    rlang::warn(ls(nc$vars))
  }  
  nc$vars <- purrr::map(nc$vars[1], ~reduce_arr(., idx_select))
  
  return(nc)
}
