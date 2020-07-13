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
  
  idx_select <- lubridate::year(lubridate::ymd(nc$time)) %in% selyears %>% 
    which()  
  
  nc$time <- nc$time[idx_select]
  nc$vars <- purrr::map(nc$vars, ~reduce_arr(., idx_select))
  
  return(nc)
}
