#' Takes mean across time steps
#'
#' Takes mean across time steps using CDO
#'
#' @param ifil Input file name
#' @param return_ofil A boolean specifying whether the output file name should be returned
#' @return An nc object
#' @export
#'
nc_timmean <- function(ifil, return_ofil = NA){

  ofil <- file.path(tempdir(), paste0("ofil_", basename(ifil)))
  system(paste0("cdo timmean ", ifil, " ", ofil))

  if (!identical(return_ofil, NA)){
    return(ofil)
  } else {
    nc <- read_nc_onefile(ofil)
    return(nc)
  }
}
