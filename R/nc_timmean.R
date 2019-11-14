#' Takes mean across time steps
#'
#' Takes mean across time steps using CDO
#'
#' @param ifil Input file name
#' @param return_ofil A boolean specifying whether the output file name should be returned
#' @return An nc object
#' @export
#'
nc_timmean <- function(ifil, return_ofil = FALSE){

  ofil <- file.path(tempdir(), paste0("ofil_", basename(ifil)))
  system(paste0("cdo timmean ", ifil, " ", ofil))

  if (return_ofil){
    return(ofil)
  } else {
    nc <- read_nc_onefile(ofil)
    return(nc)
  }
}
