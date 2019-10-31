#' Integrates a global field
#'
#' Calculates the integral of a variable given in per-area units over the entire longitude-latitude domain given in the file
#' using CDO
#'
#' @param ifil Input file name
#' @param landfil A separate file providing the land area fraction per gridcell, must have the same resolution and domain as
#' \code{ifil}
#' @return A numeric value or vector of numeric values
#' @export
#'
integrate_lonlat <- function(ifil, landfil){
  ofil <- file.path(tempdir(), "ofil_tmp.nc")
  system( paste0( path.package("rbeni"), "/bash/integrate_lonlat.sh ", ifil, " ", ofil, " ", landfil ) )
  nc <- read_nc_onefile(ofil)
  df <- tibble(time = nc$time, var = nc$vars[[nc$varnams[[1]]]]) %>%
    setNames(c("date", unlist(nc$varnams)))
  system(paste("rm", ofil))
  return(df)
}
