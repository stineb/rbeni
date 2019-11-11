#' Integrates a global field
#'
#' Calculates the integral of a variable given in per-area units over the entire longitude-latitude domain given in the file
#' using CDO
#'
#' @param ifil Input file name
#' @param landfil A separate file providing the land area fraction per gridcell, must have the same resolution and domain as
#' \code{ifil}
#' @param varnam (Optional) a variable name
#' @return A numeric value or vector of numeric values
#' @export
#'
integrate_lonlat <- function(ifil, landfil = NA, varnam = NA, ...){

  ofil <- file.path(tempdir(), paste0("ofil_", basename(ifil)))

  if (!is.na(landfil)){
    if (!file.exists(landfil)){
      rlang::abort(paste("Argument landfil provided but file does not exist: ", landfil))
    }
    system( paste0( path.package("rbeni"), "/bash/integrate_lonlat.sh ", ifil, " ", ofil, " ", landfil ) )
  } else {
    system( paste0( path.package("rbeni"), "/bash/integrate_lonlat.sh ", ifil, " ", ofil ) )
  }

  nc <- read_nc_onefile(ofil, ...)

  if (!is.na(varnam)){
    df <- tibble(time = nc$time, var = nc$vars[[ varnam ]]) %>%
      setNames(c("date", varnam))
  } else {
    df <- tibble(time = nc$time, var = nc$vars[[ nc$varnams[1] ]]) %>%
      setNames(c("date", nc$varnams[1]))
  }

  system(paste("rm", ofil))

  return(df)
}
