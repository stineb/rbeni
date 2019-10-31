#' Integrates a global field
#'
#' Calculates the integral of a variable given in per-area units over the entire longitude-latitude domain given in the file
#' using CDO
#'
#' @param ifil Input file name
#' @param landfil A separate file providing the land area fraction per gridcell, must have the same resolution and domain as
#' \code{ifil}
#' @param mean_time A logical specifying whether mean across time steps should be taken.
#' @return A numeric value or vector of numeric values
#' @export
#'
integrate_lon <- function(ifil, landfil, mean_time = TRUE){

  ofil <- file.path(tempdir(), "ofil_tmp.nc")
  system(paste0( path.package("rbeni"), "/bash/multiply_gridarea.sh ", ifil, " ", ofil, " ", landfil))
  nc <- read_nc_onefile(ofil)
  arr <- apply( nc$vars[[nc$varnams[[1]]]], c(2,3), FUN = sum, na.rm = TRUE)

  if (mean_time){

    arr2 <- apply(arr, 1, FUN = mean)

    df <- tibble(
      lat = nc$lat,
      var = arr2
      ) %>%
      setNames(c("lat", unlist(nc$varnams)))

  } else {

    df_tmp <- arr %>%
      as_tibble() %>%
      setNames(paste0("time", seq(ncol(.))))

    df <- tibble(
      lat = nc$lat
      ) %>%
      bind_cols(df_tmp)

  }

  system(paste("rm", ofil))
  return(df)
}
