#' Integrates a global field
#'
#' Calculates the integral of a variable given in per-area units over the entire longitude-latitude domain given in the file
#' using CDO
#'
#' @param ifil Input file name
#' @param landfil A separate file providing the land area fraction per gridcell, must have the same resolution and domain as
#' \code{ifil}
#' @param mean_time A logical specifying whether mean across time steps should be taken. Defaults to \code{TRUE}.
#' @param varnam (Optional) a variable name
#' @return A numeric value or vector of numeric values
#' @export
#'
integrate_lon <- function(ifil, landfil = NA, mean_time = TRUE, varnam = NA, ...){

  ofil <- file.path(tempdir(), paste0("ofil_", basename(ifil)))

  if (!is.na(landfil)){
    if (!file.exists(landfil)){
      rlang::abort(paste("Argument landfil provided but file does not exist: ", landfil))
    }
    system(paste0( path.package("rbeni"), "/bash/multiply_gridarea.sh ", ifil, " ", ofil, " ", landfil))
  } else {
    system(paste0( path.package("rbeni"), "/bash/multiply_gridarea.sh ", ifil, " ", ofil))
  }

  # system(paste0("cdo zonsum ", ifil, " ", ofil))

  nc <- read_nc_onefile(ofil, ...)

  if (is.na(varnam)){
    varnam <- nc$varnams[1]
  }

  arr <- nc$vars[[ varnam ]]

  if (mean_time){

    if (length(dim(arr))==3){
      ## assuming that it has lon, lat, time dimensions
      ## take sum across lon
      arr <- apply(arr, c(2,3), FUN = sum, na.rm = TRUE)
    }

    ## now take mean across time
    arr <- apply(arr, 1, FUN = mean)

    df <- tibble(
      lat = nc$lat,
      var = arr
      ) %>%
      setNames(c("lat", varnam))

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
