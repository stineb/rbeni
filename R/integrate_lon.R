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
#' @param mask (Optional) a mask (2D array) corresponding to spatial dimensions of \code{ifil} containing \code{NA}
#' where values should be masked out.
#' @return A numeric value or vector of numeric values
#' @export
#'
integrate_lon <- function(ifil, landfil = NA, mean_time = TRUE, varnam = NA, mask = NA, ...){


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

  ## mask out values
  if (!identical(NA, mask)){
    arr[which(is.na(mask))] <- NA
  }

  if (mean_time){

    if (length(dim(arr))==3){
      ## now take mean across time
      arr <- apply(arr, c(1,2), FUN = mean, na.rm = TRUE)
    }

    ## now take sum across longitudes
    arr <- apply(arr, 2, FUN = sum, na.rm = TRUE)

    df <- tibble(
      lat = nc$lat,
      var = arr
      ) %>%
      mutate(var = ifelse(is.nan(var), NA, var)) %>%
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
