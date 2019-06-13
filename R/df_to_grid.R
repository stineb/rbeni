#' Converts a data frame to a a grid
#'
#' Converts a data frame with longitude and latitude information to a a grid
#'
#' @param df A data frame. Must have columns named \code{'lon'} and \code{'lat'}
#' with values shared with longitude and latidue values in the NetCDF file provided
#' by argument \code{maskfiln}.
#' @param varnam A character string specifying the variable name in the data frame.
#' @param maskfiln A character string specifying the path of the NetCDF file that is
#' to be used for defining the array (grid).
#' @return An 2D matrix
#' @export
#'
df_to_grid <- function(df, varnam, maskfiln){

  ## Expand to all gridcells
  # maskfiln <- paste0(dir, "s1_fapar3g_v3_global.fland.nc")

  nc <- ncdf4::nc_open(maskfiln)
  {
    sink(paste0(maskfiln, ".txt"))
    print(nc)
    sink()
  }

  out <- list(
    lon = ncdf4::ncvar_get(nc, nc$dim$lon$name),
    lat = ncdf4::ncvar_get(nc, nc$dim$lat$name)
  )

  arr <- expand.grid(out$lon, out$lat) %>%
    as.matrix() %>%
    dplyr::as_tibble() %>%
    setNames(c("lon", "lat")) %>%
    dplyr::left_join(select(df, lon, lat, eval(varnam)), by = c("lon", "lat")) %>%

    ## convert to array
    dplyr::select(lon, lat, eval(varnam)) %>%
    tidyr::spread(lon, eval(varnam)) %>%
    dplyr::select(-lat) %>%
    as.matrix()

  return(t(arr))
}
