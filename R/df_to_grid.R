#' Converts a data frame to a a grid
#'
#' Converts a data frame with longitude and latitude information to a a grid
#'
#' @param df A data frame. Must have columns named \code{'lon'} and \code{'lat'}
#' with values shared with longitude and latidue values in the NetCDF file provided
#' by argument \code{maskfiln}.
#' @param varnam A character string specifying the variable name in the data frame.
#' @param grid A character string specifying the path of the NetCDF file that is
#' to be used for defining the array (grid). Defaults to \code{"halfdeg"} (a global
#' grid with half-degree gridcell size and longitude values ranging from -179.75,
#' -179.25, ... 179.75, and latitude values from -89.75, -89.25, ... 89.75.
#' @return An 2D matrix
#' @export
#'
df_to_grid <- function(df, varnam, grid = "halfdeg"){

  ## Expand to all gridcells
  # grid <- paste0(dir, "s1_fapar3g_v3_global.fland.nc")

  if (grid=="halfdeg"){
    out <- list(
      lon = seq(from = -179.75, to = 179.75, by = 0.5),
      lat = seq(from = -89.75, to = 89.75, by = 0.5)
    )
  } else {
    nc <- ncdf4::nc_open(grid)
    {
      sink(paste0(maskfiln, ".txt"))
      print(nc)
      sink()
    }

    out <- list(
      lon = ncdf4::ncvar_get(nc, nc$dim$lon$name),
      lat = ncdf4::ncvar_get(nc, nc$dim$lat$name)
    )
  }

  arr <- expand.grid(out$lon, out$lat) %>%
    # as.matrix() %>%
    dplyr::as_tibble() %>%
    setNames(c("lon", "lat")) %>%
    dplyr::left_join(dplyr::select(df, lon, lat, eval(varnam)), by = c("lon", "lat")) %>%

    ## convert to array
    dplyr::select(lon, lat, eval(varnam)) %>%
    tidyr::spread(lon, eval(varnam)) %>%
    dplyr::select(-lat) %>%
    as.matrix()

  return(t(arr))
}
