#' Converts a data frame to a a grid
#'
#' Converts a data frame with longitude and latitude information to a a grid
#'
#' @param df A data frame. Must have columns named \code{'lon'} and \code{'lat'}
#' with values shared with longitude and latidue values in the NetCDF file provided
#' by argument \code{maskfiln}.
#' @param varnam A character string specifying the variable name in the data frame.
#' @param lonnam A character string specifying the column name containing longitude
#' values. Defaults to \code{"lon}.
#' @param latnam A character string specifying the column name containing longitude
#' values. Defaults to \code{"lat}.
#' @return A 2D matrix
#' @export
#'
df_to_grid <- function(df, varnam, lonnam = "lon", latnam = "lat"){

  lonvals <- unique(df[[lonnam]])
  latvals <- unique(df[[latnam]])
  
  ## make sure no gaps
  lonvals2 <- seq(min(lonvals), max(lonvals), by = min(abs(lonvals[2:(length(lonvals))] - lonvals[1:(length(lonvals)-1)])))
  latvals2 <- seq(min(latvals), max(latvals), by = min(abs(latvals[2:(length(latvals))] - latvals[1:(length(latvals)-1)])))
  
  # ## Expand to all gridcells
  # # grid <- paste0(dir, "s1_fapar3g_v3_global.fland.nc")
  # 
  # if (grid=="halfdeg"){
  #   out <- list(
  #     lon = seq(from = -179.75, to = 179.75, by = 0.5),
  #     lat = seq(from = -89.75, to = 89.75, by = 0.5)
  #   )
  # } else {
  #   nc <- ncdf4::nc_open(grid)
  #   {
  #     sink(paste0(maskfiln, ".txt"))
  #     print(nc)
  #     sink()
  #   }
  # 
  #   out <- list(
  #     lon = ncdf4::ncvar_get(nc, nc$dim$lon$name),
  #     lat = ncdf4::ncvar_get(nc, nc$dim$lat$name)
  #   )
  # }

  ## complete data frame with all possible combinations of longitude and latitude values
  arr <- expand.grid(lonvals, latvals) %>%
    # as.matrix() %>%
    dplyr::as_tibble() %>%
    setNames(c("lon", "lat")) %>%
    arrange(lon) %>% 
    dplyr::left_join(dplyr::select(df, lon, lat, eval(varnam)), by = c("lon", "lat")) %>%

    ## convert to array
    dplyr::select(lon, lat, eval(varnam)) %>%
    tidyr::pivot_wider(names_from = lon, values_from = !!varnam) %>% 
    arrange(lat) %>% 
    # tidyr::spread(lon, eval(varnam)) %>%
    dplyr::select(-lat) %>%
    as.matrix()
  
  rownames(arr) <- NULL

  return(t(arr))
}
