#' Plot a nice map with ggplot.
#'
#' Returns a ggplot object for a global map plot.
#'
#' @param obj An object, either a \code{RasterBrick} (returned from a \code{raster::brick()} function call),
#' or a list returned from a \code{rbeni::read_nc_onefile()} function call.
#' @param varname A character string specifying the variable name in the NetCDF file that is to be regridded,
#' @param method Use \code{method == "bil"} for a bilinear interpolation using \code{cdo remapbil}. Use
#' \code{"max"} for a regridding by maximum using the raster R package.
#' @param outgrid Use \code{outgrid = "halfdeg"} to regrid to half-degree grid (spanning -179.75 to 179.75 in
#' longitude, and -89.75 to 89.75 in latitude).
#' @param returnobj A boolean.
#' @return If argument \code{retrunobj = TRUE}, the regridded object is returned.
#' @export
#'
regrid_nc <- function(obj, varname = "", method = "bil", outgrid = "", returnobj = FALSE, lonout = NA, latout = NA){

  if (method == "max"){

    #rlang::warn("Using raster package for regridding by maximum (method 'max')")

    if (typeof(obj)=="character"){
      if (file.exists(obj)){
        #rlang::warn("Reading file given by argument 'obj' as a raster.")
        rasta <- raster::raster(obj, varname = varname)
        filn_orig <- obj
      } else {
        rlang::abort(paste("File does not exist:", obj))
      }
    } else if (class(obj)[[1]]=="RasterLayer"){
      #rlang::warn("Raster object provided as argument 'obj'. Thanks.")
    }

    if (outgrid == "halfdeg"){
      dlon <- 0.5
      fact_x <- dlon / raster::res(rasta)[1]
      dlat <- 0.5
      fact_y <- dlat / raster::res(rasta)[2]

    } else if (!identical(NA, lonout)){
      dlon <- lonout[2] - lonout[1]
      fact_x <- dlon / raster::res(rasta)[1]
      if (!identical(NA, latout)){
        dlat <- latout[2] - latout[1]
        fact_y <- dlat / raster::res(rasta)[1]
      }

    }

    ## regrid by maximum (same aggregation factor in x and y directions)
    rasta_lores <- raster::aggregate( x = rasta, fact = fact_x, fun = max, na.rm = TRUE  )

    ## write to file
    dirn <- dirname(filn_orig)
    filn_out <- basename(filn_orig) %>% stringr::str_remove(., ".nc") %>% paste0(dirn, "/", ., "_halfdeg.nc")
    print(paste("Regridded file written:", filn_out))
    raster::writeRaster(rasta_lores, filename = filn_out, format = "CDF", overwrite = TRUE )

    ## Problem with raster writing: if time dimension has length 1, it's not written.
    ## take time dimension from original high resolution file and write file again
    rm("rasta") # to save memory
    nc_hires <- read_nc_onefile(filn_orig)
    nc_lores <- read_nc_onefile(filn_out)
    nc_lores$time <- nc_hires$time
    write_nc2(nc_lores, outfilnam = filn_out, lon = lonout, lat = latout)

    ## read back in as rbeni::nc object
    if (!returnobj){
      nc_lores <- NULL
    }

  }
  return(nc_lores)
}
