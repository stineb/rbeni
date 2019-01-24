####################################################################
## Returns an array containing the gridcell fraction of available land on the desired resolution.
## grid.out		either "halfdeg", "1x1deg", or "lpjgr" representing the thre standard resolutions
##			used in LPX.
## verbose=FALSE	set to true for verbose standard output
## fraction=TRUE        defaults to TRUE. Determines whether 'land.avail' (return values) is units of
##                      gridcell area fraction of absolute area (m2) within each gridcell.
##
## Beni Stocker, 21.03.2011
## -----------------------------------------------------------------

get.land.avail <- function( grid.out, dirnam, verbose=FALSE, fraction=TRUE ){
		
  library(RNetCDF)
  source('./regrid_library/area.R')

  if (grid.out=="halfdeg"){
    filn.dest <- paste( dirnam, "gicew_halfdeg.cdf", sep="" )
  } else if (grid.out=="1x1deg") {
    filn.dest <- paste( dirnam, "gicew_1x1deg.cdf", sep="" )
  } else if (grid.out=="lpjgr") {
    filn.dest <- paste( dirnam, "landmask_pelt04_rs_lpjgr.cdf", sep="" )
  } else {
    print("please provide destination grid as argument.")
    print("(halfdeg, 1x1deg, or lpjgr)")
  }

  ## Read file containing destination grid
  if (verbose) {print("reading grid information...")}
  nc <- open.nc(filn.dest)
  if (grid.out=="halfdeg"){
    lon <- var.get.nc(nc,"LON")
    lat <- var.get.nc(nc,"LAT")
    gicew <- var.get.nc(nc,"GICEW")
    land.avail <- 1-gicew  # gridcell fraction
  } else if (grid.out=="1x1deg") {
    lon <- var.get.nc(nc,"LON")
    lat <- var.get.nc(nc,"LAT")
    gicew <- var.get.nc(nc,"GICEW")
    land.avail <- 1-gicew  # gridcell fraction
  } else if (grid.out=="lpjgr") {
    lon <- var.get.nc(nc, "LONGITUDE")
    lat <- var.get.nc(nc, "LATITUDE")
    land.avail <- var.get.nc(nc,"LAND",c(1,1,1),c(length(lon),length(lat),1)) ## 0 ka BP slice
    land.avail[land.avail==1] <- 0
    land.avail[is.na(land.avail)] <- 0
    land.avail[land.avail==2] <- 1 # gridcell fraction
  }
  close.nc(nc)

  if (!fraction) {    
    for (k in seq(dim(land.avail)[1])){
      for (l in seq(dim(land.avail)[2])){
        land.avail[k,l] <- land.avail[k,l]*area(lat[l],lon[2]-lon[1],lat[2]-lat[1]) # in m2
      }
    }
  }

  out.land.avail <- list()
  out.land.avail$avail <- land.avail
  out.land.avail$lon <- lon
  out.land.avail$lat <- lat
  return(out.land.avail)
  
}
