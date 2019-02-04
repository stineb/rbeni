source("~/.Rprofile")
library(dplyr)
library(ncdf4)

yearstart = 2000
yearend   = 2015

for (year in yearstart:yearend){

	fil <- paste0( myhome, "data/gpp_modis/", as.character(year), "/MODIS-C06__MOD17A2-8-Daily-GPP__0.5deg__", as.character(year), "__NTSG-UMT__UHAM-ICDC__fv0.01.nc" )
	nc <- nc_open( fil )
	gpp <- ncvar_get( nc, varid="gpp" )
	lon <- nc$dim$lon$vals
	lat <- nc$dim$lat$vals
	time <- nc$dim$time$vals
	nc_close(nc)

	## fill missing values along time dimension
	gpp_filled <- gpp * NA
	for (ilon in seq(length(lon))){
		for (ilat in seq(length(lat))){
		  if (any(!is.na(gpp[ilon,ilat,]))){
		    gpp_filled[ilon,ilat,] <- na.approx( gpp[ilon,ilat,], na.rm = FALSE )
		  }
		}
	}	

	outfilnam <- gsub( ".nc", "_FILLEDBYTIME.nc", fil )
	print(paste("writing", outfilnam))
	cdf.write( gpp, "gpp", var2 = gpp_filled, varnam2 = "gpp_filled",
	           lon, rev(lat),
	           filnam = outfilnam,
	           nvars = 2,
	           time = time,
	           make.tdim = TRUE,
	           units_time = "days since 2000-01-01",
	           long_name_var1 = "Gross primary productivity",
	           units_var1 = "kgC m-2",
	           glob_hist = "created by utilities/fillmiss_modis.R"
	)
	
}

