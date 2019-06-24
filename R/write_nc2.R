#' Write array to a NetCDF file
#'
#' Writes one or multiple arrays with identical dimensions to a NetCDF file,
#' creating multiple variables each with the same dimension.
#'
#' @param var Either an rbeni-nc object or an array of size length(lon) x length(lat) x length(z_dim) x length(time).
#' The order of lon (1st dim), lat (2nd dim), time (last dim) is mandatory. If it's an rbeni-nc object, then no additional
#' aruguments are required.
#' @param varnam  A character string specifying the name of the (first) variable that is written into the NetCDF output.
#' @param filnam_template A character string specifying the file name of the template-NetCDF file from which all dimension information is read
#' and used for the NetCDF file created here. Defaults to \code{NA}, meaning that all dimension information has to be spedifyed by the user. Othewise all
#' dimension specification are overridden.
#' @param lonnam_template Longitude name in the template file. Required only if \code{!is.na(filnam_template)}.
#' @param latnam_template Latitude name in the template file. Required only if \code{!is.na(filnam_template)}.
#' @param lon     A vector containing the longitudes of the grid cell's mid-points. Defaults to \code{seq(dim(var)[1])}.
#' @param lat     A vector containing the latitudes of the grid cell's mid-points. Defaults to \code{seq(dim(var)[2])}.
#' @param z_dim   A vector containing the values of the z-dimension ("z", 3rd dimension). Defaults to NA (no z-dimension is written).
#' @param time    A vector containing the values of the time dimension. Defaults to NA (no time dimension is written).
#' @param make_zdim Write a z-dimension. If var is 2-dimensional, and \code{z_dim==NA}, create a z-dimension of length 1.
#' @param make_tdim Write a time dimension. If var is 2-dimensional, and \code{time==NA}, create a time dimension of length 1.
#' @param outfilnam  A character string specifying the name of the NetCDF file to be written.
#' @param var2 Array written to the file. Must have the same dimensions as \code{var}.
#' @param var3 Array written to the file. Must have the same dimensions as \code{var}.
#' @param var4 Array written to the file. Must have the same dimensions as \code{var}.
#' @param var5 Array written to the file. Must have the same dimensions as \code{var}.
#' @param varnam2 A character string specifying the name of \code{var2}.
#' @param varnam3 A character string specifying the name of \code{var3}.
#' @param varnam4 A character string specifying the name of \code{var4}.
#' @param varnam5 A character string specifying the name of \code{var5}.
#' @param verbose A boolean specifying whether messages are to be returned. Defaults to \code{FALSE}.
#' @param lonnam Longitude name
#' @param latnam Latitude name
#' @param zdimnam Z-dimension name
#' @param timenam Time dimension name
#' @param units_lon Units of longitude
#' @param units_lat Units of latitude
#' @param units_var1 Units of \code{var}.
#' @param units_var2 Units of \code{va2}.
#' @param units_time Units of time dimension.
#' @param units_zdim Units of z-dimension.
#' @param long_name_var1 Long name of \code{var}.
#' @param long_name_var2 Long name of \code{var2}.
#' @param missing_value Missing value for all variables.
#' @param att_title Global attribute \code{"Title"}.
#' @param att_history Global attribute \code{"History"}.

#' @return Nothing
#' @export
#'
write_nc2 <- function(var,
                      varnam = "varnam",
                      filnam_template = NA,
                      lonnam_template = "lon",
                      latnam_template = "lat",
                      lon = NA,
                      lat = NA,
                      z_dim = NA,
                      time  = NA,
                      make_zdim = FALSE,
                      make_tdim = FALSE,
                      outfilnam = "out.nc",
                      nvars          = 1,
                      var2           = NA, varnam2 = NA,
                      var3           = NA, varnam3 = NA,
                      var4           = NA, varnam4 = NA,
                      var5           = NA, varnam5 = NA,
                      verbose        = FALSE,
                      lonnam = "lon",
                      latnam = "lat",
                      zdimnam = "z",
                      timenam = "time",
                      units_lon      = "degrees_east",
                      units_lat      = "degrees_north",
                      units_var1     = "",
                      units_var2     = "",
                      units_time     = "days since 2001-1-1 0:0:0",
                      units_zdim     = "",
                      long_name_var1 = "",
                      long_name_var2 = "",
                      missing_value  = -9999,
                      att_title      = "",
                      att_history    = ""
                      ){

  ## If 'var' is a rbeni-nc element, then no additional info about dimensions must be provided
  if (is.element("vars", ls(var)) && is.element("lat", ls(var)) && is.element("lon", ls(var))){
    obj <- var
    var <- obj$vars[[1]]
    if (identical(lon, NA)) lon <- obj$lon
    if (identical(lat, NA)) lat <- obj$lat
    if ("time" %in% ls(obj)){
      make_tdim <- TRUE
    } else{
      make_tdim <- FALSE
    }
    if (length(ls(obj))>5){
      make_zdim <- TRUE
    } else {
      make_zdim <- FALSE
    }
    if (make_tdim) time <- obj$time
    if (make_zdim){
      varnams <- names(obj)
      zdimname <- varnams[-which(varnams=="lon" | varnams=="lat" | varnams=="time" | varnams=="vars" | varnams=="varnams")]
      z_dim <- obj[[zdimname]]
    }
    varnam <- obj$varnams[[1]]
    nvars <- length(obj$varnams)
    if (length(obj$varnams)>1) var2 <- obj$vars[[2]]
    if (length(obj$varnams)>2) var3 <- obj$vars[[3]]
    if (length(obj$varnams)>3) var4 <- obj$vars[[4]]
    if (length(obj$varnams)>4) var5 <- obj$vars[[5]]
    if (length(obj$varnams)>1) varnam2 <- obj$varnams[[2]]
    if (length(obj$varnams)>2) varnam3 <- obj$varnams[[3]]
    if (length(obj$varnams)>3) varnam4 <- obj$varnams[[4]]
    if (length(obj$varnams)>4) varnam5 <- obj$varnams[[5]]


  } else {

    if (identical(lon, NA)) lon <- seq(dim(var)[1])
    if (identical(lat, NA)) lat <- seq(dim(var)[2])

    ## Checks
    ## Get the dimensionality of 'var' and whether the dimension vector provided fit.
    vardims <- dim(var)
    if (length(lon)!=vardims[1]){rlang::abort("Aborting. Longitude vector provided does not match the first dimension of the argument 'var'.")}
    if (length(lat)!=vardims[2]){rlang::abort("Aborting. Latitude vector provided does not match the second dimension of the argument 'var'.")}

    if (length(vardims)==4){
      make_zdim==TRUE
      make_tdim==TRUE
      if (is.na(make_zdim)){
        if (identical(z_dim, NA)){rlang::abort("Aborting. No z_dim vector provided.")}
      }
      if (is.na(make_tdim)){
        if (identical(time, NA)) {rlang::abort("Aborting. No time vector provided.")}
      }
    } else {
      if (make_zdim) {
        if (length(z_dim)==1){
          if (identical(z_dim, NA)) {rlang::abort("No z_dim vector provided")}
        }
      }
      if (make_tdim) {
        if (length(time)==1){
          if (identical(time, NA)) {rlang::abort("No time; vector provided")}
        }
      }
    }

    if (length(vardims)==3 && identical(z_dim, NA) && identical(time, NA) ){
      print("Array to be written as 3-dim.")
      print("Provide z_dim or time")
      rlang::abort("Aborting.")
    }

    if ((length(vardims)==2 && make_tdim)||(length(vardims)==2 && make_zdim)) {
      if (make_tdim) {
        if (length(time)!=1){
          print("time vector can only have length 1")
        } else {
          tmp <- array(NA,dim=c(dim(var),1))
          tmp[,,1] <- var
          var <- tmp
          if (nvars>1){
            tmp <- array(NA,dim=c(dim(var2),1))
            tmp[,,1] <- var2
            var2 <- tmp
          }
          if (nvars>2){
            tmp <- array(NA,dim=c(dim(var3),1))
            tmp[,,1] <- var3
            var3 <- tmp
          }
          if (nvars>3){
            tmp <- array(NA,dim=c(dim(var4),1))
            tmp[,,1] <- var4
            var4 <- tmp
          }
          if (nvars>4){
            tmp <- array(NA,dim=c(dim(var5),1))
            tmp[,,1] <- var5
            var5 <- tmp
          }
        }
      } else if (make_zdim) {
        if (length(z_dim)!=1){
          print("z_dim vector can only have length 1")
        } else {
          print("extending output array by one dimension of length 1")
          tmp <- array(NA,dim=c(dim(var),1))
          tmp[,,1] <- var
          var <- tmp
          if (nvars>1){
            tmp <- array(NA,dim=c(dim(var2),1))
            tmp[,,1] <- var2
            var2 <- tmp
          }
          if (nvars>2){
            tmp <- array(NA,dim=c(dim(var3),1))
            tmp[,,1] <- var3
            var3 <- tmp
          }
          if (nvars>3){
            tmp <- array(NA,dim=c(dim(var4),1))
            tmp[,,1] <- var4
            var4 <- tmp
          }
          if (nvars>4){
            tmp <- array(NA,dim=c(dim(var5),1))
            tmp[,,1] <- var5
            var5 <- tmp
          }
        }
      }
    }

  }

  ## Define dimensions
  if (!is.na(filnam_template)){

    ## use an existing file as template for the grid (dimensions)
    nc_template <- ncdf4::nc_open( filnam_template )

    # Get dimensions from the template file
    dimidlist <- list()
    dimidlist[[1]] <- nc_template$dim[[ lonnam_template ]]
    dimidlist[[2]] <- nc_template$dim[[ latnam_template ]]
    if (length(nc_template$dim)>2) dimidlist[[3]] <- nc_template$dim[[3]]
    if (length(nc_template$dim)>3) dimidlist[[4]] <- nc_template$dim[[4]]

    ncdf4::nc_close(nc_template)

  } else {

    ## Define new dimensions
    if (verbose) {print("define dimensions...")}
    lonid                 <- ncdf4::ncdim_def(lonnam,  units = units_lon,  vals = lon,   longname = "Longitude (East)")
    latid                 <- ncdf4::ncdim_def(latnam,  units = units_lat,  vals = lat,   longname = "Latitude (North)")
    if (make_zdim) zid    <- ncdf4::ncdim_def(zdimnam, units = units_zdim, vals = z_dim, longname = "")
    if (make_tdim) timeid <- ncdf4::ncdim_def(timenam, units = units_time, vals = time,  longname = "Time")

    dimidlist <- list(lonid, latid)
    if (make_zdim) dimidlist[[3]] <- zid
    if (make_tdim){
      if (make_zdim){
        dimidlist[[4]] <- timeid
      } else {
        dimidlist[[3]] <- timeid
      }
    }

  }

  ## Define variables. All must have the same dimensions.
  varid1                           <- ncdf4::ncvar_def(name = varnam,  units = units_var1, dim = dimidlist, missval = missing_value, verbose = verbose)
  if (!identical(var2, NA)) varid2 <- ncdf4::ncvar_def(name = varnam2, units = units_var2, dim = dimidlist, missval = missing_value, verbose = verbose)
  if (!identical(var3, NA)) varid3 <- ncdf4::ncvar_def(name = varnam3, units = units_var3, dim = dimidlist, missval = missing_value, verbose = verbose)
  if (!identical(var4, NA)) varid4 <- ncdf4::ncvar_def(name = varnam4, units = units_var4, dim = dimidlist, missval = missing_value, verbose = verbose)
  if (!identical(var5, NA)) varid5 <- ncdf4::ncvar_def(name = varnam5, units = units_var5, dim = dimidlist, missval = missing_value, verbose = verbose)

  # varid_list <- purrr::map(
  #   as.list(varnams),
  #   ~ncdf4::ncvar_def(name = .,  units = "asdf", dim = dimidlist, missval = missing_value, verbose = verbose))

  varid_list <- list(varid1)
  if (!identical(var2, NA)) varid_list[[2]] <- varid2
  if (!identical(var3, NA)) varid_list[[3]] <- varid3
  if (!identical(var4, NA)) varid_list[[4]] <- varid4
  if (!identical(var5, NA)) varid_list[[5]] <- varid5

  ## Create file
  nc <- ncdf4::nc_create(outfilnam, varid_list, verbose = verbose)

  ## Write the data to the file
  ncdf4::ncvar_put( nc, varid1, var, start = NA, count = NA, verbose = verbose )
  if (!identical(var2, NA)) ncdf4::ncvar_put( nc, varid2, var2, start = NA, count = NA, verbose = verbose )
  if (!identical(var3, NA)) ncdf4::ncvar_put( nc, varid3, var3, start = NA, count = NA, verbose = verbose )
  if (!identical(var4, NA)) ncdf4::ncvar_put( nc, varid4, var4, start = NA, count = NA, verbose = verbose )
  if (!identical(var5, NA)) ncdf4::ncvar_put( nc, varid5, var5, start = NA, count = NA, verbose = verbose )

  ## Add attributes
  ncdf4::ncatt_put( nc, varid = 0, "Title",   att_title,   prec = NA, verbose = verbose, definemode = FALSE )
  ncdf4::ncatt_put( nc, varid = 0, "History", att_history, prec = NA, verbose = verbose, definemode = FALSE )

  ncdf4::nc_close(nc)

}
