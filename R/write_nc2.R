#' Write array to a NetCDF file
#'
#' Writes one or multiple arrays with identical dimensions to a NetCDF file,
#' creating multiple variables each with the same dimension.
#'
#' @param obj Either an rbeni-nc object or an array of size length(lon) x length(lat) x length(z_dim) x length(time).
#' The order of lon (1st dim), lat (2nd dim), time (last dim) is mandatory. If it's an rbeni-nc object, then no additional
#' aruguments are required.
#' @param varnams A character string (or vector of strings) specifying the name(s) of the variable(s) that is/are written into the NetCDF output.
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
#' @param path A character string specifying the file path of the NetCDF file to be written.
#' @param verbose A boolean specifying whether messages are to be returned. Defaults to \code{FALSE}.
#' @param lonnam Longitude name
#' @param latnam Latitude name
#' @param zdimnam Z-dimension name
#' @param timenam Time dimension name
#' @param units_lon Units of longitude
#' @param units_lat Units of latitude
#' @param units Units of variables.
#' @param units_time Units of time dimension.
#' @param units_zdim Units of z-dimension.
#' @param long_names Long name of \code{var}.
#' @param missing_value Missing value for all variables.
#' @param att_title Global attribute \code{"Title"}.
#' @param att_history Global attribute \code{"History"}.

#' @return The path of the written NetCDF file.
#' @export
#'
write_nc2 <- function(obj,
                      varnams = NA,
                      filnam_template = NA,
                      lonnam_template = "lon",
                      latnam_template = "lat",
                      lon = NA,
                      lat = NA,
                      z_dim = NA,
                      time  = NA,
                      make_zdim = FALSE,
                      make_tdim = FALSE,
                      path = "./out.nc",
                      verbose        = FALSE,
                      lonnam = "lon",
                      latnam = "lat",
                      zdimnam = "z",
                      timenam = "time",
                      units_lon      = "degrees_east",
                      units_lat      = "degrees_north",
                      units          = "NA",
                      units_time     = "days since 2001-01-01",
                      units_zdim     = "",
                      long_names     = "NA",
                      missing_value  = -9999,
                      att_title      = "",
                      att_history    = ""
                      ){

  ## If 'var' is a rbeni-nc element, then no additional info about dimensions must be provided
  if (class(obj) == "list"){

    if (is.element("vars", ls(obj)) && is.element("lat", ls(obj)) && is.element("lon", ls(obj))){

      if (!identical(NA, varnams)){
        var <- obj$vars[ varnams ]
      } else {
        rlang::warn("Using all variables in nc object.")
        var <- obj$vars
      }
      if (identical(lon, NA)) lon <- obj$lon
      if (identical(lat, NA)) lat <- obj$lat

      if ("time" %in% ls(obj) && is.na(make_tdim)){
        make_tdim <- TRUE
      } else {
        make_tdim <- FALSE
      }

      if (length(dim(var[[1]]))>3){
        make_zdim <- TRUE
      } else {
        make_zdim <- FALSE
      }

      if (make_tdim){
        if (units_time=="days since 2000-01-01"){
          time <- as.integer(obj$time - lubridate::ymd("2000-01-01"))
        } else if (units_time=="days since 2001-01-01"){
          time <- as.integer(obj$time - lubridate::ymd("2001-01-01"))
        } else {
          time <- obj$time
        }
      }

      if (make_zdim){
        zdimname <- varnams[-which(varnams=="lon" | varnams=="lat" | varnams=="time" | varnams=="vars" | varnams=="varnams")]
        z_dim <- obj[[zdimname]]
      }

      if (identical(NA, varnams)){
        varnams <- obj$varnams
      }

      nvars <- length(varnams)

    }

  } else {

    var <- list()

    if (identical(lon, NA)) lon <- seq(dim(obj)[1])
    if (identical(lat, NA)) lat <- seq(dim(obj)[2])

    ## Checks
    ## Get the dimensionality of 'obj' and whether the dimension vector provided fit.
    vardims <- dim(obj)
    if (length(lon)!=vardims[1]){rlang::abort("Aborting. Longitude vector provided does not match the first dimension of the argument 'obj'.")}
    if (length(lat)!=vardims[2]){rlang::abort("Aborting. Latitude vector provided does not match the second dimension of the argument 'obj'.")}

    ## sanity checks
    if (length(vardims)==4){
      make_zdim==TRUE
      make_tdim==TRUE
      if (is.na(make_zdim)){
        if (identical(z_dim, NA)){rlang::abort("Aborting. No z_dim vector provided.")}
      }
      if (is.na(make_tdim)){
        if (identical(time, NA)) {rlang::abort("Aborting. No time vector provided.")}
      }
    } else if (length(vardims)==3) {
      if (make_zdim) {
        if (length(z_dim)==1){
          if (identical(z_dim, NA)) {rlang::abort("No z_dim vector provided")}
        }
      }
      if (!is.na(time) || make_tdim){
        if (make_tdim && is.na(time)){
          rlang::abort("No time provided")
        }
      }
    } else if (length(vardims)==2){
      make_zdim <- FALSE
      make_tdim <- FALSE
    }

    if (length(vardims)==3 && identical(z_dim, NA) && identical(time, NA) ){
      print("Array to be written as 3-dim.")
      print("Provide z_dim or time")
      rlang::abort("Aborting.")
    }

    if (length(vardims)==2){
      if (make_tdim) {
        if (length(time)!=1){
          print("time vector can only have length 1")
        } else {
          tmp <- array(NA,dim=c(dim(obj),1))
          tmp[,,1] <- obj
          var[[1]] <- tmp
        }
      } else if (make_zdim) {
        if (length(z_dim)!=1){
          print("z_dim vector can only have length 1")
        } else {
          print("extending output array by one dimension of length 1")
          tmp <- array(NA,dim=c(dim(obj),1))
          tmp[,,1] <- obj
          var[[1]] <- tmp
        }
      } else {
        ## simple 2D lon-lat object
        var[[1]] <- obj
      } 
    } else {
      print("output array has two dimensions")
      tmp <- array(NA,dim=dim(obj))
      var[[1]] <- obj
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
    if (make_zdim) zid    <- ncdf4::ncdim_def(zdimnam, units = units_zdim, vals = z_dim, longname = "z")
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
  varid <- purrr::map(as.list(seq(length(varnams))),
                      ~ncdf4::ncvar_def(name = varnams[.],
                                        units = units[.],
                                        dim = dimidlist,
                                        missval = missing_value,
                                        verbose = verbose,
                                        longname = long_names[.])
                      )

  ## Create file
  rlang::inform(paste0("Writing to file: ", path, "..."))
  nc <- ncdf4::nc_create(path, varid, verbose = verbose)

  ## Write the data to the file
  purrr::map(as.list(seq(length(varnams))),
             ~ncdf4::ncvar_put( nc, varid[[.]], var[[.]], start = NA, count = NA, verbose = verbose ))

  ## Add attributes
  ncdf4::ncatt_put( nc, varid = 0, "Title",   att_title,   prec = NA, verbose = verbose, definemode = FALSE )
  ncdf4::ncatt_put( nc, varid = 0, "History", att_history, prec = NA, verbose = verbose, definemode = FALSE )

  ncdf4::nc_close(nc)

  return(path)

}
