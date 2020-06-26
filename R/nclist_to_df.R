#' Extracts full time series
#'
#' Extracts full time series from a list of NetCDF files, provided for time
#' steps (can be one time step or multiple time steps) separately and writes
#' .RData files for each longitude index.
#'
#' @param nclist A vector of character strings specifying the complete paths to files.
#' @param outdir A character string specifying output directory where data frames are written
#' using the \code{save} statement.
#' @param fileprefix A character string specifying the file name prefix.
#' @param varnam The variable name(s) for which data is to be read from NetCDF files.
#' @param ilon An integer specifying an individual longitude index. If provided, it overrides that
#' the function extracts data for all longitude indices.
#' @param lonnam The dimension name of longitude.
#' @param latnam The dimension name of latitude
#' @param timenam The name of dimension variable used for time. Defaults to \code{"time"}.
#' @param timedimnam The name of the dimension (axis) used for time. Defaults to \code{"time"}.
#' @param ncores Number of cores for parallel execution (distributing extraction of
#' longitude slices). When set to \code{"all"}, the number of cores for parallelisation is
#' determined by \code{parallel::detectCores()}. Defaults to \code{1} (no parallelisation).
#' @param single_basedate A logical specifying whether all files in the file list have the same
#' base date (e.g., time units given in 'days since <basedate>').
#' @param fgetdate A function to derive the date used for the time dimension based on the file name.
#'
#' @return Nothing. Writes data to .RData files for each longitude index.
#' @export
#'
nclist_to_df <- function(nclist, outdir, fileprefix, varnam, ilon = NA, lonnam = "lon", latnam = "lat", timenam = "time", timedimnam = "time", ncores = 1, single_basedate = FALSE, fgetdate = NA){

  require(dplyr)
  require(magrittr)


  ## Determine longitude indices
  if (is.na(ilon)){
    ## open one file to get longitude information
    nlon <- ncmeta::nc_dim(nclist[1], lonnam) %>%
      dplyr::pull(length)

    ilon <- seq(nlon)
  }

  if (single_basedate && is.na(fgetdate)){
    ## get base date (to interpret time units in 'days since X')
    basedate <- ncmeta::nc_atts(nclist[1], timenam) %>%
      tidyr::unnest(cols = c(value)) %>%
      dplyr::filter(name == "units") %>%
      dplyr::pull(value) %>%
      stringr::str_remove("days since ") %>%
      stringr::str_remove(" 00:00:00") %>%
      lubridate::ymd()
  } else {
    basedate <- NA
  }

  if (ncores=="all"){
    ncores <- parallel::detectCores()
  }

  ## collect time series per longitude slice and create separate files per longitude slice.
  ## This step can be parallelized (dependecies: tidync, dplyr, tidyr, purrr, magrittr)
  if (ncores > 1 && length(ilon) > 1){

    cl <- multidplyr::new_cluster(ncores) %>%
      multidplyr::cluster_library(c("dplyr", "purrr", "tidyr", "tidync", "dplyr", "magrittr")) %>%
      multidplyr::cluster_assign(nclist = nclist) %>%
      multidplyr::cluster_assign(outdir = outdir) %>%
      multidplyr::cluster_assign(fileprefix = fileprefix) %>%
      multidplyr::cluster_assign(varnam = varnam) %>%
      multidplyr::cluster_assign(lonnam = lonnam) %>%
      multidplyr::cluster_assign(latnam = latnam) %>%
      multidplyr::cluster_assign(basedate = basedate) %>%
      multidplyr::cluster_assign(timenam = timenam) %>%
      multidplyr::cluster_assign(timedimnam = timedimnam) %>%
      multidplyr::cluster_assign(fgetdate = fgetdate) %>%
      multidplyr::cluster_assign(nclist_to_df_byilon = nclist_to_df_byilon)

    ## distribute to cores, making sure all data from a specific site is sent to the same core
    df_out <- tibble(ilon = ilon) %>%
      multidplyr::partition(cl) %>%
      dplyr::mutate(out = purrr::map_int( ilon,
                                      ~nclist_to_df_byilon(nclist, ., outdir, fileprefix, varnam, lonnam, latnam, basedate, timenam, timedimnam, fgetdate)))

  } else {
    purrr::map(as.list(ilon), ~nclist_to_df_byilon(nclist, ., outdir, fileprefix, varnam, lonnam, latnam, basedate, timenam, timedimnam, fgetdate))
  }

}

nclist_to_df_byilon <- function(nclist, ilon, outdir, fileprefix, varnam, lonnam, latnam, basedate, timenam, timedimnam, fgetdate){

  nclist_to_df_byfil <- function(filnam, ilon, basedate, varnam, lonnam, latnam, timenam, timedimnam, fgetdate){

    if (is.na(basedate) && is.na(fgetdate)){
      ## get base date (to interpret time units in 'days since X')
      basedate <- ncmeta::nc_atts(filnam, timenam) %>%
        tidyr::unnest(cols = c(value)) %>%
        dplyr::filter(name == "units") %>%
        dplyr::pull(value) %>%
        stringr::str_remove("days since ") %>%
        stringr::str_remove(" 00:00:00") %>%
        lubridate::ymd()
    }

    df <- tidync::tidync(filnam) %>%
      tidync::hyper_filter(lon = index == ilon) %>%
      tidync::hyper_tibble(select_var(varnam))

    if (nrow(df)>0){

      if (!is.na(fgetdate)){
        df <- df %>%
          dplyr::rename(lon = !!lonnam, lat = !!latnam) %>%
          dplyr::mutate(time = fgetdate(filnam))

      } else {
        df <- df %>%
          dplyr::rename(time = !!timedimnam, lon = !!lonnam, lat = !!latnam) %>%
          dplyr::mutate(time = basedate + lubridate::days(time)) #  - lubridate::days(1)

      }

    }

    return(df)
  }

  ## check whether output has been created already (otherwise do nothing)
  if (!dir.exists(outdir)){system(paste0("mkdir -p ", outdir))}
  outpath <- paste0(outdir, fileprefix, "_ilon_", ilon, ".RData")

  if (!file.exists(outpath)){

    print(paste("Getting file", outpath, "..."))

    ## get data from all files at given longitude index ilon
    df <- purrr::map(
      as.list(nclist),
      ~nclist_to_df_byfil(., ilon, basedate = basedate, varnam = varnam, lonnam = lonnam, latnam = latnam, timenam = timenam, timedimnam = timedimnam, fgetdate)
      )

    ## chech if any element has zero rows and drop that element
    drop_zerorows <- function(y) { return(y[!sapply(y, function(x) nrow(x)==0 )]) }
    df <- df %>%
      drop_zerorows()

    if (length(df)>0){
      df <- df %>%
        dplyr::bind_rows() %>%
        dplyr::group_by(lon, lat) %>%
        tidyr::nest() %>%
        dplyr::mutate(data = purrr::map(data, ~arrange(., time)))

      save(df, file = outpath)
      rm("df")
    }

  } else {
    print(paste("File exists already:", outpath))
  }

  return(ilon)
}


