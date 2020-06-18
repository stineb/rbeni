#' Extracts full time series
#'
#' Extracts full time series from a list of NetCDF files, provided for time
#' steps (can be one time step or multiple time steps) separately.
#'
#' @param nclist A vector of character strings specifying the complete paths to files.
#' @param outdir A character string specifying output directory where data frames are written
#' using the \code{save} statement.
#' @param fileprefix A character string specifying the file name prefix.
#' @param varnam The variable name(s) for which data is to be read from NetCDF files.
#' @param lonnam The dimension name of longitude.
#' @param timenam The name of dimension variable used for time. Defaults to \code{"time"}.
#' @param timedimnam The name of the dimension (axis) used for time. Defaults to \code{"time"}.
#' @param ncores Number of cores for parallel execution (distributing extraction of
#' longitude slices).
#' @param single_basedate A logical specifying whether all files in the file list have the same
#' base date (e.g., time units given in 'days since <basedate>').
#'
#' @return Nothing. Writes data to files.
#' @export
#'
nclist_to_df <- function(nclist, outdir, fileprefix, varnam, lonnam, timenam = "time", timedimnam = "time", ncores = 1, single_basedate = FALSE){

  require(dplyr)
  require(magrittr)

  ## open one file to get longitude information
  nlon <- ncmeta::nc_dim(nclist[1], lonnam) %>%
    dplyr::pull(length)

  if (single_basedate){
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

  ## collect time series per longitude slice and create separate files per longitude slice.
  ## This step can be parallelized (dependecies: tidync, dplyr, tidyr, purrr, magrittr)
  if (ncores > 1){

    cl <- multidplyr::new_cluster(ncores) %>%
      multidplyr::cluster_library(c("dplyr", "purrr", "tidyr", "tidync", "dplyr", "magrittr")) %>%
      multidplyr::cluster_assign(nclist = nclist) %>%
      multidplyr::cluster_assign(outdir = outdir) %>%
      multidplyr::cluster_assign(fileprefix = fileprefix) %>%
      multidplyr::cluster_assign(varnam = varnam) %>%
      multidplyr::cluster_assign(lonnam = lonnam) %>%
      multidplyr::cluster_assign(basedate = basedate) %>%
      multidplyr::cluster_assign(timenam = timenam) %>%
      multidplyr::cluster_assign(timedimnam = timedimnam) %>%
      multidplyr::cluster_assign(nclist_to_df_byidx = nclist_to_df_byidx)

    ## distribute to cores, making sure all data from a specific site is sent to the same core
    df_out <- tibble(ilon = seq(nlon)) %>%
      multidplyr::partition(cl) %>%
      dplyr::mutate(out = purrr::map_int( ilon,
                                      ~nclist_to_df_byidx(nclist, ., outdir, fileprefix, varnam, lonnam, basedate, timenam, timedimnam)))

  } else {
    purrr::map(as.list(seq(nlon)), ~nclist_to_df_byidx(nclist, ., outdir, fileprefix, varnam, lonnam, basedate, timenam, timedimnam))
  }

}

nclist_to_df_byidx <- function(nclist, idx, outdir, fileprefix, varnam, lonnam, basedate, timenam, timedimnam){

  nclist_to_df_byfil <- function(filnam, idx, basedate, varnam, lonnam, timenam, timedimnam){

    if (is.na(basedate)){
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
      tidync::hyper_filter(lon = index == idx) %>%
      tidync::hyper_tibble(select_var(varnam)) %>%
      dplyr::rename(time = !!timedimnam)
    
    if (nrow(df)>0){
      df <- df %>% 
        dplyr::mutate(time = basedate + lubridate::days(time) - lubridate::days(1))
    }

    return(df)
  }

  ## check whether output has been created already (otherwise do nothing)
  if (!dir.exists(outdir)){system(paste0("mkdir -p ", outdir))}
  outpath <- paste0(outdir, fileprefix, "_ilon_", idx, ".RData")

  if (!file.exists(outpath)){

    ## get data from all files at given longitude index idx
    df <- purrr::map_dfr(
      as.list(nclist),
      ~nclist_to_df_byfil(., idx, basedate = basedate, varnam = varnam, lonnam = lonnam, timenam = timenam, timedimnam = timedimnam)
      ) %>%
      dplyr::group_by(lon, lat) %>%
      tidyr::nest() %>%
      dplyr::mutate(data = purrr::map(data, ~arrange(., time)))

    print(paste("Writing file", outpath, "..."))
    save(df, file = outpath)

  } else {
    print(paste("File exists already:", outpath))
  }

  return(idx)
}


