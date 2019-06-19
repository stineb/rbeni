#' Extract point data fron NetCDF file
#'
#' Extracts point data fron NetCDF file, given a geographic location (longitude,
#' latitude).
#'
#' @param lon A numeric specifying the longitude of the point for which data is to be read.
#' @param lat A numeric specifying the latitude of the point for which data is to be read.
#' @param filnam A character string specifying the file name of the NetCDF file.
#' @param varnam A character string specifying the variable name of the variable in the NetCDF file.
#' @param verbose A boolean specifying whether verbose messages should be printed. Defaults to \code{FALSE}.
#' @return A value or vector of values.
#' @export
#'
extract_point_nc <- function( lon, lat, filnam, varnam, verbose = FALSE ){
  cmd <- paste0( "./bash/extract_pointdata_byfil.sh ", filnam, " ", varnam, " lon", " lat ", sprintf("%.2f",lon), " ", sprintf("%.2f",lat) )
  if (verbose) print( paste( "executing command:", cmd ) )
  system( cmd )
  vec <- read.table( "./out.txt" )$V1
  if (!is.numeric(vec)) vec <- NA
  if (verbose) print(vec)
  return( vec )
}
