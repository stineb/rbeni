extract_point_nc <- function( lon, lat, filnam, varnam, verbose = FALSE ){
  cmd <- paste0( "./bash/extract_pointdata_byfil.sh ", filnam, " ", varnam, " lon", " lat ", sprintf("%.2f",lon), " ", sprintf("%.2f",lat) )
  if (verbose) print( paste( "executing command:", cmd ) )
  system( cmd )
  vec <- read.table( "./out.txt" )$V1
  if (!is.numeric(vec)) vec <- NA
  if (verbose) print(vec)
  return( vec )
}