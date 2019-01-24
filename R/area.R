#' Gridcell area
#'
#' Calculates the gridcell area on the spherical Earth with radius 6370499.317638 km.
#'
#' @param lat Latitude in degrees
#' @param dx Gridcell extent in longitude in degrees. Defaults to 1.
#' @param dy Gridcell extent in latitude in degrees. Defaults to 1.
#'
#' @return A numerical value.
#
#' @export
#'
#' @examples area <- calc_area(45, 0.5)
#'
calc_area <- function( lat, dx=1, dy=1 ){
  r_earth <- 6370499.317638  # to be consistent with how Ferret calculates areas of spheres (https://www.pmel.noaa.gov/maillists/tmap/ferret_users/fu_2016/msg00155.html)
  area <- 4 * r_earth^2 * 0.5 * dx * pi/180 * cos( abs(lat) * pi/180 ) * sin( 0.5 * dy * pi/180 )
  return(area)
}
