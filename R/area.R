############################################################################
## Returns grid cell area in m2 on a spherical Earth
## MUST BE CONSISTENT WITH CALCULATION AND EARTH RADIUS AS DEFINED IN
## outannual.F, modelpara.inc !!!
## Beni Stocker, 25.03.2011.
## -------------------------------------------------------------------------

## dx and dy are in units of degrees

area <- function(lat,dx=1,dy=1){
  # r_earth <- 6370000
  r_earth <- 6370499.317638  # to be consistent with how Ferret calculates areas of spheres (https://www.pmel.noaa.gov/maillists/tmap/ferret_users/fu_2016/msg00155.html) 
  area <- 4 * r_earth^2 * 0.5 * dx * pi/180 * cos( abs(lat) * pi/180 ) * sin( 0.5 * dy * pi/180 )
  return(area)
}
