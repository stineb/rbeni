#' Empty global map
#'
#' Returns a ggplot object for an empty global map
#'
#' @param lonmin Left edge (longitude, in degrees), defaults to -180.
#' @param lonmax Right edge (longitude, in degrees), defaults to 180.
#' @param latmin Lower edge (latitude, in degrees), defaults to -90.
#' @param latmax Upper edge (latitude, in degrees), defaults to 90.
#' @return A ggplot object for a global map plot.
#' @export
#'
plot_map_simpl <- function(lonmin = -180, lonmax = 180, latmin = -60, latmax = 85){

	library(rworldmap)
  library(ggplot2)
  library(rnaturalearth)
  library(sf)

	#data(coastsCoarse)

	#sPDF <- getMap()[getMap()$ADMIN!='Antarctica',]
	sPDF <- rnaturalearth::ne_countries(scale = 110, returnclass = "sf")

	##---------------------------------------------
	## map theme
	##---------------------------------------------
	theme_map <- theme_grey() +    # theme_minimal()
	  theme(

	    plot.title = element_text(hjust = 0, face="bold", size = 18),

	    legend.position = "right", # c(0.07, 0.35), #"left"
	    # legend.key.size = unit(c(5, 1), "mm"),
	    legend.title=element_text(size=12),
	    legend.text=element_text(size=10),

	    # axis.line = element_blank(),
	    # axis.text = element_blank(),
	    # axis.title = element_blank(),

	    # panel.grid.major = element_blank(),
	    panel.grid.minor = element_blank()
	    # plot.margin = unit( c(0, 0, 0, 5) , "mm")
	  )

	# define labels
	lat.labels <- seq(-90, 90, 30)
	lat.short  <- seq(-90, 90, 10)
	lon.labels <- seq(-180, 180, 60)
	lon.short  <- seq(-180, 180, 10)

	# a <- sapply( lat.labels, function(x) if (x>0) {bquote(.(x)*degree ~N)} else if (x==0) {bquote(.(x)*degree)} else {bquote(.(-x)*degree ~S)} )
	# b <- sapply( lon.labels, function(x) if (x>0) {bquote(.(x)*degree ~E)} else if (x==0) {bquote(.(x)*degree)} else {bquote(.(-x)*degree ~W)})

	# a <- sapply( lat.labels, function(x) if (x>0) {substitute(paste(nn, degree), list(nn=x))} else if (x==0) {substitute(paste(nn, degree), list(nn=x))} else {substitute(paste(nn, degree), list(nn=x))} )
	# b <- sapply( lon.labels, function(x) if (x>0) {substitute(paste(nn, degree), list(nn=x))} else if (x==0) {substitute(paste(nn, degree), list(nn=x))} else {substitute(paste(nn, degree), list(nn=x))} )

	# a <- parse(text = paste(lat.labels, "*degree ~ N", sep = ""))
	# b <- parse(text = paste(lon.labels, "*degree ~ E", sep = ""))

	a <- sapply( lat.labels, function(x) if (x>0) {parse(text = paste0(x, "*degree ~ N"))} else if (x==0) {parse(text = paste0(x, "*degree"))} else {parse(text = paste0(-x, "*degree ~ S"))} )
	b <- sapply( lon.labels, function(x) if (x>0) {parse(text = paste0(x, "*degree ~ E"))} else if (x==0) {parse(text = paste0(x, "*degree"))} else {parse(text = paste0(-x, "*degree ~ W"))} )

	##---------------------------------------------
	## Create ggplot object
	##---------------------------------------------
	gg <- ggplot() +

		# background countries
	  #geom_polygon(data=sPDF, aes(long, lat, group=group), color="black", fill='grey75') +
	  geom_sf(data = sPDF, color="black", fill='grey75') +
	  # coord_sf(
	  #   ylim = c(-60, 90)
	  # ) +

    scale_x_continuous(expand = c(0,0), limits = c(lonmin,lonmax)) +
    scale_y_continuous(expand = c(0,0), limits = c(latmin,latmax)) +
	  labs( x = "", y = "") +

	  theme_bw() +
	  theme(axis.ticks.y.right = element_line(),
	        axis.ticks.x.top = element_line(),
	        panel.grid = element_blank())

  return(gg)
}
