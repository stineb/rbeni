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

  # download global coastline data from naturalearth
  countries <- rnaturalearth::ne_countries(scale = 110, returnclass = "sf")

  ##---------------------------------------------
  ## Projection
  ##---------------------------------------------
  # set coordinate systems
  robinson <- CRS("+proj=robin +over")

  # create a bounding box for the robinson projection
  bb <- sf::st_union(sf::st_make_grid(
    st_bbox(c(xmin = -180,
              xmax = 180,
              ymax = 90,
              ymin = -90),
            crs = st_crs(4326)),
    n = 100))
  bb_robinson <- st_transform(bb, as.character(robinson))

	# # clip countries to bounding box
	# # and transform
	# countries <- countries %>%
	#   st_buffer(0) %>%
	#   st_intersection(st_union(bb)) %>%
	#   st_transform(robinson)

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

	##---------------------------------------------
	## Create ggplot object
	##---------------------------------------------
	gg <- ggplot() +

		# background countries
	  # geom_sf(data = countries, color="black", fill='grey75', size = 0.1) +
	  geom_sf(data = countries,
	          colour = 'black',
	          fill = 'grey75',  # NA for empty
	          linetype = 'solid',
	          size = 0.1) +

	  # bounding box
	  geom_sf(data = bb_robinson,
	          colour = 'black',
	          linetype = 'solid',
	          fill = NA,
	          size = 0.1)

	  # coord_sf(
	  #   ylim = c(-60, 90)
	  # ) +

#     scale_x_continuous(expand = c(0, 0), limits = c(lonmin, lonmax)) +
#     scale_y_continuous(expand = c(0, 0), limits = c(latmin, latmax)) +
# 	  labs( x = "", y = "") +

	  # theme_bw()
	  # theme(axis.ticks.y.right = element_line(),
	  #       axis.ticks.x.top = element_line(),
	  #       panel.grid = element_blank())

  return(gg)
}
