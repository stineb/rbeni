#' Empty global map
#'
#' Returns a ggplot object for an empty global map
#'
#' @param lonmin Left edge (longitude, in degrees), defaults to -180.
#' @param lonmax Right edge (longitude, in degrees), defaults to 180.
#' @param latmin Lower edge (latitude, in degrees), defaults to -90.
#' @param latmax Upper edge (latitude, in degrees), defaults to 90.
#' @param dir_ne A character string specifying where to download Naturalearth layers. Once downloaded, they can be quickly loaded. Defaults to \code{"~/data/naturalearth/"}.
#' @return A ggplot object for a global map plot.
#' @export
#'
plot_map_simpl <- function(lonmin = -180,
                           lonmax = 180,
                           latmin = -60,
                           latmax = 85,
                           dir_ne = "~/data/naturalearth/"){

  require(ggplot2)
  require(sf)

  # ## define domain object
  # domain <- c(lonmin, lonmax, latmin, latmax)

  ## read 110 m resolution coastline from NaturalEarth data (is a shapefile)
  coast <- rnaturalearth::ne_coastline(scale = 110, returnclass = "sf")

  # download oceans
  scale <- 110
  if (length(list.files(path = dir_ne,
                        pattern = paste0("ne_", as.character(scale), "m_ocean"))
  ) == 0){
    layer_ocean <- rnaturalearth::ne_download(
      scale = scale,
      type = "ocean",
      category = "physical",
      returnclass = "sf",
      destdir = dir_ne
    )
  } else {
    layer_ocean <- rnaturalearth::ne_load(
      scale = scale,
      type = "ocean",
      category = "physical",
      returnclass = "sf",
      destdir = dir_ne
    )
  }

  # # download global coastline data from naturalearth
  # countries <- rnaturalearth::ne_countries(scale = 110, returnclass = "sf")

  # ##---------------------------------------------
  # ## Projection
  # ##---------------------------------------------
  # # set coordinate systems
  # robinson <- CRS("+proj=robin +over")
  #
  # # create a bounding box for the robinson projection
  # bb <- sf::st_union(sf::st_make_grid(
  #   st_bbox(c(xmin = -180,
  #             xmax = 180,
  #             ymax = 90,
  #             ymin = -90),
  #           crs = st_crs(4326)),
  #   n = 100))
  # bb_robinson <- st_transform(bb, as.character(robinson))

	# # clip countries to bounding box
	# # and transform
	# countries <- countries %>%
	#   st_buffer(0) %>%
	#   st_intersection(st_union(bb)) %>%
	#   st_transform(robinson)

  ##---------------------------------------------
  ## Create ggplot object
  ##---------------------------------------------
  gg <- ggplot2::ggplot() +

    # plot ocean
    geom_sf(data = layer_ocean,
            color = NA,
            fill = "white") +

    # plot coastline
    geom_sf(data = coast,
            colour = 'black',
            size = 0.1) +

    # set extent in longitude and latitude
    coord_sf(xlim = c(lonmin, lonmax),
             ylim = c(latmin, latmax),
             expand = FALSE   # to draw map strictly bounded by the specified extent
    ) +

    # some layout modifications
    xlab('') +
    ylab('') +
    theme_bw() +
    theme(axis.ticks.y.right = element_line(),
          axis.ticks.x.top = element_line(),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "grey70"),
          plot.background = element_rect(fill = "white")
    )

  return(gg)
}
