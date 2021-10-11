#' Plot a nice map with ggplot.
#'
#' Returns a cowplot object for a global map plot.
#'
#' @param obj An object, either a \code{RasterBrick} (returned from a \code{raster::brick()} function call),
#' or a list returned from a \code{rbeni::read_nc_onefile()} function call.
#' @param nbin An integer specifying the number of bins used for the color key.
#' @param maxval A numeric value specifying the maximum value for which the color key is to be extended. Defaults
#' to \code{NA} (the 99\% quantile of values is used).
#' @param breaks A numeric vector specifying the breaks for the color scale. Defaults to \code{NA}, i.e. breaks
#' are determined automatically based on \code{nbin} and \code{maxval}.
#' @param lonmin Left edge (longitude, in degrees), defaults to -180.
#' @param lonmax Right edge (longitude, in degrees), defaults to 180.
#' @param latmin Lower edge (latitude, in degrees), defaults to -90.
#' @param latmax Upper edge (latitude, in degrees), defaults to 90.
#' @param plot_title A character string specifying the plot title
#' @param plot_subtitle A character string specifying the plot subtitle
#' @param legend_title A character string specifying the legend title (annotation above the color key)
#' @param colorscale Either function that returns a set of colors or a vector of color names from which to interpolate.
#' Defaults to \code{virids::viridis}.
#' @param do_reproj A boolean specifying whether to re-project the map to Robin projection
#' @param combine A boolean specifying whether the map and the colorscale should be combined using cowplot.
#' Defaults to \code{TRUE}. If \code{FALSE}, a list of elements are retruned, where elements are the ggplot2 plot object
#' and the coloscale object returned by the call to \link{plot_discrete_cbar}.
#' @param varnam If \code{obj} is a rbeni-nc object (returned by \code{read_nc_onefile()}), \code{varnam} must be
#' provided (a character string specifying the variable name in \code{obj$vars[[varnam]]}).
#'
#' @return A ggplot object for a global map plot.
#' @export
#'
plot_map3 <- function(obj, maxval = NA, breaks = NA, lonmin = -180, lonmax = 180, latmin = -90, latmax = 90,
                      nbin = 10, legend_title = waiver(), colorscale = viridis::viridis, do_reproj = FALSE,
											plot_title = waiver(), plot_subtitle = waiver(), combine = TRUE, varnam = NULL, ...){

	library(rworldmap)
	library(cowplot)
  library(ggplot2)
  library(raster)
	data(coastsCoarse) # from rworldmap package

	sPDF <- getMap()[getMap()$ADMIN!='Antarctica',]

	##---------------------------------------------
	## interpret object
	##---------------------------------------------
	if (identical(class(obj), "character")){

		## read as raster brick
		rasta <- raster::brick(obj)

		##---------------------------------------------
		## re-project
		##---------------------------------------------
		# declare incoming CSR (should be done wayyyyyyy earlier than this)
		if (do_reproj){
		  crs(rasta) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
		  rasta_reproj <- projectRaster(rasta, crs=CRS("+proj=robin"))
		} else {
		  rasta_reproj <- rasta
		}

		##---------------------------------------------
		## convert to data frame for ggplot
		##---------------------------------------------
		tstep <- 1
		df <- as(rasta_reproj[[tstep]], "SpatialPixelsDataFrame")
		df <- as.data.frame(df)
		names(df) <- c("layer", "x", "y")


	} else if (identical(class(obj), "matrix")){

		## Complement info of matrix
    if (length(dim(obj))==2){

      if (dim(obj)[1] == 720 && dim(obj)[2]==360){
        grid <- "halfdeg"
      } else if (dim(obj)[1] == 360 && dim(obj)[2]==720){
        obj <- t(obj)
        grid <- "halfdeg"
      } else if (dim(obj)[1] == 360 && dim(obj)[2]==180){
        grid <- "1x1deg"
      } else if (dim(obj)[1] == 180 && dim(obj)[2]==360){
        obj <- t(obj)
        grid <- "1x1deg"
      }

      ## obj is a 2D matrix (grid)
      if (grid=="halfdeg"){
        lon <- seq(from = -179.75, to = 179.75, by = 0.5)
        lat <- seq(from = -89.75, to = 89.75, by = 0.5)
      } else if (grid=="1x1deg"){
        lon <- seq(from = -179.75, to = 179.75, by = 1)
        lat <- seq(from = -89.75, to = 89.75, by = 1)
      } else {
        rlang::warn("Dimensions not identified")
        lon <- seq(dim(obj)[1])
        lat <- seq(dim(obj)[2])
      }
      df <- grid_to_df(obj, grid = grid, dropna = FALSE) %>%
        setNames(c("x", "y", "layer"))

    } else {
      rlang::abort("Aborted. Argument obj is a matrix but does not have two dimensions.")
    }

  } else if (identical(class(obj)[[1]], "RasterBrick")){

    ## convert into data frame with longitude (x) and latitude (y)
    ## convert object into data frame
    df <- as(obj, "SpatialPixelsDataFrame")
    df <- as.data.frame(df)
    names(df) <- c("layer", "x", "y")

  } else if (is.element("vars", ls(obj)) && is.element("lat", ls(obj)) && is.element("lon", ls(obj))){

    ## is a rbeni-nc element
    if (is.null(varnam)) rlang::abort("Error: provide the variable name to be plotted as argument varnam.")
    df <- nc_to_df(obj, varnam = varnam) %>%
      dplyr::rename(x=lon, y=lat)

  } else if (is.data.frame(obj)){

    ## is already a data frame. thanks.
    df <- as_tibble(obj) %>%
      dplyr::filter(lon > lonmin & lon < lonmax & lat > latmin & lat < latmax) %>%
      rename(x=lon, y=lat)
      # dplyr::select(x, y, !!varnam) %>%
      # setNames(c("x", "y", "layer"))
  }

	## of more than one variable is available, make varnam a required argument
	if (is.null(varnam)){
	  varnam <- names(df %>% dplyr::select(-x, -y))
	  if (length(varnam) > 1){
	    rlang::abort(paste("Aborting. Argument varnam not provided and more than one variable available. Which one to take?"))
	  }
	}

	## reduce and rename	
	df <- df %>% 
	  dplyr::select(x, y, !!varnam) %>%
	  setNames(c("x", "y", "layer"))
	  

	##---------------------------------------------
	## Bin data
	##---------------------------------------------
	toptriangle <- FALSE
	bottomtriangle <- FALSE

	if (identical(NA, breaks)){
	  breaks <- scales::pretty_breaks(n = nbin)(df$layer)
	  rlang::warn("Overwriting nbin after defining breaks with scales::pretty_breaks().")
	  nbin <- length(breaks) - 1
	} else {
	  nbin <- length(breaks) - 1
	}

	breaks_with <- breaks

	if (is.infinite(breaks[length(breaks)])){
	  toptriangle <- TRUE
	  breaks <- breaks[-(length(breaks)-1)]
	}
	if (is.infinite(breaks[1])){
	  bottomtriangle <- TRUE
	  breaks <- breaks[-2]
	}

	## update
	nbin <- length(breaks) - 1
	
	## add dummy rows to make sure values in layer span the entire range
	df <- df %>% 
	  bind_rows(
	    tibble(
	      x = NA,
	      y = NA,
	      layer = breaks[1:(length(breaks)-1)] + 0.5 * (breaks[2]-breaks[1])
	    )
	  )

	## bin data
	df$layercut <- as.factor(base::cut(df$layer, breaks=breaks, labels = FALSE, include.lowest = TRUE))

	# # remove symbols from category-strings to make them nicer in the legend
	# df$layercut <- gsub("\\(|\\]", "", df$layercut)
	# df$layercut <- gsub("\\,", " - ", df$layercut)

	##---------------------------------------------
	## Create color scale
	##---------------------------------------------
	if (class(colorscale)=="function"){

	  colorscale <- colorscale(nbin, direction = -1)

	} else if (class(colorscale)=="character"){

	  colorscale <- colorRampPalette( colorscale )( nbin )

	} else if (class(colorscale)=="palette"){

	  ## nothing to do in this case
	  #colorscale <- colorscale

	} else {

	  rlang::abort("colorscale could not be set.")

	}


	if (toptriangle){
	  colorscale <- c(colorscale, colorscale[length(colorscale)])
	}
	if (bottomtriangle){
	  colorscale <- c(colorscale[1], colorscale)
	}

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
	dloncoarse <- (lonmax - lonmin)/6
	dlonfine   <- (lonmax - lonmin)/18
	dlatcoarse <- (latmax - latmin)/6
	dlatfine   <- (latmax - latmin)/18
	lat.labels <- seq(latmin, latmax, dlatcoarse)
	lat.short  <- seq(latmin, latmax, dlatfine)
	lon.labels <- seq(lonmin, lonmax, dloncoarse)
	lon.short  <- seq(lonmin, lonmax, dlonfine)

	# a <- sapply( lat.labels, function(x) if (x>0) {bquote(.(x)*degree ~N)} else if (x==0) {bquote(.(x)*degree)} else {bquote(.(-x)*degree ~S)} )
	# b <- sapply( lon.labels, function(x) if (x>0) {bquote(.(x)*degree ~E)} else if (x==0) {bquote(.(x)*degree)} else {bquote(.(-x)*degree ~W)})

	a <- sapply( lat.labels, function(x) if (x>0) {parse(text = paste0(x, "*degree ~ N"))} else if (x==0) {parse(text = paste0(x, "*degree"))} else {parse(text = paste0(-x, "*degree ~ S"))} )
	b <- sapply( lon.labels, function(x) if (x>0) {parse(text = paste0(x, "*degree ~ E"))} else if (x==0) {parse(text = paste0(x, "*degree"))} else {parse(text = paste0(-x, "*degree ~ W"))} )
	

	##---------------------------------------------
	## Create ggplot object
	##---------------------------------------------
	ggmap <- ggplot() +

		# background countries
	  geom_polygon(data = sPDF, aes(long, lat, group = group), color=NA, fill='grey95') +

		# rasta_reproj grid
		geom_tile(data=df, aes(x = x, y = y, fill = layercut, color = layercut), show.legend = FALSE) +

		# Coastline
		geom_path(data=coastsCoarse, aes(long, lat, group = group), color='black') +

    scale_fill_manual(values=colorscale) +
    scale_color_manual(values=colorscale) +

    scale_x_continuous(expand = c(0, 0), limits = c(-lonmin, lonmax), breaks = lon.labels, labels = b) +
    scale_y_continuous(expand = c(0, 0), limits = c(-latmin, latmax), breaks = lat.labels, labels = a) +

	  labs( x = "", y = "", title = plot_title, subtitle = plot_subtitle) +

	  xlim(lonmin, lonmax) +
	  ylim(latmin, latmax)


	gglegend <- plot_discrete_cbar(
		breaks           = breaks_with, # Vector of breaks. If +-Inf are used, triangles will be added to the sides of the color bar
		colors           = colorscale,
		legend_title     = legend_title,
		legend_direction = "vertical",
		...
    )

	if (combine){
	  out <- cowplot::plot_grid(ggmap, gglegend, ncol = 2, rel_widths = c(1, 0.2))
	} else {
	  out <- list(ggmap = ggmap, gglegend = gglegend)
	}

  return(out)
}


## Copied from https://github.com/adrfantini/plot_discrete_cbar

plot_discrete_cbar = function(
    breaks, # Vector of breaks. If +-Inf are used, triangles will be added to the sides of the color bar
    palette = "Greys", # RColorBrewer palette to use
    colors = RColorBrewer::brewer.pal(length(breaks) - 1, palette), # Alternatively, manually set colors
    direction = 1, # Flip colors? Can be 1 or -1
    spacing = "natural", # Spacing between labels. Can be "natural" or "constant"
    border_color = NA, # NA = no border color
    legend_title = NULL,
    legend_direction = "horizontal", # Can be "horizontal" or "vertical"
    font_size = 3.5,
    expand_size = 0, # Controls spacing around legend plot
    expand_size_y = 0.8,
    spacing_scaling = 0.3, # Multiplicative factor for label and legend title spacing
    width = 0.01, # Thickness of color bar
    triangle_size = 0.05 # Relative width of +-Inf triangles
    ) {

    require(ggplot2)

    if (!(spacing %in% c("natural", "constant"))) stop("spacing must be either 'natural' or 'constant'")
    if (!(direction %in% c(1, -1))) stop("direction must be either 1 or -1")
    if (!(legend_direction %in% c("horizontal", "vertical"))) stop("legend_direction must be either 'horizontal' or 'vertical'")
    breaks = as.numeric(breaks)
    new_breaks = sort(unique(breaks))
    if (any(new_breaks != breaks)) warning("Wrong order or duplicated breaks")
    breaks = new_breaks
    if (class(colors) == "function") colors = colors(length(breaks) - 1)
    if (length(colors) != length(breaks) - 1) stop("Number of colors (", length(colors), ") must be equal to number of breaks (", length(breaks), ") minus 1")
    if (!missing(colors)) warning("Ignoring RColorBrewer palette '", palette, "', since colors were passed manually")

    if (direction == -1) colors = rev(colors)

    inf_breaks = which(is.infinite(breaks))
    if (length(inf_breaks) != 0) breaks = breaks[-inf_breaks]
    plotcolors = colors

    n_breaks = length(breaks)

    labels = breaks

    if (spacing == "constant") {
        breaks = 1:n_breaks
    }

    r_breaks = range(breaks)
    d_breaks = breaks[2] - breaks[1]

    cbar_df = data.frame(stringsAsFactors = FALSE,
        y = breaks,
        yend = c(breaks[-1], NA),
        color = as.character(1:n_breaks)
    )[-n_breaks,]

    xmin = 1 - width/2
    xmax = 1 + width/2

    cbar_plot = ggplot(
      cbar_df,
      aes(xmin=xmin, xmax = xmax, ymin = y, ymax = yend, fill = factor(color, levels = 1:length(colors)))
      ) +
      geom_rect(show.legend = FALSE, color=border_color)

    ## Add arrows
    if (any(inf_breaks == 1)) { # Add < arrow for -Inf
        firstv = breaks[1]
        polystart = data.frame(
            x = c(xmin, xmax, 1),
            y = c(rep(firstv, 2), firstv - diff(r_breaks) * triangle_size)
        )
        plotcolors = plotcolors[-1]
        cbar_plot <- cbar_plot +
            geom_polygon(data=polystart, aes(x=x, y=y),
                        show.legend = FALSE,
                        inherit.aes = FALSE,
                        fill = colors[1],
                        color=border_color)
    }
    if (any(inf_breaks > 1)) { # Add > arrow for +Inf
        lastv = breaks[n_breaks]
        polyend = data.frame(
            x = c(xmin, xmax, 1),
            y = c(rep(lastv, 2), lastv + diff(r_breaks) * triangle_size)
        )
        plotcolors = plotcolors[-length(plotcolors)]
        cbar_plot <- cbar_plot +
            geom_polygon(data=polyend, aes(x=x, y=y),
                        show.legend = FALSE,
                        inherit.aes = FALSE,
                        fill = colors[length(colors)],
                        color=border_color)
    }

    if (legend_direction == "horizontal") {
        #horizontal legend
        mul = 1
        x = xmin
        xend = xmax
        cbar_plot <- cbar_plot + coord_flip()
        angle = 0
        legend_position = xmax + 0.1 * spacing_scaling

    } else {
        # vertical legend
        mul = -1
        x = xmax
        xend = xmin
        angle = -90
        legend_position = xmin # xmax + 0.2 * spacing_scaling
    }

    ymid <- (breaks[length(breaks)] + breaks[1]) / 2
    dy <- breaks[length(breaks)] - breaks[1]
    ybottom_abs <- ymid - dy/2 * 1/expand_size_y
    ytop_abs    <- ymid + dy/2 * 1/expand_size_y

    # Create color key
    cbar_plot <- cbar_plot +
        geom_segment(data = data.frame(y = breaks, yend = breaks),
            aes(y=y, yend=yend),
            x = x - 0.01 * mul * spacing_scaling, xend = x, #+ 0.01 * mul * spacing_scaling, # xend = xend,
            inherit.aes = FALSE) +
        annotate(geom = 'text', x = x - 0.02 * mul * spacing_scaling, y = breaks,
                label = labels,
                size = font_size,
                hjust = 0) +
        # scale_x_continuous(expand = c(expand_size,expand_size)) +
        scale_fill_manual(values=plotcolors) +
        theme_void() +
        expand_limits(y = c(ybottom_abs, ytop_abs), x = c(xend, x - 0.1 * mul * spacing_scaling))

    # Add legend title
    if (!is.null(legend_title)) {
        cbar_plot <- cbar_plot +
            annotate(
              geom = 'text',
              x = legend_position,
              # y = mean(r_breaks),
              y = max(r_breaks) + d_breaks * 1.5,
              label = legend_title,
              # angle = angle,
              angle = 0,
              size = font_size,
              fontface = 1,
              hjust = 0
              )
    }

    return(cbar_plot)
}

