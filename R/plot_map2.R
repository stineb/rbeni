#' Plot a nice map with ggplot.
#'
#' Returns a ggplot object for a global map plot.
#'
#' @param obj An object, either a \code{RasterBrick} (returned from a \code{raster::brick()} function call),
#' or a list returned from a \code{rbeni::read_nc_onefile()} function call.
#' @param nbin An integer specifying the number of bins used for the color key.
#' @param maxval A numeric value specifying the maximum value for which the color key is to be extended. Defaults
#' to \code{NA} (the 99\% quantile of values is used).
#' @param legend_title A character string specifying the legend title (annotation above the color key)
#' @param centered A boolean specifying whether a centered color scale should be used. Defaults to \code{FALSE}.
#' @param breaks A numeric vector specifying the breaks for the color scale. Defaults to \code{NA}, i.e. breaks
#' are determined automatically based on \code{nbin} and \code{maxval}.
#' @return A ggplot object for a global map plot.
#' @export
#'
plot_map2 <- function( obj, nbin = 10, maxval = NA, legend_title = "", centered = FALSE, breaks = NA ){

  require(rworldmap)
  require(raster)
  require(ggplot2)
  require(scales)

  # Get polygon of continent outline                                     -----
  data(coastsCoarse)
  sPDF <- getMap()[getMap()$ADMIN!='Antarctica',]

  ## convert into data frame with longitude (x) and latitude (y)
  if (class(obj)[[1]] == "RasterBrick"){
    ## convert object into data frame
    df_sp <- as(rasta, "SpatialPixelsDataFrame")
    df <- as.data.frame(df_sp)
    names(df) <- c("layer", "x", "y")

  } else if (is.element("vars", ls(obj)) && is.element("lat", ls(obj)) && is.element("lon", ls(obj))){
    ## is a rbeni-nc element
    df <- nc_to_df(obj) %>%
      rename(x=lon, y=lat)

  } else if (is.data.frame(obj)){
    ## is already a data frame. thanks.
    df <- as_tibble(obj)

  }

  ## map theme
  theme_map <- theme_grey() +    # theme_minimal()
    theme(

      plot.title = element_text(hjust = 0, face="bold", size = 18),

      legend.position = "right", # c(0.07, 0.35), #"left"
      legend.key.size = unit(c(5, 1), "mm"),
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

  a <- sapply( lat.labels, function(x) if (x>0) {bquote(.(x)*degree ~N)} else if (x==0) {bquote(.(x)*degree)} else {bquote(.(-x)*degree ~S)} )
  b <- sapply( lon.labels, function(x) if (x>0) {bquote(.(x)*degree ~E)} else if (x==0) {bquote(.(x)*degree)} else {bquote(.(-x)*degree ~W)})

  if (centered){
    vec_colors <- c( "royalblue4", "royalblue2", "wheat", "tomato2", "tomato4" )
  } else {
    vec_colors <- c( "wheat", "tomato2", "tomato4", "orchid4" )
  }

  if (is.na(breaks)){
    breaks <- seq(0, maxval, length.out = (nbin+1))
    if (is.na(maxval)) maxval <- quantile(df$layer, 0.99, na.rm = TRUE) %>% ceiling()
  } else {
    nbin <- length(breaks) - 1
    maxval <- max(breaks)
  }

  ## set up plot
  gg <- ggplot() +

    # background countries
    geom_polygon(data=sPDF, aes(long, lat, group=group),
                 color=NA, fill='grey95') +

    # Flux grid
    geom_tile(data=df, aes(x=x, y=y, fill=layer)) +

    # Coastline
    geom_path(data=coastsCoarse, aes(long, lat, group=group),
              color='black', size=0.1) +

    scale_fill_gradientn(colors = vec_colors, na.value = 'grey90',
                         breaks = breaks, limits = c(0,maxval), oob=scales::squish ) +

    scale_x_continuous(expand = c(0,0), limits = c(-180,180), breaks = lon.labels, labels = b) +
    scale_y_continuous(expand = c(0,0), limits = c(-65,85),   breaks = lat.labels, labels = a) +

    labs( x = "", y = "") +

    # Format colorbar
    guides(fill = guide_colorbar(nbin=nbin,
                                 raster=FALSE,
                                 barheight = 15,
                                 barwidth=1,
                                 frame.colour=c("black"),
                                 frame.linewidth=0.2,
                                 ticks = FALSE,
                                 ticks.colour="black",
                                 ticks.linewidth = 0.15,
                                 direction="vertical",
                                 title = legend_title
                                 # label.vjust = 1.6
                                 )) +
    theme_map

  return(gg)
}



