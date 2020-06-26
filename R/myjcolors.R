#' Colors for a nice palette
#'
#' Returns colors from the \code{jcolors::jocolors("default")} palette,
#' complemented by \code{c("darkgreen", "navyblue", "darkgoldenrod", "orchid")}
#'
#' @param ncolors An integer specifying the number of colors to return.
#' Retunrs the first \code{ncolors} colors of the palette.
#'
#' @return A named vector of characters specifying the hex codes of the colors
#' @export
#'
myjcolors <- function(ncolors){

  jcols <- jcolors::jcolors("default")

  ## add
  out <- c(jcols, gplots::col2hex("darkgreen"), gplots::col2hex("navyblue"), gplots::col2hex("darkgoldenrod"), gplots::col2hex("orchid") )
  names(out) <- c(names(jcols), "darkgreen", "navyblue", "darkgoldenrod", "orchid")

  out <- out[1:ncolors]
  return(out)
}
