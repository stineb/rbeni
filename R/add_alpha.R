#' Add transparency to a named color
#'
#' This function converts a named color to RGB and adds an alpha (relative transparency) value.
#'
#' @param col A character string specifying the named color
#' @param alpha A numeric value specifying the transparency (0 for fully transparent)
#'
#' @return The output of a \code{rgb} call.
#
#' @export
#'
#' @examples transparent_red <- add_alpha("red", 0.3)
add_alpha <- function( col, alpha ){
  ## add alpha to color given as a name
  col    <- col2rgb( col, alpha=TRUE )/255
  col[4] <- alpha
  col    <- rgb(col[1,],col[2,],col[3,],col[4,])
  return( col )
}
