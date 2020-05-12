#' Interpolate without much fuss.
#'
#' @param vec A vector of numeric values
#'
#' @return A vector of numeric values
#
#' @export
#'
#' @examples vec <- c(seq(1,10), NA, seq(12,20))
#' print(myapprox(vec))
myapprox <- function(vec){

  x <- 1:length(vec)
  vec <- approx(x, vec, xout = x)$y

  return(vec)
}
