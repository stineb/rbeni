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

  vec <- approx(vec, xout = 1:length(vec))$y

  # vec <- ifelse(
  #   sum(!is.na(vec))>1,
  #   approx(vec, xout = 1:length(vec))$y,
  #   NA
  # )

  return(vec)
}
