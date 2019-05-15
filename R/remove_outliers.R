#' Removes outliers
#'
#' Removes outliers based on inter-quartile range
#'
#' @param vec A vector of numeric values containing outliers.
#' @param coef A numeric value specifying the range of values not considered
#' outliers (interquartile range times \code{coef}).
#'
#' @export
#'
#' @examples
#'
remove_outliers <- function( vec, coef=1.5 ) {
  ## use the command boxplot.stats()$out which use the Tukey’s method to identify
  ## the outliers ranged above and below the <coef`>*IQR.
  outlier <- boxplot.stats( vec, coef=coef )$out
  vec[ which( is.element( vec, outlier ) ) ] <- NA
  return( vec )
}
