mycurve <- function( func, from, to, col='black', add=FALSE, lwd=1, lty=1 ){
  range_x <- seq( from, to, by=(to-from)/100 )
  range_y <- sapply( range_x, func )
  if (add){
    lines( range_x, range_y, type="l", col=col, lwd=lwd, lty=lty )
  } else {
    plot( range_x, range_y, type="l", col=col, lwd=lwd, lty=lty )
  }
}