#' Removes NA values from head and tail
#'
#' Removes all consecutive NA values starting from the head
#' and tail of a vector up to the first non-NA value.
#'
#' @param vec A vector of numeric values.
#'
#' @return A reduced vector, shortened after removing consecutive
#' NAs from the head and tail of the initial vector (argument \code{vec}).
#
#' @export
#'
#' @examples vec <- c(rep(NA, 5), seq(1,10), NA, seq(12,20), rep(NA,10))
#' cutna_headtail(vec)
cutna_headtail <- function( vec ){

  ## Remove (cut) NAs from the head and tail of a vector.
  ## Returns the indexes to be dropped from a vector

  ## remove NAs from head
  if (is.na(vec[1])){
    idx <- 0
    while ( idx < length(vec) ){
      idx <- idx + 1
      test <- head( vec, idx )
      if (any(!is.na(test))){
        ## first non-NA found at position idx
        cuthead <- idx - 1
        break
      }
    }
    vec <- vec[ -(1:cuthead) ]
    #idxs_head <- 1:cuthead
  } else {
    #idxs_head <- c()
  }


  ## remove NAs from tail
  if (is.na(vec[length(vec)])){
    idx <- 0
    while ( idx < length(vec) ){
      idx <- idx + 1
      test <- tail( vec, idx )
      if (any(!is.na(test))){
        ## first non-NA found at position idx, counting from tail
        cuttail <- idx - 1
        break
      }
    }
    vec <- vec[ -((length(vec)-cuttail+1):length(vec)) ]
    #idxs_tail <- (length(vec)-cuttail+1):length(vec)
  } else {
    #idxs_tail <- c()
  }

  #idxs <- c( idxs_head, idxs_tail )

  #return(idxs)
  return(vec)

}

