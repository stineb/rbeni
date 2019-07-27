#' Reduces the rows of a data frame based on the removal of NA values
#'
#' Reduces the rows of a data frame based on the removal of NA values from
#' head and tail of a given column. Considers all consecutive NA values
#' starting from the head and tail of a vector up to the first non-NA value.
#'
#' @param df A data frame
#' @param col A character string specifying the column based on which the
#' removal of consecutive NA values from head and tail is done.
#' @param extend A logical specifying whether NAs at the head or tail of a
#' data frame's column (\code{col}) should be filled with the first (last)
#' non-NA value of the respective column. Defaults to \code{FALSE}.
#'
#' @return A reduced data frame, shortened after removing consecutive
#' NAs from the head and tail of the column (argument \code{col}) of the
#' initial data frame (argument \code{df}).
#
#' @export
#'
#' @examples df <- data.frame( columnname = c(rep(NA, 5), seq(1,10), NA, seq(12,20), rep(NA,10)))
#' print(cutna_headtail_df(df, "columnname"))
cutna_headtail_df <- function( df, col, extend = FALSE ){

  ## Remove (cut) NAs from the head and tail of a vector.
  ## Returns the indexes to be dropped from a vector

  ## remove NAs from head
  vec <- unname(unlist(df[col]))
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
    #vec <- vec[ -(1:cuthead) ]
    idxs_head <- 1:cuthead
  } else {
    idxs_head <- c()
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
    #vec <- vec[ -((length(vec)-cuttail+1):length(vec)) ]
    idxs_tail <- (length(vec)-cuttail+1):length(vec)
  } else {
    idxs_tail <- c()
  }

  idxs <- c( idxs_head, idxs_tail )

  if (extend){
    vec_replace <- vec
    vec_replace[idxs_head] <- vec_replace[max(idxs_head)+1]
    vec_replace[idxs_tail] <- vec_replace[min(idxs_tail)-1]
    df[[col]] <- vec_replace
  } else {
    df <- dplyr::slice(df, -idxs)
  }

  #return(idxs)
  return(df)

}

