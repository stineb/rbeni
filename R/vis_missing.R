#' Visualise missing values
#'
#' Creates a ggplot object of a raster plot that visualises missing values in a data frame.
#' In the plot, variables are organised by rows.
#'
#' @param df A data frame
#'
#' @return A ggplot object
#' @export
#'
vis_missing <- function(df){

  require(reshape2)

  gg <- df %>%
    is.na() %>%
    reshape2::melt() %>%
    ggplot(aes(Var2, Var1, fill=value)) +
    geom_raster() +
    coord_flip() +
    scale_y_continuous(NULL, expand = c(0, 0)) +
    scale_fill_grey(name = "",
                    labels = c("Present",
                               "Missing")) +
    xlab("Observation") +
    theme(axis.text.y  = element_text(size = 100/ncol(df)))

  return(gg)
}


