#' Analyse modelled values versus observed data.
#'
#' Calculates a set of performance statistics and optionally creates plots of modelled
#' versus observed values.
#'
#' @param df A data frame containing columns with names corresponding to arguments
#' \code{mod} and \code{obs}
#' @param mod A vector of numeric values representing modelled values.
#' @param obs A vector of numeric values representing observed values.
#' @param type If \code{"points"}, uses \code{geom_points()}, if \code{"hex"}
#' uses \code{ggplot2::geom_hex()}, if \code{"heat"} uses adjusted
#' \code{geom_points()} with color indicating density
#' @param filnam A character string specifying the name of the file containing
#' the plot.
# @param use_factor (Optional) A character string classifying points (optional)
#'
#' @export
#'
#' @examples
#'
analyse_modobs2 <- function(
  df,
  mod,
  obs,
  type       = "points",
  filnam     = NA,
  xlim       = NULL,
  ylim       = NULL,
  use_factor = NULL,
  ...
  ){

  require(ggplot2)
  require(dplyr)
  require(LSD)
  require(ggthemes)
  require(RColorBrewer)

  #if (identical(filnam, NA)) filnam <- "analyse_modobs.pdf"

  ## rename to 'mod' and 'obs' and remove rows with NA in mod or obs
  df <- df %>%
    ungroup() %>%
    dplyr::select(mod=mod, obs=obs) %>%
    tidyr::drop_na(mod, obs)

  ## get linear regression (coefficients)
  linmod <- lm( obs ~ mod, data=df )

  ## construct metrics table using the 'yardstick' library
  df_metrics <- df %>%
    yardstick::metrics(obs, mod) %>%
    dplyr::bind_rows( tibble( .metric = "n",        .estimator = "standard", .estimate = summarise(df, numb=n()) %>% unlist() ) ) %>%
    dplyr::bind_rows( tibble( .metric = "slope",    .estimator = "standard", .estimate = coef(linmod)[2]) ) %>%
    # dplyr::bind_rows( tibble( .metric = "nse",      .estimator = "standard", .estimate = hydroGOF::NSE( obs, mod, na.rm=TRUE ) ) ) %>%
    dplyr::bind_rows( tibble( .metric = "mean_obs", .estimator = "standard", .estimate = summarise(df, mean=mean(obs, na.rm=TRUE)) %>% unlist() ) ) %>%
    dplyr::bind_rows( tibble( .metric = "prmse",    .estimator = "standard",
                       .estimate = filter(., .metric=="rmse") %>% dplyr::select(.estimate) %>% unlist() /
                         filter(., .metric=="mean_obs") %>% dplyr::select(.estimate) %>% unlist() ) ) %>%
    dplyr::bind_rows( tibble( .metric = "pmae",    .estimator = "standard",
                       .estimate = filter(., .metric=="mae") %>% dplyr::select(.estimate) %>% unlist() /
                         filter(., .metric=="mean_obs") %>% dplyr::select(.estimate) %>% unlist() ) ) %>%
    dplyr::bind_rows( tibble( .metric = "bias",        .estimator = "standard", .estimate = summarise(df, mean((mod-obs), na.rm=TRUE    )) %>% unlist() ) ) %>%
    dplyr::bind_rows( tibble( .metric = "pbias",       .estimator = "standard", .estimate = summarise(df, mean((mod-obs)/obs, na.rm=TRUE)) %>% unlist() ) )

  rsq_lab <- format( df_metrics %>% filter(.metric=="rsq") %>% dplyr::select(.estimate) %>% unlist() %>% unname(), digits = 2 )
  rmse_lab <- format( df_metrics %>% filter(.metric=="rmse") %>% dplyr::select(.estimate) %>% unlist() %>% unname(), digits = 3 )
  mae_lab <- format( df_metrics %>% filter(.metric=="mae") %>% dplyr::select(.estimate) %>% unlist() %>% unname(), digits = 3 )
  bias_lab <- format( df_metrics %>% filter(.metric=="bias") %>% dplyr::select(.estimate) %>% unlist() %>% unname(), digits = 3 )
  slope_lab <- format( df_metrics %>% filter(.metric=="slope") %>% dplyr::select(.estimate) %>% unlist() %>% unname(), digits = 3 )
  n_lab <- format( df_metrics %>% filter(.metric=="n") %>% dplyr::select(.estimate) %>% unlist() %>% unname(), digits = 3 )

  if (type=="heat"){

    # ## Copied from http://auguga.blogspot.com/2015/10/r-heat-scatter-plot.html
    # df_tmp <- df %>% tidyr::drop_na()
    # dens <- MASS::kde2d(df_tmp$mod, df_tmp$obs)

    # # create a new data frame of that 2d density grid
    # # (needs checking that I haven't stuffed up the order here of z?)
    # gr <- with(dens, expand.grid(x,y)) %>%
    #   as_tibble() %>%
    #   setNames(c("xgr", "ygr")) %>%
    #   bind_cols(tibble(zgr = as.vector(dens$z)))

    # # Fit a model
    # mod <- loess(zgr ~ xgr * ygr, data=gr)

    # # Apply the model to the original data to estimate density at that point
    # df$pointdens <- predict(mod, newdata=dplyr::tibble(xgr=df$mod, ygr=df$obs))

    # # Draw plot
    # gg <- ggplot(df, aes(x=mod, y=obs, color=pointdens)) +
    #   theme_classic() +
    #   scale_colour_gradientn(colours = LSD::colorpalette('heat', 5)) +
    #   geom_point() +
    #   geom_smooth(method='lm', color="red", size=0.5) +
    #   geom_abline(intercept=0, slope=1, linetype="dotted") +
    #   coord_fixed() +
    #   xlim(0,NA) +
    #   theme_classic() +
    #   labs(subtitle = bquote( italic(R)^2 == .(rsq_lab) ~~~
    #       RMSE == .(rmse_lab) ~~~
    #       MAE == .(mae_lab) ~~~
    #       slope == .(slope_lab) ~~~
    #       italic(N) == .(n_lab) ),
    #     x=xlab, y=ylab)

    # if (!identical(filnam, NA)) pdf(filnam, width=6, height=6)

    # if (identical(xlim, NULL)) xlim <- c( min(range(df$mod, na.rm=TRUE)[1], range(df$obs, na.rm=TRUE)[1]), max(range(df$mod, na.rm=TRUE)[2], range(df$obs, na.rm=TRUE)[2]) )
    # if (identical(ylim, NULL)) ylim <- xlim

    # par(las=1)
    # with(df,
    #   LSD::heatscatter(
    #     mod,
    #     obs,
    #     xlim=xlim,
    #     ylim=ylim,
    #     main="",
    #     xlab=xlab,
    #     ylab=ylab,
    #     ggplot=TRUE
    #     ...
    #   ) )

    # abline( c(0,0), c(1,1), col="black", lty=3 )
    # abline( linmod, col="red", lty=1 )

    # ## left
    # mtext( bquote( italic(R)^2 == .(format( rsq_lab, digits = 3) ) ), side=3, line=1, cex=1.0, adj=0.0 )
    # mtext( paste( "RMSE =", format( rmse_lab, digits = 3 ) ), side=3, line=0, cex=1.0, adj=0.0 )

    # ## right
    # mtext( paste( "bias =",  format( bias_lab, digits = 3 ) ), side=3, line=2, cex=1.0, adj=1.0 )
    # mtext( paste( "slope =", format( slope_lab, digits = 3 ) ), side=3, line=1, cex=1.0, adj=1.0 )
    # mtext( bquote( italic(N) == .(format( n_lab, digits = 3) ) ), side=3, line=0, cex=1.0, adj=1.0 )

    # if (!identical(filnam, NA)) dev.off()
    source("~/LSD/R/LSD.heatscatter.R")
    gg <- heatscatter(
                  df$mod,
                  df$obs,
                  xlim=xlim,
                  ylim=ylim,
                  main="",
                  ggplot=TRUE )

    gg <- gg +
      geom_smooth(method='lm', color="red", size=0.5, se=FALSE) +
      geom_abline(intercept=0, slope=1, linetype="dotted") +
      theme_classic() +
      labs(
        subtitle = bquote( italic(R)^2 == .(rsq_lab) ~~
                                RMSE == .(rmse_lab) ~~
                                bias == .(bias_lab) ~~
                                slope == .(slope_lab) ~~
                                italic(N) == .(n_lab) ))

    if (!identical(filnam, NA)) {
      ggsave(filnam, width=5, height=5)
    } else {
      print(gg)
    }

  } else if (type=="hex"){

    ## ggplot hexbin
    gg <- df %>%
      ggplot2::ggplot(aes(x=mod, y=obs)) +
      geom_hex() +
      scale_fill_gradientn(
        colours = colorRampPalette( c("gray65", "navy", "red", "yellow"))(5)) +
      geom_smooth(method='lm', color="red", size=0.5) +
      geom_abline(intercept=0, slope=1, linetype="dotted") +
      # coord_fixed() +
      # xlim(0,NA) +
      # ylim(0,NA) +
      theme_classic() +
      labs(
        subtitle = bquote(
          italic(R)^2 == .(rsq_lab) ~~~
          RMSE == .(rmse_lab) ~~~
          bias == .(bias_lab) ~~~
          slope == .(slope_lab) ~~~
          italic(N) == .(n_lab) ))

    if (!identical(filnam, NA)) {
      ggsave(filnam, width=5, height=5)
    } else {
      print(gg)
    }

  } else if (type=="points") {

    ## points
    gg <- df %>%
      ggplot(aes(x=mod, y=obs)) +
      geom_point() +
      geom_smooth(method='lm', color="red", size=0.5) +
      geom_abline(intercept=0, slope=1, linetype="dotted") +
      # coord_fixed() +
      # xlim(0,NA) +
      # ylim(0,NA) +
      theme_classic() +
      labs(
        # subtitle = expression( paste( italic(R)^2, "\n",
        #   "beni")),
        subtitle = bquote( italic(R)^2 == .(rsq_lab) ~~
                                RMSE == .(rmse_lab) ~~
                                bias == .(bias_lab) ~~
                                slope == .(slope_lab) ~~
                                italic(N) == .(n_lab) ))

    if (!identical(filnam, NA)) {
      ggsave(filnam, width=5, height=5)
    } else {
      print(gg)
    }

  }

  return(list(df_metrics=df_metrics, gg=gg))
}
