#' Analyse modelled values versus observed data.
#'
#' Calculates a set of performance statistics and optionally creates plots of modelled
#' versus observed values.
#'
#' @param df A data frame containing columns with names corresponding to arguments
#' \code{mod} and \code{obs}
#' @param mod A character string specifying the variable name (column) of the
#' modelled (simulated) values in data frame \code{df}.
#' @param obs A character string specifying the variable name (column) of the
#' observed values in data frame \code{df}.
#' @param type If \code{"points"}, uses \code{geom_points()}, if \code{"hex"}
#' uses \code{ggplot2::geom_hex()}, if \code{"heat"} uses adjusted
#' \code{geom_points()} with color indicating density, if \code{"density"} uses
#' \code{stat_density_2d()} to draws polygos of equal density.
#' @param filnam A character string specifying the name of the file containing
#' the plot. Defaults to \code{NA} (no file is created).
#' @param relative A logical specifying whether the relative RMSE and bias (after
#' division by the mean) is to be showed in the subtitle labels.
#' @param shortsubtitle A boolean specifying whether to display only R2, and in
#' the subtitle metrics.
#' @param plot_subtitle A boolean specifying whether to display any metrics. Defaults
#' to \code{TRUE}.
#' @param plot_linmod A boolean specifying whether to display the fitted linear
#' regression as a red line. Defaults to \code{TRUE}.
#' @param label A boolean specifying whether points should be labelled using ggrepel.
#' Defaults to \code{FALSE}. Only available for \code{type = "points}. Use argument
#' \code{nlabels} to specify how many points should be labelled, starting with points
#' that have the largest residuals from the linear regression fit.
#' @param id A character string specifying the column name that identifies the points.
#' The column's values must be of type integer and is used to label points in case of
#' \code{label = TRUE}.
#' @param nlabels An integer specifying how many points to be labelled, starting with points
#' that have the largest residuals from the linear regression fit. Only available
#' for \code{type = "points}. Defaults to one.
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
  relative   = FALSE,
  xlim       = NULL,
  ylim       = NULL,
  use_factor = NULL,
  shortsubtitle = FALSE,
  plot_subtitle = TRUE,
  plot_linmod = TRUE,
  label       = FALSE,
  id          = NULL,
  nlabels     = 1,
  ...
  ){

  require(ggplot2)
  require(dplyr)
  require(LSD)
  require(ggthemes)
  require(RColorBrewer)

  #if (identical(filnam, NA)) filnam <- "analyse_modobs.pdf"

  ## rename to 'mod' and 'obs' and remove rows with NA in mod or obs
  if (label){
    df <- df %>%
      as_tibble() %>%
      ungroup() %>%
      dplyr::select(mod=mod, obs=obs, id=!!id) %>%
      tidyr::drop_na(mod, obs)

  } else {
    df <- df %>%
      as_tibble() %>%
      ungroup() %>%
      dplyr::select(mod=mod, obs=obs) %>%
      tidyr::drop_na(mod, obs)

  }

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
                       .estimate = dplyr::filter(., .metric=="rmse") %>% dplyr::select(.estimate) %>% unlist() /
                         dplyr::filter(., .metric=="mean_obs") %>% dplyr::select(.estimate) %>% unlist() ) ) %>%
    dplyr::bind_rows( tibble( .metric = "pmae",    .estimator = "standard",
                       .estimate = dplyr::filter(., .metric=="mae") %>% dplyr::select(.estimate) %>% unlist() /
                         dplyr::filter(., .metric=="mean_obs") %>% dplyr::select(.estimate) %>% unlist() ) ) %>%
    dplyr::bind_rows( tibble( .metric = "bias",        .estimator = "standard", .estimate = dplyr::summarise(df, mean((mod-obs), na.rm=TRUE    )) %>% unlist() ) ) %>%
    dplyr::bind_rows( tibble( .metric = "pbias",       .estimator = "standard", .estimate = dplyr::summarise(df, mean((mod-obs)/obs, na.rm=TRUE)) %>% unlist() ) ) %>%
    dplyr::bind_rows( tibble( .metric = "cor",         .estimator = "standard", .estimate = cor(df$mod, df$obs) ) ) %>% 
    dplyr::bind_rows( tibble( .metric = "cor_test",    .estimator = "standard", .estimate = cor.test(df$mod, df$obs)$p.value ) )
    
  rsq_val <- df_metrics %>% dplyr::filter(.metric=="rsq") %>% dplyr::select(.estimate) %>% unlist() %>% unname()
  rmse_val <- df_metrics %>% dplyr::filter(.metric=="rmse") %>% dplyr::select(.estimate) %>% unlist() %>% unname()
  mae_val <- df_metrics %>% dplyr::filter(.metric=="mae") %>% dplyr::select(.estimate) %>% unlist() %>% unname()
  bias_val <- df_metrics %>% dplyr::filter(.metric=="bias") %>% dplyr::select(.estimate) %>% unlist() %>% unname()
  slope_val <- df_metrics %>% dplyr::filter(.metric=="slope") %>% dplyr::select(.estimate) %>% unlist() %>% unname()
  n_val <- df_metrics %>% dplyr::filter(.metric=="n") %>% dplyr::select(.estimate) %>% unlist() %>% unname()

  if (relative){
    rmse_val <- rmse_val / mean(df$obs, na.rm = TRUE)
    bias_val <- bias_val / mean(df$obs, na.rm = TRUE)
  }

  rsq_lab <- format( rsq_val, digits = 2 )
  rmse_lab <- format( rmse_val, digits = 3 )
  mae_lab <- format( mae_val, digits = 3 )
  bias_lab <- format( bias_val, digits = 3 )
  slope_lab <- format( slope_val, digits = 3 )
  n_lab <- format( n_val, digits = 3 )

  results <- tibble( rsq = rsq_val, rmse = rmse_val, mae = mae_val, bias = bias_val, slope = slope_val, n = n_val )

  if (shortsubtitle){
    subtitle <- bquote( italic(R)^2 == .(rsq_lab) ~~
                          RMSE == .(rmse_lab) )
  } else {
    subtitle <- bquote( italic(R)^2 == .(rsq_lab) ~~
                          RMSE == .(rmse_lab) ~~
                          bias == .(bias_lab) ~~
                          slope == .(slope_lab) ~~
                          italic(N) == .(n_lab) )
  }

  if (type=="heat"){

    # if (!identical(filnam, NA)) dev.off()
    # source("~/LSD/R/LSD.heatscatter.R")

    gg <- heatscatter(
                  df$mod,
                  df$obs,
                  xlim=xlim,
                  ylim=ylim,
                  main="",
                  ggplot=TRUE )

    gg <- gg +
      geom_abline(intercept=0, slope=1, linetype="dotted") +
      theme_classic() +
      labs(x = mod, y = obs)

    if (plot_linmod) gg <- gg + geom_smooth(method='lm', color="red", size=0.5, se=FALSE)
    if (plot_subtitle) gg <- gg + labs(subtitle = subtitle)

    if (!identical(filnam, NA)) {
      ggsave(filnam, width=5, height=5)
    }

  } else if (type=="hex"){

    ## ggplot hexbin
    gg <- df %>%
      ggplot2::ggplot(aes(x=mod, y=obs)) +
      geom_hex(bins = 100) +
      scale_fill_gradientn(
        colours = colorRampPalette( c("gray65", "navy", "red", "yellow"))(5), 
        trans = "log") +
      geom_abline(intercept=0, slope=1, linetype="dotted") +
      # coord_fixed() +
      # xlim(0,NA) +
      # ylim(0,NA) +
      theme_classic() +
      labs(x = mod, y = obs)

    if (plot_subtitle) gg <- gg + labs(subtitle = subtitle)
    if (plot_linmod) gg <- gg + geom_smooth(method='lm', color="red", size=0.5, se=FALSE)

    if (!identical(filnam, NA)) {
      ggsave(filnam, width=5, height=5)
    }

  } else if (type=="points") {

    if (label){
      df <- df %>%
        dplyr::mutate(.res = mod - obs) %>%
        dplyr::mutate(.absres = abs(.res)) %>%
        dplyr::arrange(desc(.absres)) %>%
        dplyr::mutate(.rankres = 1:n()) %>%
        dplyr::mutate(.dolab = ifelse(.rankres <= nlabels, TRUE, FALSE))

      ## points with labels
      library(ggrepel)
      gg <- df %>%
        ggplot(aes(x=mod, y=obs, label = ifelse(.dolab, id, ""))) +
        geom_point() +
        geom_label_repel(min.segment.length = 0, seed = 42, box.padding = 0.5) +
        geom_point(data = dplyr::filter(df, .dolab), color = "red") +
        geom_abline(intercept=0, slope=1, linetype="dotted") +
        theme_classic() +
        labs(x = mod, y = obs)

    } else {
      ## points
      gg <- df %>%
        ggplot(aes(x=mod, y=obs)) +
        geom_point() +
        geom_abline(intercept=0, slope=1, linetype="dotted") +
        theme_classic() +
        labs(x = mod, y = obs)

    }


    if (plot_subtitle) gg <- gg + labs(subtitle = subtitle)
    if (plot_linmod) gg <- gg + geom_smooth(method='lm', color="red", size=0.5, se=FALSE)

    if (!identical(filnam, NA)) {
      ggsave(filnam, width=5, height=5)
    }

  } else if (type=="density") {

    ## density as raster
    gg <- df %>%
      ggplot(aes(x=mod, y=obs)) +

      stat_density_2d(
        geom = "raster", #the geometric object to display the data (in this case: rectangles)
        aes(fill = after_stat(density)), #using `density`, a variable calculated by the stat
        contour = FALSE
      ) +

      scale_fill_gradientn(colours = colorRampPalette( c("white", "gray65", "navy", "red", "yellow"))(6),
                           guide = FALSE) +

      geom_abline(intercept=0, slope=1, linetype="dotted") +
      # coord_fixed() +
      # xlim(0,NA) +
      # ylim(0,NA) +
      theme_classic() +
      labs(x = mod, y = obs)

    if (plot_subtitle) gg <- gg + labs(subtitle = subtitle)
    if (plot_linmod) gg <- gg + geom_smooth(method='lm', color="red", size=0.5, se=FALSE)

    if (!identical(filnam, NA)) {
      ggsave(filnam, width=5, height=5)
    }

  }

  return(list(df_metrics=df_metrics, gg=gg, linmod=linmod, results = results))
}
