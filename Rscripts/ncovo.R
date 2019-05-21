## This is to be used in combination with the bash function 'ovo' to plot content of files from command line.
## Add the 'ovo' function to your bash_profile:
# function ovo {
#   # Creates a x-y line plot of a table using R and opens the PDF
#   # 1. argument: file name(s). If multiple file names are passed as arguments, they should be included as "filn1 filn2 ...".
#   Rscript /alphadata01/bstocker/sofun/utils_sofun/analysis_sofun/ovo.R "$1"
#   open ~/tmp/tmp_ovo.pdf
# }

# require(ncdf4)
# require(lubridate)
# require(rbeni)
require(ggplot2)
# require(purrr)
library(dplyr)

source("/alphadata01/bstocker/rbeni/R/conv_noleap_to_ymd.R")

## First read in the arguments listed at the command line
args <- commandArgs(trailingOnly = TRUE)

## arguments:
## 1. file name
## 2. x-axis limits in R syntax, e.g. "c(2000,2012)"

if ( length(args)==0 ){

  ## supply default values
  print( "No arguments supplied. ...")
  filn <- list.files( path="./", pattern="*nc" )[1]

} else {

  ## evaluate arguments
  fils <- strsplit( args[1], " " )[[1]]

  # filn <- "/alphadata01/bstocker/sofun/trunk/output_nc/AR-SLu.2011.d.pet.nc"
  # fils <- c(filn, "/alphadata01/bstocker/sofun/trunk/output_nc/AR-SLu.2011.d.gpp.nc")

  print("fils:")
  print(fils)

  read_nc_onefile <- function(filn){

    nc <- ncdf4::nc_open(filn)
    # Save the print(nc) dump to a text file
    {
      sink(paste0(filn, ".txt"))
      print(nc)
      sink()
    }

    out <- list(
      lon = ncdf4::ncvar_get(nc, nc$dim$lon$name),
      lat = ncdf4::ncvar_get(nc, nc$dim$lat$name),
      time = ncdf4::ncvar_get(nc, nc$dim$time$name)
    )

    if (nc$dim$time$units=="days since 2001-1-1 0:0:0"){
      out$time <- conv_noleap_to_ymd(out$time, since = lubridate::ymd("2001-01-01"))
    }

    # get variables
    varnams <- ls(nc$var)
    getvar <- function(varnam){
      #tibble( !!varnam := ncdf4::ncvar_get(nc, varnam) )
      out <- list()
      out[[varnam]] <- ncdf4::ncvar_get(nc, varnam)
    }
    vars <- purrr::map(as.list(varnams), ~getvar(.)) %>%
      setNames(varnams)

    nc$var[[varnams[1]]]$units

    out[["vars"]] <- vars
    out[["varnams"]] <- varnams

    return(out)
  }

  out <- purrr::map(as.list(fils), ~read_nc_onefile(.)) %>%
    setNames(fils)

  lon  <- purrr::map_dfc(out, "lon")
  lat  <- purrr::map_dfc(out, "lat")
  time <- purrr::map_dfc(out, "time")
  vars <- purrr::map_dfc(out, "vars")

  if (nrow(lon)==1 && nrow(lat)==1){
    ## point data => plot time series
    time <- time %>% dplyr::select(1) %>% setNames("time")
    lon_lab <- lon[1,1] %>% unlist() %>% format(digits=5)
    lat_lab <- lat[1,1] %>% unlist() %>% format(digits=5)
    vars %>%
      dplyr::bind_cols(time, .) %>%
      tidyr::gather(varnam, value, 2:(ncol(vars)+1)) %>%
      ggplot2::ggplot(aes(x = time, y = value, color = varnam)) +
      ggplot2::geom_line() +
      ggplot2::labs(subtitle = paste("Longitude: ", lon_lab, "  Latitude: ", lat_lab)) +
      ggplot2::scale_color_discrete(name="", breaks=ls(vars), labels=ls(vars))
    ggplot2::ggsave("~/tmp/tmp_ovo.pdf")
  }

}




