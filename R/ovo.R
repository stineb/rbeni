## This is to be used in combination with the bash function 'ovo' to plot content of files from command line.
## Add the 'ovo' function to your bash_profile:
# function ovo {
#   # Creates a x-y line plot of a table using R and opens the PDF
#   # 1. argument: file name(s). If multiple file names are passed as arguments, they should be included as "filn1 filn2 ...".
#   Rscript /alphadata01/bstocker/sofun/utils_sofun/analysis_sofun/ovo.R "$1"
#   open ~/tmp/tmp_ovo.pdf
# }


## First read in the arguments listed at the command line
args <- commandArgs(trailingOnly = TRUE)

## arguments:
## 1. file name
## 2. x-axis limits in R syntax, e.g. "c(2000,2012)"

if( length(args)==0 ){
  
  ## supply default values
  print( "No arguments supplied. ...")
  filn <- list.files( path="./", pattern="*out" )[1]

} else {

  ## evaluate arguments
  fils <- strsplit( args[1], " " )[[1]]

  pdf( "~/tmp/tmp_ovo.pdf", width=8, height=6 )
    par( las=1 )
    
    plotinit <- TRUE
    colidx <- 1
    for (filn in fils){

      data <- read.table( filn )
      nvars <- ncol(data)-1    

      if (plotinit) { 
        plot( data$V1, data$V2, type="l" ) 
        if (nvars>1){
          for (ivar in 3:ncol(data)){
            colidx <- colidx + 1
            lines( data$V1, data[,ivar], col=colidx )
          }      
        }
      } else {
        for (ivar in 2:ncol(data)){
          colidx <- colidx + 1
          lines( data$V1, data[,ivar], col=colidx )
        }              
      }
      plotinit <- FALSE
    }

  dev.off()

}
