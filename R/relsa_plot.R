#' Plot RELSA score and variables together
#'
#' The \code{relsa_plot} function visualizes the RELSA score alone or in combination with one of the variables.
#'
#' @param set dataset sample data frame
#' @param RELSA RELSA object
#' @param levels k+1 levels from kmeans calculations (relsa_levels function)
#' @param animal which unique animal in the set?
#' @param plotvar which variable shall be plotted (does not scale with RELSA score)
#' @param plotRELSA plot the RELSA score "yes/no"?
#' @param myylim custom y-axis limits (left axis)
#' @param myYlim custom y-axis limits (right axis)
#' @param mypch custom dot marker
#' @param myXlab custom x-axis label
#'
#' @return \code{relsa} data.frame with RELSA scores for each variable, the sum and mean score
#'
#' @importFrom graphics plot abline mtext
#'
#' @export
#'

relsa_plot <- function(set=NULL, RELSA, levels=NULL, animal=1, plotvar=NULL, plotRELSA=TRUE,
                       myylim=c(70,110),  myYlim=c(0,2), mypch=1, myXlab="time point"){

  plotset    <- set[set$id==unique(set$id)[animal],]

  relsatype  <- as.numeric(unlist(RELSA$relsa$rms))

  if(length(plotvar)!=0){
    plot(plotset$day, plotset[,which(names(plotset)==plotvar)], pch=mypch, type="b", ylab=plotvar, xlab=myXlab, ylim=myylim)

    if(plotRELSA==TRUE){
      par(new = TRUE)
      plot(plotset$day,
           relsatype,   type = "b", lwd=2, xaxt = "n", yaxt = "n", ylab = "", xlab = "", col="red", ylim=myYlim )
      axis(side = 4)
      mtext("RELSA Score", side = 4, line = 3)

      # write level lines to plot
      if(length(levels)==0){
      }else{
        for(l in 1:length(levels)){
          abline(h=levels[l], lwd=1, lty=2)
        }
      }

    }else{}

  }else{
    plot(RELSA$relsa$day ,
         relsatype,   type = "b", lwd=2, ylab = "RELSA score", xlab=myXlab, col="red", ylim=myYlim, pch=mypch )

    # write level lines to plot
    if(length(levels)==0){
    }else{
      for(l in 1:length(levels)){
        abline(h=levels[l], lwd=1, lty=2)
      }
    }

  }


  return(plotset)
}
