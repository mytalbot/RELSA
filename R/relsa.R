#' Relative Severity Assessment Score
#'
#' The \code{relsa} function calculates a composite relative severity score based on normalized
#' differences. Further, the values are regularized with a reference set to estimate relative
#' severity.
#'
#' @param set dataset sample data frame
#' @param bsl reference or baseline data (must be in RELSA format, see relsa_load)
#' @param a unique animal index in data set
#' @param drop character vector of variables to drop
#' @param turnvars variables with inverted development under severity (i.e. temperature might rise or drop depending on the actual animal model)
#' @param relsaNA codes how to handle NaN values during calculations (default is NA)
#'
#' @return \code{relsa} list with RELSA results (differences and weights plus RELSA score)
#'
#' @examples
#'\dontrun{
#' RELSA <- relsa(set=testset, bsl, a=1, drop=c("bw","score"), turnvars="temp")
#'}
#'
#' @export
#'

relsa <- function(set, bsl, a=1, drop=NULL, turnvars=NULL, relsaNA=NA ){

  ### Build a data subset defined by the chosen animal
  whichanimal  <- unique(set$id )
  subdata      <- set[set$id == whichanimal[a], 4:dim(set)[2]]
  days         <- subdata$day

  ### Drop variables (check if the dropped vars are in the set, if so: do it)
  if( length(turnvars[turnvars %in% names(subdata)])==0 ){
    turnvars   <- NULL
  }else{}

  if( is.null(drop)==TRUE ){
  }else{
    subdata[names(subdata) %in% drop] <- NULL
    turnvars                          <- turnvars[!(turnvars %in% drop)]
  }

  ### calculate difference matrix for the animal & turn "positive" variables
  if( is.null(turnvars)==TRUE ){
    delta            <- 100 - subdata[-1]
    bsdelta          <- 100 - bsl$maxsev
  }else{
    delta            <- 100 - subdata[-1]
    delta[,turnvars] <- delta[, turnvars]* -1

    bsdelta          <- 100 - bsl$maxsev         # for the extreme values of baseline vars
    bsdelta[turnvars]<- bsdelta[turnvars]* -1
  }

  ### set all negative values to NA - otherwise zero would allow usage in the mean!
  delta[delta<0]     <- 0

  # round delta
  delta              <- round(delta,2)

  # get the order of names in delta for baseline relsa score 2
  namen              <- names(delta)

  ### RELSAs
  # If all is empty, there goes NA everywhere
  if(length(delta)==0 ){
    delta              <- cbind(day= days, NA)

    relsa_3            <- NULL
    relsa_3            <- round( wfactor ,2)
    relsa_3            <- cbind(relsa_3, wf=NA)
    relsa_3            <- cbind(relsa_3, rms=NA)
    relsa_3            <- cbind(relsa_3, rmsplus=NA)

  }else{
    # weighting the relsa score by knowing the extreme values from the training set
    wfactor   <- NULL
    for(r in 1:dim(delta)[1]){
      wfactor          <- as.data.frame(round( rbind(wfactor, delta[r,] / bsdelta[namen]), 2))
      #wfactor          <- as.data.frame(round( rbind(wfactor, delta[r,]^2 / bsdelta[namen]^2), 2)) # exp alternative
    }

    ### set all negative values to NA
    wfactor[wfactor<0] <- NA

    ### calculate wRELSA and Weight factor alone
    wf_sum             <- round(apply(wfactor, 1, sum, na.rm=T),2 ) # Summarize
    wf                 <- wf_sum  / (dim(wfactor)[2] - apply(apply(wfactor,2, is.na),1,sum))

    if(length(wf)>0){
      wf[is.nan(wf)] <- relsaNA
    }else{}

    # if weights are smaller 0, kill them!
    wf[wf <0]        <- relsaNA

    # get NA positions for rms correction
    na.idx           <- is.na(wf)

    ####### RELSA: score 2: METHOD 2 (RMS) #######
    # RMS RELSA
    rms     <- c()
    for(l in 1:dim(wfactor)[1]){
      RMS                <- sqrt( sum( wfactor[l, ]^2 ,na.rm=TRUE )/ (length(wfactor[l, ]) - sum(is.na(wfactor[l, ])))  )
     # RMS                <- sum( wfactor[l, ] ,na.rm=TRUE )/ (length(wfactor[l, ]) - sum(is.na(wfactor[l, ])))
     # RMS                <- sqrt( sum( wfactor[l, ]^2 ,na.rm=TRUE )) # old
      if( sum(is.na(wfactor[l, ]))==length(names(wfactor)) ){ RMS<-NA} else {}
      rms[l]             <- RMS
    }
    rms[na.idx]          <- NA
    rms     <- as.data.frame(rms)

    ####### RELSA: finalizing the results #######
    relsa_3            <- NULL
    relsa_3            <- round( wfactor ,2)
    relsa_3            <- as.data.frame(relsa_3)
    relsa_3            <- cbind(relsa_3, wf)
    relsa_3            <- cbind(days, relsa_3)

    if(all(is.na(rms))==TRUE){
      relsa_3$rms      <- as.numeric(unlist(rms))
    }else{
      relsa_3$rms      <- round( rms,2)
    }
  }

  vars  <- names(subdata)[!(names(subdata) %in% "day")]

  return( list(delta=delta, relsa=relsa_3, testVars=vars) )

}
