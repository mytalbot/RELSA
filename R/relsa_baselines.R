#' Baseline calculation
#'
#' The \code{relsa_baselines} function calculates baseline levels for the provided reference set.
#'
#' @param dataset  sample data frame
#' @param bslday time point in the data frame used for baseline calculation (i.e. day=-1)
#' @param variables variables to use as references (must be loaded!)
#' @param turnvars variable names with "turned" direction (e.g. c("hr","temp"))
#'
#' @return \code{baseline} list with reference variables set to 100 \%, maximum reached values and some characteristics
#'
#' @export


relsa_baselines <- function(dataset=NULL, bslday=-1, variables=NULL, turnvars=NULL){

  # Check if there are variables
  if(length(variables)==0){
    print("Function cannot work! Please provide variables for the model.")
  }else{

    bl                <- dataset[dataset$day==bslday, variables]
    baseline          <- bl

    ############################################################################################
    # maxi/minimale Auslenkung des Modells für die Severity Parameter (ist schon normalisiert!)
    maxsev            <- apply(dataset[,variables],2,min, na.rm=TRUE)
    if(length(turnvars)>0){
      if(length(turnvars)==1){
        maxsev[turnvars] <- max(dataset[, turnvars], na.rm=TRUE)
      }else{
        maxsev[turnvars] <- apply(dataset[, turnvars],2,max, na.rm=TRUE)
      }
    }else{}

    ############################################################################################
    # Model characteristics
    ristics           <- data.frame(n          = length(unique(dataset$id)),
                                    treatments = length(unique(dataset$treatment)),
                                    conditions = length(unique(dataset$condition)),
                                    variables  = dim(dataset[, variables])[2])



  return(list(baseline=baseline, maxsev=maxsev, ristics=ristics) )

  }
}
