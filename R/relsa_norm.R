#' Data normalization for RELSA score calculation
#'
#' The \code{relsa_norm} function calculates 95\% confidence intervals and mean values for provided sample baselines.
#'
#' @param set dataset sample data frame
#' @param normthese variable string in the input data set that shall be normalized
#' @param ontime unique reference time point in dataset (e.g. 1 for day 1)
#'
#' @return \code{set} data.frame with normalized column
#'
#' @export
#'
#'
relsa_norm     <- function(set, normthese=NULL, ontime=1){

  traintiere   <- unique(set$id)

  if(length(normthese)==0){
  }else{

    if(length(normthese)>1){

      for(j in 1:length(normthese)){
        norm       <- NULL
        for(i in 1:length(traintiere)){
          mymeans  <- (set[set$id == traintiere[i],normthese[j]] / set[set$id == traintiere[i], normthese[j]][ontime])*100
          mymeans[is.nan(mymeans)]       <- NA
          mymeans[is.infinite(mymeans) ] <- NA
          mymeans[ length(mymeans)==0   ]<- NA
          myday    <- set[set$id == traintiere[i], "day"]
          mytier   <- set[set$id == traintiere[i], "id"]
          norm     <- rbind(norm, data.frame(id=mytier, day=myday, act=mymeans))
        }
        set[, which(names(set)==normthese[j]) ] <- norm[,3]
      }


    }else{
      norm         <- NULL
        for(i in 1:length(traintiere)){
          mymeans  <- (set[set$id == traintiere[i],normthese] / set[set$id == traintiere[i], normthese][ontime])*100
          mymeans[is.nan(mymeans)]       <- NA
          mymeans[ is.infinite(mymeans) ]<- NA
          mymeans[ length(mymeans)==0   ]<- NA
          myday    <- set[set$id == traintiere[i], "day"]
          mytier   <- set[set$id == traintiere[i], "id"]
          norm     <- rbind(norm, data.frame(id=mytier, day=myday, act=mymeans))
        }
      set[, which(names(set)==normthese) ] <- norm[,3]
    }

  }
  return(set)
}
