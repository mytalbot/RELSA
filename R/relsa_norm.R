#' Data normalization for RELSA score calculation
#'
#' The \code{relsa_norm} function normalizes defined variables to 100 % at a given time point.
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
          mymeans  <- (set[set$id %in% traintiere[i], normthese[j]] / set[set$id %in% traintiere[i], normthese[j]][ontime])*100
          mymeans[is.nan(mymeans)]       <- NA
          mymeans[is.infinite(mymeans) ] <- NA
          mymeans[ length(mymeans)==0   ]<- NA
          myday    <- set[set$id == traintiere[i], "day"]
          mytier   <- set[set$id == traintiere[i], "id"]
          norm     <- rbind(norm, data.frame(id=mytier, day=myday, act=mymeans))

          # check if there are multiple entries per id (e.g., multiple measurements per day)
          n_occur <- c()
          n_occur <- data.frame(table( set[set$id %in% traintiere[i],"day"]))
          if(sum(n_occur$Freq ) > length(n_occur$Var1) ){
            warning("There are multiple daily entries per id. Normalization probably incorrect.")
          }else{}
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

          # check if there are multiple entries per id (e.g., multiple measurements per day)
          n_occur <- c()
          n_occur <- data.frame(table( set[set$id %in% traintiere[i],"day"]))
          if(sum(n_occur$Freq ) > length(n_occur$Var1) ){
            warning("There are multiple daily entries per id. Normalization probably incorrect.")
          }else{}

        }
      set[, which(names(set)==normthese) ] <- norm[,3]
    }

  }
  return(set)
}
