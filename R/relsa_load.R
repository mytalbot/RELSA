#' Loads data in RELSA Format
#'
#' The \code{relsa_load} function loads data into a ready-to-use format for RELSA.
#'
#' @param file dataset sample data frame
#' @param treatment treatment string (e.g., "Transmitter")
#' @param condition condition string (e.g. Carprofen) in the sample table

#' @return \code{set} data.frame with set sample data
#'
#' @importFrom utils read.table
#'
#' @export
#'

relsa_load <- function(file, treatment=NULL, condition=NULL){

  if ( is.null(treatment)==FALSE & is.null(condition)==FALSE) {
    set    <- read.table(file, header = T, row.names = 1,fill = TRUE, sep="\t" )
    set    <- set[set$treatment==treatment & set$condition==condition, ]
    print("Done!")
  } else if ( is.null(treatment)==FALSE ) {
    set    <- read.table(file, header = T, row.names = 1,fill = TRUE, sep="\t" )
    set    <- set[set$treatment==treatment, ]
    print("Done!")
  } else if ( is.null(condition)==FALSE) {
    set    <- read.table(file, header = T, row.names = 1,fill = TRUE, sep="\t" )
    set    <- set[set$condition==condition, ]
    print("Done!")
  }else{
    set      <- read.table(file, header = T, row.names = 1, fill = TRUE, sep="\t")
    print("Done!")
  }

  # Curate missing daily data for equal lengths -----------------------------
  vars <- names(set)[5:dim(set)[2]]
  ids  <- as.character(unique(set$id))

  # determine max number on n in data.frame
  N    <- c()
    for(i in 1:length(ids)){
    n       <- dim(set[set$id==ids[i],])[1]
    N[i]    <- n
  }
  Nmax      <- max(N, na.rm=TRUE)

  # Wenn kleiner als Nmax, dann kure, sonst appende einfach
  SET <- NULL
  for(z in 1:length(ids)){
    D         <- set[set$id==ids[z],]
    if(dim(D)[1]<Nmax){
      delta   <-  Nmax - dim(D)[1]
      days    <- D$day
      gapdays <- seq(from=max(D$day, na.rm=TRUE)+1, to=max(D$day, na.rm=TRUE)+delta, by=1)

      D[(nrow(D)+1):Nmax,] <- NA

      D$id        <- D$id[1]
      D$treatment <- D$treatment[1]
      D$condition <- D$condition[1]
      D$day       <- c(days, gapdays)

      SET         <- rbind(SET, D)

      warning("Time vectors in the raw data have different lengths. This was adjusted automatically. You do not have to do anything.")
    }else{
      SET         <- rbind(SET, D)
    }
  }
  set <- SET
  return(set)
}
