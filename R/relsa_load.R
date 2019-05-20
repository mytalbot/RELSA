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
  return(set)
}
