#' RELSA Default Reference Model
#'
#' The \code{relsa_reference} functions calculates the default RELSA reference model - to keeps things simple.
#' The reference set is the surgery data (transmitter-implanted, female B6 mice)
#'
#' @return list with baselines (bsl) and cluster levels (levels)
#'
#' @export
#'

relsa_reference <- function(vars=c("bwc", "burON","hr","hrv", "temp", "act"),
                            normthese = c("hr","burON","hrv", "temp", "act"),
                            turnthese = c("hr","temp"  )){
  raw          <- RELSA::surgery
  raw          <- raw[raw$treatment=="Transmitter", ]

  vars         <- vars
  turnthese    <- turnthese
  pre          <- relsa_norm(cbind(raw[,1:4], raw[,vars]), normthese=normthese)
  bsl          <- relsa_baselines(dataset=pre, variables=vars, turnvars= turnthese)
  levels       <- relsa_levels(refset=pre, bsl=bsl, turns = turnthese, k=4)

  return(list(bsl=bsl, levels=levels))

}

