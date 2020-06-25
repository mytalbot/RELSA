#' RELSA Default Reference Model
#'
#' The \code{relsa_reference} functions calculates the default RELSA reference model - to keeps things simple.
#'
#' @return list with baselines (bsl) and cluster levels (levels)
#'
#' @export
#'

relsa_reference <- function(){
  raw          <- RELSA::postop
  raw          <- raw[raw$treatment=="Transmitter", ]

  vars         <- c("bwc", "bur2h","burON","hr","hrv", "temp", "act", "mgs")
  turnthese    <- c("hr", "mgs", "temp"  )
  pre          <- relsa_norm(cbind(raw[,1:4], raw[,vars]), normthese=vars)
  bsl          <- relsa_baselines(dataset=pre, variables=vars, turnvars= turnthese)
  levels       <- relsa_levels(refset=pre, bsl=bsl, turns = turnthese, k=4)

  return(list(bsl=bsl, levels=levels))

}

