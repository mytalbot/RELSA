#' RELSA Wrapper
#'
#' The \code{relsa_wrapper} analyses the data set and calculates various RELSA objects for later analysis.
#' Please note that the data must be in the RELSA format.
#'
#' @param querydata dataset sample data frame
#' @param baseline cluster baselines from \code{relsa_baselines} function
#' @param treatment treatment column, potential data filter for subsampling (use column name as character, e.g. "transmitter")
#' @param condition condition column, potential data filter for subsampling (use column name as character, e.g. "Carprofen")
#' @param normthese variable vector (colum names as character, e.g. c("hr","temp")) with the names of the variables that need "normalization"
#' @param turnsQuery variable vector (colum names as character, e.g. c("hr","temp")) with the names of the variables that need "turning"
#' @param dropsQuery variables to drop
#' @param animalnr animal number (not id) in the data set for which an example RELSA flow is generated (defaults to 1)
#' @param ymax y-max of the y axis (defaults to 1.2)
#' @param ymin y-min of the y axis (defaults to 0)
#' @param pcadims for how many principal components shall the dimensional contributions be calculated (note: only 2 can be plotted, defaults to 2)
#' @param studylabel you can assign an individual study name to label some of the tables in the relsa object
#' @param severity you can assign a prospective severity for your experiment (has no effect on model calculations, just information)
#' @param colorlabel you can assign a color label to some tables of the relsa object

#' @return \code{robj} RELSA object - a list with RELSA calculation outputs.
#'
#' @importFrom utils read.table
#' @importFrom stats sd
#' @import FactoMineR
#' @import dplyr
#' @export
#'

relsa_wrapper <- function(querydata, baseline=NULL, treatment=NULL, condition=NULL, normthese=NULL,
                          turnsQuery=NULL, dropsQuery=NULL, animalnr=1, ymax=1.2, ymin=0,
                          pcadims=2, studylabel=NULL, severity=NULL, colorlabel=NULL,
                          showScree="yes", saveTiff="no", showPlot="no", k=6){


  ### Baseline model
  raw          <- RELSA::postop
  raw          <- raw[raw$treatment=="Transmitter", ]

  vars         <- c("bwc", "bur2h","burON","hr","hrv", "temp", "act", "mgs")
  turnvars     <- c("hr", "mgs", "temp")
  org          <- cbind(raw[,1:4], raw[,vars])
  pre          <- relsa_norm(org,   normthese=c("hr","hrv", "temp", "act", "mgs" ), ontime=1)
  bsl          <- relsa_baselines(dataset=pre, bslday=-1, variables=vars, turnvars=turnvars)

  levels       <- relsa_levels(pre, mypath="C:/MHH Bleich/Papers/Nature RELSA/Figs/", bsl,
                               filename="Burrowing levels", drops=c("bw","score"), turns=c("hr","mgs","temp"), relsaNA=NA, k=k,
                               showScree=showScree, customCol= c("red","green","blue","magenta"), seed=123, myYlim=c(0,1.4),
                               saveTiff=saveTiff, showPlot=showPlot)


  # Load the query data & build test case OR provide the data frame
  if(is.data.frame(querydata)){

    if ( is.null(treatment)==FALSE & is.null(condition)==FALSE) {
      querydata    <- querydata[querydata$treatment==treatment & querydata$condition==condition, ]
      print("Done!")
    } else if ( is.null(treatment)==FALSE ) {
      querydata    <- querydata[querydata$treatment==treatment, ]
      print("Done!")
    } else if ( is.null(condition)==FALSE) {
      querydata    <- querydata[querydata$condition==condition, ]
      print("Done!")
    }else{
      querydata    <- querydata
      print("Done!")
    }

  }else{
    querydata  <- relsa_load(querydata, treatment=treatment, condition=condition )
  }

  testraw      <- querydata
  vars         <- names(testraw)[5:length(names(testraw))] #names(testraw[,-c(1:4)])
  pre_test     <- cbind(testraw[,1:4], testraw[,vars])
  testset      <- relsa_norm(pre_test, normthese=normthese, ontime=1)


  # apply dropouts to testset before continuing
  if( is.null(dropsQuery)==TRUE ){
  }else{
    testset[names(testset) %in% dropsQuery] <- NULL
  }

  # calculate the RELSAs
  df      <-NULL
  profile <- c()
  for(i in 1:length(unique(testset$id))){
    animal       <- i
    R            <- relsa(testset, bsl=bsl, a=animal,  drop=dropsQuery, turnvars=turnsQuery, relsaNA=NA)$relsa
    R$treatment  <- pre_test[pre_test$id==unique(pre_test$id)[animal],"treatment"]
    R$condition  <- pre_test[pre_test$id==unique(pre_test$id)[animal],"condition"]
    df           <- rbind(df, data.frame(id=unique(pre_test$id)[animal], day=R$day,
                                         treatment=R$treatment, condition=R$condition, rms=R$rms))
  }

  # calculate the target differences to refmaxdelta per variable
  deltascores    <-NULL
  for(i in 1:length(unique(testset$id))){
    animal           <- i
    delta            <- relsa(testset, bsl=bsl, a=animal,  drop=dropsQuery, turnvars=turnsQuery, relsaNA=NA)$delta

    trt              <- pre_test[pre_test$id==unique(pre_test$id)[animal],"treatment"]
    cond             <- pre_test[pre_test$id==unique(pre_test$id)[animal],"condition"]

    preambl           <- NULL
    preambl$id        <- NULL
    preambl$day       <- NULL
    preambl$treatment <- NULL
    preambl$condition <- NULL

    preambl$id        <- unique(pre_test$id)[animal]
    preambl$day       <- R$day[1:dim(delta)[1]]
    preambl$treatment <- trt
    preambl$condition <- cond


    deltascores       <- rbind(deltascores,
                               cbind(id        = preambl$id,
                                     day       = preambl$day,
                                     treatment = preambl$treatment,
                                     condition = preambl$condition,
                                     delta))


  }


  # calculate the relsa weights
  relsaweights   <-NULL
  for(i in 1:length(unique(testset$id))){
    animal       <- i
    rw           <- relsa(testset, bsl=bsl, a=animal,  drop=dropsQuery, turnvars=turnsQuery, relsaNA=NA)$relsa[,-1]
    rw[,c("wf","rms")] <- NULL

    trt          <-  testset[testset$id==unique(testset$id)[animal],"treatment"]
    cond         <-  testset[testset$id==unique(testset$id)[animal],"condition"]

    preambl           <- NULL
    preambl$id        <- NULL
    preambl$day       <- NULL
    preambl$treatment <- NULL
    preambl$condition <- NULL

    preambl$id        <- unique(testset$id)[animal]
    preambl$day       <- R$day[1:dim(rw)[1]]
    preambl$treatment <- trt
    preambl$condition <- cond

    relsaweights       <- rbind(relsaweights,
                               cbind(id        = preambl$id,
                                     day       = preambl$day,
                                     treatment = preambl$treatment,
                                     condition = preambl$condition,
                                     rw))



  }


  # Unique animals in the query set
  tiere          <- unique(df$id)

  # calculate RELSA max for each model (time-independent)
  relsamax <- NULL
  ddf      <- NULL
  for (j in 1:length(tiere)){
    relsamax   <- rbind(relsamax, data.frame(id        = tiere[j],
                                             treatment = as.character(df[df$id==tiere[j], 3][1]),
                                             condition = as.character(df[df$id==tiere[j], 4][1]),
                                             relsa     = max( df[df$id==tiere[j], "rms"], na.rm=TRUE) ))
  }

  # color option the RELSA max table
  if(is.null(colorlabel) ==TRUE){
  }else{
    relsamax$color = colorlabel
  }

  # add a studylabel for later RELSA max discrimination and subsetting
  if(is.null(studylabel)){
  }else{
    relsamax$label <- studylabel
  }

  # add a label for the prospective severity
  if(is.null(severity)){
  }else{
    relsamax$severity <- severity
  }


  ### PCA -variable contributions for the first 2 dimensions
  q         <- testset[5:dim(testset)[2]]

  if((5-sum(apply(q,2,sum, na.rm=TRUE)==0))==1){

    contributions <- NA

  }else{
    # drop the drops
    if(sum(names(q) %in% dropsQuery)==0){
      q   <- q[complete.cases(q),]
    }else{
      nn  <- which(names(q) %in% dropsQuery)
      q   <- q[,-nn]
      q   <- q[complete.cases(q),]
    }

   q      <- q[complete.cases(q),]

   pca    <- PCA(q, scale.unit = TRUE, graph = FALSE)

    if(pcadims==1){
      contributions <- pca$var$contrib[, 1]
    }else{
      contributions <- apply(pca$var$contrib[, 1:pcadims ],1,sum)
    }
  }



  ### Calculate the most changing RELSA Weight!
  V <- NULL
  for(t in 1:length(tiere)){
    v    <- relsaweights[deltascores$id%in%tiere[t],]

    V    <- rbind(V, suppressWarnings( apply( v[5:dim(v)[2]],2, max, na.rm=TRUE )))
    is.na(V) <- sapply(V, is.infinite)
  }

  Change           <- t(data.frame(apply(V,2,mean, na.rm=TRUE)))
  colnames(Change) <- colnames(V)
  rownames(Change) <- 1

  ChangeSD           <- t(data.frame(apply(V,2,sd, na.rm=TRUE)))
  colnames(ChangeSD) <- colnames(V)
  rownames(ChangeSD) <- 1

  Change  [is.nan(Change)]   <- NA
  ChangeSD[is.nan(ChangeSD)] <- NA


  ### Example Animals anschauen
  if(is.null(animalnr)){

  }else{
    tiere <- unique(df$id)
    dat   <- df[df$id==tiere[animalnr], ]

    plot(dat$day, dat$rms, pch=19, type="b", ylim=c(ymin,ymax), xlab="time", ylab="RELSA score")
    abline(h=1, lty=2, lwd=2)
    #points(dat$day[which(dat$rms==max(dat$rms, na.rm=TRUE))][which(as.numeric(is.na(dat$rms))==1)[1]-1], max(dat$rms, na.rm=TRUE), pch=1, lwd=2, cex=2.2, col="red")
    #text(dat$day[which(dat$rms==max(dat$rms, na.rm=TRUE))]  , max(dat$rms, na.rm=TRUE),
    #     labels=max(dat$rms, na.rm=TRUE), cex=0.9, pos=4, col="red")
  }

  return(list(raw=testraw, normalized=testset, df=df, deltascores=deltascores, Rw=relsaweights,
              relsamax=relsamax, PCAcontrb=contributions, PCAobj=pca, mean_max_Rw_change_SD= ChangeSD,
              mean_max_Rw_change=Change, n=length(tiere), levels=levels))
}
