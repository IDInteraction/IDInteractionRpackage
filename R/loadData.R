#'  Load participant tracking data
#'
#'  Load the participant tracking data.  Assumes csv data is in "full" format
#'  i.e. all bounding box vertices are given
#'
#'
#' @param The input csv file
#'
#' @return The tracking data
#'
#' @export
loadParticipantTrackingData <- function(indata){
  participantData <- read.csv(file = indata,
                              col.names = c("frame",
                                            "time",
                                            "actpt",
                                            "bbcx",
                                            "bbcy",
                                            "bbw",
                                            "bbh",
                                            "bbrot",
                                            "bbv1x",
                                            "bbv1y",
                                            "bbv2x",
                                            "bbv2y",
                                            "bbv3x",
                                            "bbv3y",
                                            "bbv4x",
                                            "bbv4y"))

  return(participantData)

}


#' Load annotation data for a participant
#'
#' @param indata The input csv file
#'
#' @return The annotation data
#'
#' @export
loadParticipantAnnotationData <- function(indata){
  annotationdata <- read.csv(file = indata,
                             col.names=c("time",
                                         "toipad",
                                         "totv",
                                         "toelsewhere",
                                         "attentionlocation"))

  return(annotationdata)

}


#' Get where the participants attention was directed at, at time t
#'
#' @param time The time in ms
#' @param annotationdata The annotation data-set to query
#'
#' @importFrom dplyr "%>%"
#' @export
getattention <- function(time, annotationdata){
  attention <- annotationdata[annotationdata$time <= time,] %>%
    dplyr::summarise(dplyr::last(attentionlocation))

  return(as.numeric(attention))
}

#' Add annotations to a tracking data set
#'
#' @param trackingdata  The tracking data-set
#' @param annotationdata The annotation data-set
#'
#' @importFrom dplyr "%>%"
#'
#' @export
annotateTracking <- function(trackingdata, annotationdata){


  attentions <- sapply(trackingdata$time,  getattention, annotationdata=annotationdata)
  attentionlevels <-levels(annotationdata$attentionlocation)

  attentions <- factor(attentionlevels[attentions], levels = attentionlevels)

  trackingdata$attention <- attentions

  return(trackingdata)
}
