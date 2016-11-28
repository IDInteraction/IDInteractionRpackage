# Copyright (c) 2016 The University of Manchester, UK.
#
# Licenced under LGPL version 2.1. See LICENCE for details.
#
# The IDInteraction Attention Classification was developed in the IDInteraction project, funded by the Engineering and Physical Sciences Research Council, UK through grant agreement number EP/M017133/1.
#
# Author: David Mawdsley

# Functions to load and preprocess data

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
  
  earliertimes <- annotationdata$time <= time
  # Return first attention in file, since annotation notes *changes* 
  if(sum(earliertimes) == 0){ 
    return(annotationdata[1,"attentionlocation"])
  }
  
  attention <- tail(annotationdata[earliertimes, "attentionlocation"], n=1)
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
  
  # From Aitor's code:
  ##############TIME SHIFT FIX
  #It was found that there was a mismatch between the tracking and the annotations. I add 5 seconds to all tracking results.
  #tracking code already includes the start of the tracking, so I need to shift all annotations by that timestamp
  #annotatedDF$Timestamp..ms. = annotatedDF$Timestamp..ms. + trackingDF$Timestamp..ms.[1]
  annotationdata$time  = annotationdata$time + trackingdata$time[1]
  ###############TIME SHIFT END
  
  attentions <- sapply(trackingdata$time,  getattention, annotationdata=annotationdata)
  attentionlevels <-levels(annotationdata$attentionlocation)
  
  attentions <- factor(attentionlevels[attentions], levels = attentionlevels)
  
  trackingdata$attention <- attentions
  
  return(trackingdata)
}

#' given a numberSequence, normalises the numbers according to the min and max of the sequence
#'
#' @param numberSequence The numbers to normalise to the range 0...1
normalise0to1 <- function(numberSequence){
  return((numberSequence - min(numberSequence))/(max(numberSequence)-min(numberSequence)))
}


#' Add tracking features
#'
#' Taken from Aitor's code
#'
#' @param combinedDF the tracking data with added annotation data
#' @param participantCode the code of the participant
#'
#' @return The data frame with tracking features added
#'
#' @export
createFeatureDF <- function(combinedDF, participantCode = NA){
  
  featureDF <- data.frame(participantCode = participantCode,
                          frame = combinedDF$frame,
                          timestampms = combinedDF$time,
                          timestampMMSS = paste(floor(combinedDF$time/60/1000),":",floor((combinedDF$time/1000)%%60),sep=""),
                          attentionName = combinedDF$attention,
                          attentionIpad = as.integer(combinedDF$attention == "ipad"),
                          attentionTV = as.integer(combinedDF$attention == "tv"),
                          attentionElsewhere= as.integer(combinedDF$attention == "elsewhere"),
                          boxRotation = combinedDF$bbrot,
                          boxHeight = combinedDF$bbh,
                          boxWidth = combinedDF$bbw)
  
  featureDF[,"boxArea"] <- featureDF$boxHeight * featureDF$boxWidth
  
  featureDF[, "boxXcoord"] <- combinedDF$bbcx
  featureDF[,"boxYcoord"] <- combinedDF$bbcy
  
  #same as boxYcoord, but adjusted for the max and min
  featureDF[,"boxXcoordRel"] <- normalise0to1(combinedDF$bbcx)
  featureDF[,"boxYcoordRel"] <- normalise0to1(combinedDF$bbcy)
  
  featureDF[,"widthHeightRatio"] <- featureDF$boxHeight / featureDF$boxWidth
  
  
  ###Additional temporal features will be calculated here
  
  return(featureDF)
}


#' Generate a tracking feature dataframe for a participant
#'
#' @param participantCode The code number of the participant
#' @param trackingLoc The file path containing the tracking data
#' @param annoteLoc The file path containing the annotation data
#' @param trackingSuffix The suffiex to apply to tracking data, e.g. P01_suffix.csv
#' @param annoteSuffix The suffiex to apply to annotation data, e.g. P01_suffix.csv
#'
#' @return A data frame containing tracking and annotation data
#'
#' @export
createTrackingAnnotation <- function(participantCode,
                                     trackingLoc,
                                     annoteLoc,
                                     trackingSuffix = "_video.csv",
                                     annoteSuffix = "-timings.csv"){
  
  trackfn <- paste0(trackingLoc, participantCode, trackingSuffix)
  annotefn <- paste0(annoteLoc, participantCode, annoteSuffix)
  
  tracking <- loadParticipantTrackingData(trackfn)
  annotation <- loadParticipantAnnotationData(annotefn)
  
  trackannotate <- annotateTracking(tracking, annotation)
  
  featureDF <- createFeatureDF(trackannotate, participantCode = participantCode)
  
  return(featureDF)
}

#' Get a list of participants from a directory
#'
#' This function simply returns a list of all unique participant codes of the form P\\d+ in a
#' directory.  It doesn't (currently) check we have the same file for each participant.
#'
#' @param indir The input directory
#'
#' @return A vector of participant codes
#'
#' @export
getParticipantCodes <- function(indir){
  
  filelist <- list.files(indir)
  participantCodes <- unique(stringr::str_extract(filelist, "(P\\d+)"))
  
  return(participantCodes)
}

#' Flag whether each observation is for training or prediction
#'
#' Flag the start of the data with a training flag
#'
#' @param indata The input data set
#' @param traingime The amount of time to use for training, in minutes
#' @param timevar The variable containing the timestamp in ms
#'
#' @return a logical vector of length nrow(indata) indicating whether data are for
#' training (TRUE), or prediction (FALSE)
#'
#' @export
#'
flagtraining <- function(indata, trainingtime, timevar = "timestampms"){
  
  istraining <- ifelse(indata[,timevar] <= trainingtime * 1000 * 60, 
                       TRUE, 
                       FALSE)
  
  return(istraining)
}



#' Add a prefix to a set of variables
#' 
#' Add a prefix to all variables except those specified in a data-frame.
#' This is used to separate out the various object tracking/detection variables
#' @param indata The inputdata set
#' @param prefix The prefix to add to the variables
#' @param exclvars The variables not to prefix - will typically be participant codes, timestamps etc
#' 
#' @return The data-set with appropriate variables renamed
#' 
#' @export
renameVariables <- function(indata, prefix, exclvars = c("participantCode",
                                                         "timestampms",
                                                         "timestampMMSS",
                                                         "attentionName",
                                                         "attentionIpad",
                                                         "attentionTV",
                                                         "attentionElsewhere")){
  
  invars <- names(indata)
  
  newnames <- ifelse(invars %in% exclvars, invars, paste0(prefix, invars))
  
  names(indata) <- newnames
  
  return(indata)
  
}

#' Load a set of participants' tracking and annotation data; concatenate
#' 
#' @param p The participant names
#' @param trackingLoc The directory containing the tracking data
#' @param annoteLoc The directory containing the annotation data
#' 
#' @return A date set containing combined tracking and annotation data for each participant
#' 
#'@export
loadExperimentData <- function(p, trackingLoc, annoteLoc, ...){
  allparticipants <- NULL
  for(p in participants){
    thisparticipant <- createTrackingAnnotation(p,
                                                trackingLoc = trackingLoc,
                                                annoteLoc = annoteLoc,
                                                ...
    )
    
    allparticipants <- rbind(allparticipants, thisparticipant)
    
  }
  
  return(allparticipants)
} 


#' Get information about a video file
#' 
#' This function calls the Bash script midentify.sh, which is included as part of the package.
#' It requires mencoder is available in the path
#' 
#' @param videoIn the input video file
#' 
#' @return Information about the video file as a list
#' @export
getVideoInfo <- function(videoIn){
  scriptfile <- system.file("bin", "midentify.sh", package="IDInteraction")
  
  if(nchar(scriptfile) < 12){
    stop("Script not found")
  }
  
  vidinfo <- system2(scriptfile, args = videoIn, stdout = TRUE) 
  
  vidsplit <- strsplit(vidinfo, "=")
  
  vidlist <- list()
  
  # Convert what we can to numeric
  convertValue <- function(x){
    #http://stackoverflow.com/questions/24129124/how-to-determine-if-a-character-vector-is-a-valid-numeric-or-integer-vector
    if (suppressWarnings(all(!is.na(as.numeric(as.character(x)))))) {
      as.numeric(as.character(x))
    } else {
      x
    }
  }
  
  for(c in vidsplit){
    vidlist[c[1]] <- convertValue(c[2])
  }
  
  return(vidlist)
}

#' Get the length of a video in ms
#' 
#' @param participantCode the participantCode e.g. "P01"
#' @param vidloc the folder containing the videos
#' @param videoSuffix the suffix to appent to the participantCode
#' 
#' @return The length of the video in ms
#' @export
getVideoLength <- function(participantCode, vidLoc,
                           videoSuffix = "_video.mp4"){
  videofile <- paste0(vidLoc, participantCode, videoSuffix)
  
  videolength <- 1000 * getVideoInfo(videofile)$ID_LENGTH
  
  return(videolength)
}

#' Get the number of fps in a video
#' 
#' @param participantCode the participantCode e.g. "P01"
#' @param vidloc the folder containing the videos
#' @param videoSuffix the suffix to appent to the participantCode
#' 
#' @return The number of fps in a video
#' @export
getVideoFPS <- function(participantCode, vidLoc,
                           videoSuffix = "_video.mp4"){
  videofile <- paste0(vidLoc, participantCode, videoSuffix)
  
  videofps <- getVideoInfo(videofile)$ID_VIDEO_FPS
  
  return(videofps)
}


#' Get the number of frames in a video
#' 
#' @param participantCode the participantCode e.g. "P01"
#' @param vidloc the folder containing the videos
#' @param videoSuffix the suffix to appent to the participantCode
#' 
#' @return The length of the video in ms
#' @export
getVideoFrames <- function(participantCode, vidLoc,
                           videoSuffix = "_video.mp4"){
  videofile <- paste0(vidLoc, participantCode, videoSuffix)
  
  videoinfo <- getVideoInfo(videofile)
  
  videoframes <- videoinfo$ID_LENGTH * videoinfo$ID_VIDEO_FPS
  
  return(videoframes)
}




#' Get the timeskip at the start of a video
#' 
#' @param participantCode the participant code
#' @param skipLoc the location of the skip files
#' 
#' @return The time in ms before the experiment started
#' @export
getSkipTime <- function(participantCode, 
                          skipLoc){
  
  skipfile <- paste0(skipLoc, participantCode, "_video.skip" )
  
  skipval <- scan(skipfile, quiet = TRUE)
  
  if(length(skipval) != 1 || !is.numeric(skipval)){
    stop("Error reading skip file")
  }
  
  return(skipval)
}
  
  