#' Read an openface file
#' 
#' @param infile The input file
#' @param checkValid Whether to perform (some) checks on the validity of input data
#' 
#' @return The open face data
#' 
#' @export
readOpenFace <- function(infile, checkValid = FALSE){
  
  openface <- data.table::fread(infile, data.table = FALSE)
  if (checkValid)
  {
    if (!all( seq( from = openface$frame[1], by = 1, length.out = nrow(openface)) == openface$frame))
    {
      stop("Missing frames in OpenFace data")
    }
    
    if (!all(complete.cases(openface)) )
    {
      stop("Missing values in OpenFace data")
    }
  }
  
  return(openface)
  
}

#' Load openface and attention data, and join (taking account of frame offsets and skips)
#' 
#' Optionally output the ground truth data in a format suitable for abc-dt (to verify offsets are OK),
#' and skip files (for cppMT pipeline)
#' 
#' keyfile contains the annotations we wish to manually extract as "events", which we later filter on
#' 
#' If the videosource is "kinect" the necessary transformations will be applied to the timestamps to 
#' synchronise the kinect video to the webcam (which the ground truth was encoded from).
#' 
#' @param attentionfile The file containing the ground truth attention data (named Pxx_attention.txt)
#' @param keyfile The file containing annotations we wish to define as events
#' @param openfacefile The file containing the openface data
#' @param videosource The source of the video; webcam, kinect
#'
#' @return A dataframe containing the openface and ground truth data
#' 
#' @export
loadOpenfaceAndAttention <- function(attentionfile,
                                     keyfile = NULL,
                                     openfacefile,
                                     videosource ="specify",
                                     participantCode, ... ){
  
  if (!(videosource %in% c("kinect", "webcam")))
  {
    stop("Video source must be kinect or webcam")
  }
  
  args <- list(...)
  

  
  if ("checkValid" %in% names(args))
  {
    openface <- readOpenFace(openfacefile, checkValid = TRUE)
  }
  else
  {
    openface <- readOpenFace(openfacefile)
  }

    attentions <- loadSpotTheDifferenceFile(attentionfile,
                                          keyfile = keyfile,
                                          participantCode = participantCode)
    thisParticipanAttention <- attentions[attentions$participantCode == participantCode,]
    thisParticipanAttention <- thisParticipanAttention[order(thisParticipanAttention$attTransMidss),]
  
  if (videosource == "kinect")
  {
    
    webcamKinectOffsetFrames <- getKinectWebcamOffset(participantCode)
    KinectFrameTimes <- getKinectFrameTimes(participantCode)
    
    openface$webcamtime <- kinectTimeToWebcamTime( openface$timestamp, KinectFrameTimes,webcamKinectOffsetFrames)
  }else{
    openface$webcamtime <- openface$timestamp
  }
  
  # Attach attentions
  openface$attention <- sapply(openface$webcamtime, getattention2,
                               annoteset = thisParticipanAttention[thisParticipanAttention$eventtype == "attention",])
  
  
  openface$attention <- factor(openface$attention)
  
  
  return(openface)
  
} 



#' Load cppMT and attention data, and join (taking account of frame offsets and skips)
#' 
#' keyfile contains the annotations we wish to manually extract as "events", which we later filter on
#' 
#' If the videosource is "kinect" the necessary transformations will be applied to the timestamps to 
#' synchronise the kinect video to the webcam (which the ground truth was encoded from).
#' 
#' @param attentionfile The file containing the ground truth attention data (named Pxx_attention.txt)
#' @param keyfile The file containing annotations we wish to define as events
#' @param openfacefile The file containing the openface data
#' @param videosource The source of the video; webcam, kinect
#'
#' @return A dataframe containing the openface and ground truth data
#' 
#' @export
loadcppMTAndAttention <- function(attentionfile,
                                  keyfile = NULL,
                                  cppmtfile,
                                  videosource ="specify",
                                  participantCode ){
  
  
  if (!(videosource %in% c("kinect", "webcam")))
  {
    stop("Video source must be kinect or webcam")
  }
  
  
  
  attentions <- loadSpotTheDifferenceFile(attentionfile,
                                          keyfile = keyfile,
                                          participantCode = participantCode)
  
  
  cppmtData <- loadParticipantTrackingData(cppmtfile)
  cppmtData$timestamp <- cppmtData$time/1000
  
  thisParticipanAttention <- attentions[attentions$participantCode == participantCode,]
  thisParticipanAttention <- thisParticipanAttention[order(thisParticipanAttention$attTransMidss),]
  
  if (videosource == "kinect")
  {
    
    webcamKinectOffsetFrames <- getKinectWebcamOffset(participantCode)
    KinectFrameTimes <- getKinectFrameTimes(participantCode, returnFrames = TRUE)
    
    cppmtData$webcamtime <- kinectTimeToWebcamTime( cppmtData[,c("frame", "timestamp")], 
                                                    KinectFrameTimes,webcamKinectOffsetFrames)
  }else{
    cppmtData$webcamtime <- cppmtData$timestamp
  }
  
  # Attach attentions
  cppmtData$attention <- sapply(cppmtData$webcamtime, getattention2,
                                annoteset = thisParticipanAttention[thisParticipanAttention$eventtype == "attention",])
  
  
  cppmtData$attention <- factor(cppmtData$attention)
  
  # Drop kinect time and replace with webcamtime
  cppmtData$time <- cppmtData$webcamtime * 1000
  
  featureset <- createFeatureDF(cppmtData, participantCode)
  
  
  featureset$time <- featureset$timestampms / 1000
  
  return(featureset)
  
}


#' Get the time an event occured at
#' 
#' Using the annotation data in attentiondata, return the time an event occured at
#' 
#' 
#' @param participantCode the participant code
#' @param experimentpart the part of the experiment (1 or 2)
#' @param event the event (start or end)
#' @param attentiondata the data frame containing the attentiondata
#' @param attentionpoint what point in the attention transition to take
#' 
#' @export
getEventTime <- function(participantCode, experimentpart, event, attentiondata, 
                         attentionpoint = "attTransMidss"){
  
  
  # Get the frame number that an experiemnt part start/ends at
  part <- stringr::str_match(experimentpart, "\\d+")
  if (is.na(part)) {
    stop("Experiment part could not be extracted")
  }
  
  eventstring <- paste0(event, part)
  
  eventdata <- attentiondata[attentiondata$eventtype == "event" & 
                               attentiondata$participantCode == participantCode &
                               attentiondata$annotation == eventstring,] 
  if (nrow(eventdata) > 1) {
    stop("Multiple events matched")
  } else if (nrow(eventdata) == 0) {
    return(NA)
  } else {
    attentiontime <- eventdata[[attentionpoint]]
    return(attentiontime)
  }
}