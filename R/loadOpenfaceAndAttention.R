#' Read an openface file
#' 
#' @param infile
#' 
#' @return The open face data
#' 
#' @export
readOpenFace <- function(infile){
  
  openface <- read.csv(infile)
  
  
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
                                     participantCode ){
  
  if (!(videosource %in% c("kinect", "webcam")))
  {
    stop("Video source must be kinect or webcam")
  }
  
  
  
  attentions <- loadSpotTheDifferenceFile(attentionfile,
                                          keyfile = keyfile,
                                          participantCode = participantCode)
  
  
  openface <- readOpenFace(openfacefile)
  
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