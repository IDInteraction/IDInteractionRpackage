#' Load a sport the difference ground truth data file, and clean
#' 
#' @param infile The file containing the ground truth data for a participant
#' @param keyfile An optional CSV file specifying how to recode annotations
#' @param participantCode The participant code
#' 
#' @return A data table containing the ground truth data for the participant
#' 
#' @export
loadSpotTheDifferenceFile <- function(infile, keyfile = NULL, participantCode)
{
  
  col_names = c("eventtype",
                "null",
                "attTransStarthms",
                "attTransStartss",
                "attTransStartms",
                "attTransEndhms",
                "attTransEndss",
                "attTransEndms",
                "attDurationhms",
                "attDurationss",
                "attDurationms",
                "annotation")
  # Participant 16 doesn't have times in ms recorded
  col_names16 = c("eventtype",
                  "null",
                  "attTransStarthms",
                  "attTransStartss",
                  #"attTransStartms",
                  "attTransEndhms",
                  "attTransEndss",
                  #"attTransEndms",
                  "attDurationhms",
                  "attDurationss",
                  #"attDurationms",
                  "annotation")
  
  attentions <- readr::read_delim(infile, 
                                  delim = "\t",
                                  col_names = (if (participantCode == "P16") {col_names16}else{col_names})
  )
  
  attentions$participantCode <- participantCode
  
  
  # Just keep the columns we have in all files (ms timestamps aren't used anyway)
  attentions <- attentions[,c(col_names16, "participantCode")]
  
  # Remove hms timestamps, since not used
  attentions <- attentions[, names(attentions)[is.na(stringr::str_match(names(attentions), "hms"))]]
  
  # remove dummy column
  attentions <- attentions[,!(names(attentions)=="null")]
  
  # Correct typing errors
  attentions$annotation <- recodevalues(attentions$annotation, "TV_to_+tablet",
                                        "TV_to_tablet")
  attentions$annotation <- recodevalues(attentions$annotation, "Tv",
                                        "TV")
  attentions$annotation <- recodevalues(attentions$annotation, "start _tablet",
                                        "start_tablet")
  
  # Aitor started coding participant 1 with these longhand names; they should be coded as follows
  # (confirmed with him 25 Jan)
  attentions$annotation <- recodevalues(attentions$annotation, "TV_to_tablet",
                                        "tablet")
  
  attentions$annotation <- recodevalues(attentions$annotation, "tablet_to_TV",
                                        "TV")
  
  # Eventtype isn't consistently coded annotation/annotations:
  attentions$eventtype <- recodevalues(attentions$eventtype, "annotations", "annotation")
  
  if(sum(is.na(attentions$annotation)) > 0){
    warning(paste(sum(is.na(attentions$annotation)), "missing annotations.  Dropping observations"))
    attentions <- attentions[!is.na(attentions$annotation),]
  }
  
  if(!all(complete.cases(attentions))){
    stop("Missing data not allowed")
  }
  
  # Generate events for start and end of each part of the experiment
  # These are constructed by matching on a (unique) annotation in the original file,
  # or by generating a new event at the specified timestamp.
  if (!is.null(keyfile)) {
    
    eventkey <- readr::read_csv(keyfile)
    
    # We handle events we match by annotation and events we generate by timestamp separately
    if (!("participantCode" %in% names(eventkey)))
    {
      stop("participantCode not found in input file")
    }
    if (!("event" %in% names(eventkey)))
    {
      stop("event not found in input file")
    }
    
    if(any(!xor(!is.na(eventkey$annotation), !is.na(eventkey$timestamp))))
    {
      stop("Can match by annotation or timestamp, nto both")
    }
    
    # Subset to just the participant we're working on
    eventkey <- eventkey[eventkey$participantCode == participantCode,]
    
    
    if ("annotation" %in% names(eventkey))
    {
      
      eventkeyByEvent <- eventkey[!is.na(eventkey$annotation),c("participantCode", "event", "annotation")]
      if (nrow(eventkeyByEvent) > 0)
      {
        if (!all(complete.cases(eventkeyByEvent))) {
          stop("Missing data not allowed in keyfile")
        }
        
        applyevents <- function(x){
          
          matchrowmask <- attentions["participantCode"] == x["participantCode"] & 
            attentions$annotation == x["annotation"]
          if(any(is.na(matchrowmask))){
            stop("NAs found in matchrowmask")
          }
          
          
          if(sum(matchrowmask) > 1){
            stop(paste("Matched more than one event for", x["participantCode"], ":", x["annotation"]))
          }
          if(sum(matchrowmask) == 0){
            warning(paste("couldn't match event for", x["participantCode"], ":", x["annotation"]))
          }else{
            
            matchrow <- attentions[matchrowmask, ]
            matchrow$eventtype <- "event"
            matchrow$annotation <- x["event"]
            # Note - global assing
            attentions <<- rbind(attentions, matchrow)
            # Replace row, to keep uniqueness of times
            #attentions[matchrowmask, ] <<- matchrow
          }
          
        }
        
        apply(eventkeyByEvent, 1,applyevents)
      }
    }
    if ("timestamp" %in% names(eventkey))
    {
      
      eventkeyByTimestamp <- eventkey[!is.na(eventkey$timestamp),c("participantCode", "event", "timestamp")]
      
      if (nrow(eventkeyByTimestamp) > 0) 
      {
        timestampevents <- data.frame(
          eventtype = "event",
          participantCode = eventkeyByTimestamp$participantCode,
          attTransStartss = eventkeyByTimestamp$timestamp,
          attTransEndss = eventkeyByTimestamp$timestamp,
          attDurationss = 0,
          annotation = eventkeyByTimestamp$event)
        
        attentions <- rbind(attentions,timestampevents)
      }
    }
    
    
  }
  # Calculate the midpoint of each transition period 
  attentions$attTransMidss <- attentions$attTransStartss + 
    (attentions$attTransEndss - attentions$attTransStartss)/2
  
  return(attentions)
  
}



#' Load the spot the difference ground truth data, and clean
#' 
#' Load the ground truth data from the spot the differnece experiment.   Correct coding errors on load and standardise events
#' 
#' @param inloc The folder containing the ground truth data
#' @param keyfile An optional CSV file specifying how to recode annotations
#'
#' 
#' 
#' @return A data table contaning the ground truth data for all participants found in inloc
#' @export
loadSpotTheDifference <- function(inloc, keyfile = NULL){
  attentions <- NULL
  
  for (f in list.files(inloc)) {
    
    p <- stringr::str_extract(f, "P\\d\\d")[1]    
    
    thisattention <- loadSpotTheDifferenceFile(paste0(inloc, f), keyfile = keyfile,
                                               participantCode = p)
    
    
    attentions <- dplyr::bind_rows(attentions, thisattention)
  }
  
  return(attentions)
}


#' Annote a tracking file with spot_the_difference attentions
#' 
#' Given a time, return the participant's attention at that time.  Requires a table,
#' SORTED BY TIME, containing the participnts attention
#' 
#' TODO Make this function generic with getattention()
#' 
#' @param time The time to get the attention for, in seconds
#' @param annoteset The data containing the ground truth
#' @param annoteTimeColumn The column of annoteset that contains the timestamps
#' @param annoteAttentionColumn The column of annoteset that contains the attentions
#' @param eventtypeColumn The type of event being annotated.
#' @return The attention at the requested time
#' @export
getattention2 <- function(time, annoteset, annoteTimeColumn = "attTransMidss",
                          annoteAttentionColumn = "annotation",
                          eventtypeColumn = "attention",
                          keepannotations = NULL){
  
  
  if(!is.null(keepannotations)){
    annoteset <- annoteset[annoteset$eventtype == eventtypeColumn,]
  }
  
  
  if(time < 0){
    return(NA)
  }
  
  earliertimes <- annoteset[,annoteTimeColumn] <= time
  # Return first attention in file, since annotation notes *changes* 
  if(sum(earliertimes) == 0){ 
    attention <- annoteset[1,annoteAttentionColumn]
  }else{
    attention <- tail(annoteset[earliertimes, annoteAttentionColumn], n=1)
  }
  
  if("tbl_df" %in% class(attention)){
    return (attention[[1]])
  }else{
    return(attention)
  }
}


#' Get the frame offset for a kinect video wrt the webcam video
#' 
#' @param participantCode The participant code
#' @param offsetfile The file containing the offsets; this shouldn't need to be changed
#' TODO Make this a proper option
#' 
#' @return The number of frames to offset by.  Will stop if participant not found
#' @export
#' 
getKinectWebcamOffset <- function(participantCode,
                                  offsetfile="~/IDInteraction/spot_the_difference/controlfiles/frameoffsets.csv")
{
  
  offsets <- read.csv(offsetfile, stringsAsFactors = FALSE)
  
  frameskip <- offsets[offsets$participantCode==participantCode, "delta"]
  if(length(frameskip) == 0){
    stop("Cannot find participant")
  }
  return(frameskip)
}



#' Return the annotations occuring between two events
#' 
#' @param sortattentions The attention file for a participant, sorted by time
#' @param event1 The event to start the extraction at
#' @param event2 The event to end the extraction at
#' @param sortcheck The variable to use to check sorted
#' @return A data-frame contaning the required frames
#' 
#' @export
extractBetweenEvents <- function(sortattentions, event1, event2, sortcheck = "attTransMidss"){
  # TODO test this is trapping correctly
  if(is.unsorted(sortattentions[,sortcheck]))
    stop("Data isn't sorted")
  
  
  if(length(unique(sortattentions$participantCode))> 1){
    stop("Function only works on a single participant")
  }
  
  event1index <- which(sortattentions$eventtype == "event" & sortattentions$annotation == event1)
  event2index <- which(sortattentions$eventtype == "event" & sortattentions$annotation == event2)
  
  
  if(is.na(event1index)){
    stop("Cannot find start event")
  }
  if(is.na(event2index)){
    stop("Cannot find end event")
  }
  if(length(event1index) > 1 || length(event2index) > 1){
    stop(">1 event found")
  }
  
  
  if(event1index > event2index){
    stop("Event 2 happens before event 1")
  }
  
  return(sortattentions[event1index:event2index,])
  
}


#' Encode transitions
#' Record the transitions between attentions as separate states
#' 
#' TODO - pass column names as parameters
#' 
#' 
#' @param indata The input data set for a single participant
#' 
#' @return The attentions, inclduding transitions
#' 
#' @export
encodeTransitions <- function(indata){
  
  if("participantCode" %in% names(indata)){
    if(length(unique(indata$participantCode)) > 1){
      stop("Can only encode transitions for a single participant")
    }
  }
  
  attentiondata <- indata[, c("attTransStartss", "attTransEndss", "annotation")]
  
  
  lagset <- attentiondata %>% dplyr::mutate(lagStart=lag(attTransStartss), 
                                            lagEnd=lag(attTransEndss),
                                            lagAnnotation=lag(annotation))
  
  
  
  steadyactivities <- lagset[, c("attTransEndss", "annotation")] %>% dplyr::rename(timeStamp = attTransEndss )
  
  lagset <- lagset %>% dplyr::mutate(newannote= paste0(lagAnnotation, "__", annotation))
  
  transactivities <- lagset[,c("attTransStartss", "newannote"), ] %>% dplyr::rename(timeStamp = attTransStartss) %>% dplyr::rename(annotation = newannote)
  
  combinedset <- dplyr::bind_rows(steadyactivities, transactivities)
  
  combinedset <- combinedset[ order(combinedset$timeStamp)[-1],]
  
  return(combinedset)
  
}



#' Convert a kinect timestamp to a webcam timestamp
#' 
#' This function converts a kinect timestamp to a webcam timestamp.  We need to take account of the offset
#' at the start of the video files (due to each camera starting to record at a different time), and take
#' account of the fact that there are some missing frames in the Kinect data.  (If there are missing frames
#' in the webcam this doesn't matter since ground truth is encoded with respect to webcam time)
#' 
#' @param KinectVideoTimes A vector containing the time recorded for each frame in the output video.  This will be (frame number - 1)/fps
#' @param KinectTimeStamps A vector containing the time recorded *by the kinect* for each frame in the output video.  These were generated when the frames were extracted
#' @param KinectOffsetFrames The offset in frames (kinect - webcam) between the start of the Kinect video and the start of the webcam video.  This needs to be derived by hand by comparing frames in both videos
#' @param fps The framerate of the videos (must be the same for both)
#' @return A vector containing the webcam time for each KinectVideoTime. These times can be used to determine the attention in each Kinect video frame
#' 
#' @export
kinectTimeToWebcamTime <- function(KinectVideoTimes, KinectTimeStamps=NULL, KinectOffsetFrames, fps=30){
  
  
  
  if(!is.null(KinectTimeStamps)){
    
    if(length(KinectTimeStamps) != length(KinectVideoTimes)){
      stop(paste("Actual video timestamps and extracted timestamps must be the same length",
                 "Loaded timestamps:", length(KinectTimeStamps),
                 "Video times:", length(KinectVideoTimes)))
    }
    
    
    # Frame-by-frame comparison of times
    # We substract the first extracted time off all others, since the API
    # outputs an (apparently) arbitrary offset to the video file
    
    KinectVideoTimes=KinectTimeStamps-KinectTimeStamps[1]
    
  }
  
  
  KinectOffsetSeconds <- KinectOffsetFrames/30
  
  offsettime <-  offsetTime(KinectVideoTimes, KinectOffsetSeconds)
  
  return(offsettime)
}


#' Load the time of each frame, as recorded by the Kinect
#' 
#' Note that the times returned are offset by an (apparently) arbitrary amount
#' 
#' @param participantCode The code of the participant whose data we wish to load
#' @param frameloc The location of the framelist files
#' 
#' @return A vector of times
#' 
#' @export
getKinectFrameTimes <- function(participantCode,
                                frameloc = "~/IDInteraction/spot_the_difference/kinect/"){
  
  framefile <- paste0(frameloc, participantCode, "_framelist.csv")
  
  frames <- read.csv(framefile, header = FALSE, col.names = c("frame", "time"))
  
  if (!all(seq_along(frames$frame) == frames$frame))
  {
    stop("Missing frames in input file")
  }
  
  
  return(frames$time)
}