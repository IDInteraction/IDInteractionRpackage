
#' Load the spot the difference ground truth data, and clean
#' 
#' Load the ground truth data from the spot the differnece experiment.   Correct coding errors on load
#' 
#' @param inloc The folder containing the ground truth data
#' @param keyfile An optional CSV file specifying how to recode annotations
#'
#' keyfile must 
#' 
#' @return A data table contaning the ground truth data for all participants found in inloc
#' @export
loadSpotTheDifference <- function(inloc, keyfile = NULL){
  
  
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
  
  
  attentions <- NULL
  
  for(f in list.files(inloc)){
    
    participantCode <- stringr::str_match(f, "^(P\\d+)_")[2]
    
    thisattention <- readr::read_delim(paste0(inloc, f), 
                                       delim="\t",
                                       col_names = (if(participantCode=="P16"){col_names16}else{col_names})
    )
    
    

    thisattention$participantCode <- participantCode
    
    attentions <- dplyr::bind_rows(attentions, thisattention)
    
  }
  
  # Just keep the columns we have in all files (ms timestamps aren't used anyway)
  attentions <- attentions[,c(col_names16, "participantCode")]
  
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
  # These are a copy of the annotation line, but coded consistently
  if(!is.null(keyfile)){
    eventkey <- readr::read_csv(keyfile)
    
    if(!all(complete.cases(eventkey))){
      stop("Missing data not allowed in keyfile")
    }
    
    applyevents <- function(x){
      
      matchrowmask <- attentions["participantCode"] == x["participantCode"] & 
        attentions$annotation == x["annotation"]
      #TODO - test no NAs in matchrowmask
      
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
        attentions <<-rbind(attentions, matchrow)
        
      }
      
    }
    
    apply(eventkey, 1,applyevents)
    
    
    
  }
  # Calculate the midpoint of each transition period 
  attentions$attTransMidss <- attentions$attTransStartss + (attentions$attTransEndss - attentions$attTransStartss)/2
  
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
#' 
#' @return The attention at the requested time
#' @export
getattention2 <- function(time, annoteset, annoteTimeColumn = "attTransMidss",
                          annoteAttentionColumn = "annotation"){
  
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
  
  frameskip <- offsets[participantCode, "delta"]
  
  return(frameskip)
}