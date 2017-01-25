
#' Load the spot the difference ground truth data, and clean
#' 
#' Load the ground truth data from the spot the differnece experiment.   Correct coding errors on load
#' 
#' @param inloc The folder containing the ground truth data
#' 
#' @return A data table contaning the ground truth data for all participants found in inloc
#' @export
loadSpotTheDifference <- function(inloc){
  
  
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
    
    
    thisattention$null <- NULL
    thisattention$participantCode <- participantCode
    
    attentions <- dplyr::bind_rows(attentions, thisattention)
    
  }
  
  # Correct typing errors
  attentions$annotation <- recodevalues(attentions$annotation, "TV_to_+tablet",
                                        "TV_to_tablet")
  attentions$annotation <- recodevalues(attentions$annotation, "Tv",
                                        "TV")
  attentions$annotation <- recodevalues(attentions$annotation, "start _tablet",
                                        "start_tablet")
  
  
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