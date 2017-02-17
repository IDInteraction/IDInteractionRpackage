#' Load the web touch log file
#' 
#' @param infile The input filename
#' 
#' @return The web toch log 
#' 
#' @export
readWebTouchLog <- function(infile = "~/IDInteraction/spot_the_difference/web/idinteractionTouches.log")
{
  
  wtl  <- read.csv(infile,
                   col.names = c("serverTime",
                                 "label",
                                 "indexofdifference",
                                 "xcoord",
                                 "ycoord",
                                 "pageindex",
                                 "imageindex",
                                 "tabletTime"),
                   stringsAsFactors = FALSE,
                   header = FALSE)
  
  # Extract a participant if we are able to
  participants <- stringr::str_match(tolower(wtl$serverTime),"participant\\s?(\\d+)" )[,2]
  # Horrid hack to deal with first participant
  participants  <- ifelse(participants == "11468228385059", 1, participants)
  
  # Check we've got all participants
  goodrows <- participants[!is.na(participants)]
  if (!all(seq_along(goodrows) == as.numeric(goodrows))) {
    stop("Lost participant")
  }
  
  
  #Carry forward participant and pad to standard format
  wtl$participantCode <- stringr::str_pad(as.numeric(zoo::na.locf(participants)),2,"left", pad = "0")
  
  # TODO - deal with serverTime; just drop for now since we don't need it
  
  wtl$serverTime <- NULL
  
  return(wtl)
}