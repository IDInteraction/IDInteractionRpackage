# Functions related to loading and processing indata data

#' Read a data-set containing indata data
#' 
#' Read a data-set containing indata data.  This consists of one row per frame
#' containing the fields "frame", /v.x/, /v.y/
#' 
#' If vectors are numbered from 1 they are automatically renumbered to be zero indexed
#' for easiser comparison with the Python/C++ library, unless renumber = FALSE 
#' 
#' @param infile The input file name
#' @param renumber Whether to renumber nodes so zero indexed (default = TRUE)
#' 
#' @return A dataframe containing the indata data
#' 
#' @export
readLandmark <- function(infile, renumber = TRUE){
  
  indata <- read.csv(infile)
  innames <- names(indata)
  if(!("frame" %in% innames)){
   stop("frame column not found") 
  }
  
  if(any(na.exclude(stringr::str_match(innames, "p(\\d+)x")[,2]) != 
     na.exclude(stringr::str_match(innames, "p(\\d+)y")[,2]))){
    stop("Vertices must be in pairs")
  }
    
  
  if(renumber){
    if(min(as.numeric(stringr::str_match(innames, "p(\\d+)x")[,2]), na.rm=TRUE) == 0){
      print("Already zero indexed")
      break
    }
    
    nameparts <- stringr::str_match(innames, "(.+?)(\\d+)(.+?)")
    nameparts[,3] <- as.character(as.numeric(nameparts[,3]) -1 )
    
    reassemblednames <- apply(nameparts, 1, function(x) paste0(x[2:4], collapse=""))
    mask <- is.na(nameparts[,1])
    for(i in 1:length(mask)){
      if(mask[i]){
        reassemblednames[i] <- innames[i]
      }
    }

    names(indata) <- reassemblednames
    
  }
  
  indata <- indata[, names(indata) != "X"]
  
  return(indata)
  
}


#' Add features to the indata data set
#' 
#' Features are things like the area of each eye, face orientation etc
#' 
#' @param indata The input indata data-set
#' 
#' @return A dataframe containing each calculated feature
#' 
#' @export
calcLandmarkFeatures <- function(indata){
  
  righteye <- apply(indata, 1, calcAreaRowWise, vertices = 36:41)
  lefteye <-  apply(indata, 1, calcAreaRowWise, vertices = 42:47)
  nosedist <- apply(indata,1, calcDistanceRowWise, vertices=c(30,33))
  facerotation <- apply(indata,1, calcAngleRowWise, vertices = c(0,16))
  mouthrotation <- apply(indata,1, calcAngleRowWise, vertices = c(48,54))
  
  return(data.frame(frame=indata$frame,
                    righteye,
                    lefteye,
                    nosedist,
                    facerotation,
                    mouthrotation))
  
}

