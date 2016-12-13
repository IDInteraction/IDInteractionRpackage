#' Load a series of points from a Cpp-mt run (generate with --output-points=outfile.csv on
#' plotpoints branch)
#' 
#' @param infile The input csv file
#' @return A data frame containing frame number, x, y for each point on each frame
#' 
#' @export
loadpoints <- function(infile){
  pointfile <- read.csv(infile, col.names = c("frame","x","y"))
  
  
  return(pointfile)
                        
}


#' Get the number of detected points per frame
#' 
#' @param inpoints A data frame of points
#' 
#' @return a data frame containing the number of points in each frame
#' 
#' @export
getPointsPerFrame <- function(inpoints){

  pointsperframe <- data.frame(table(trackpoints$frame))
  colnames(pointsperframe) <- c("frame", "points")
  
  return(pointsperframe)
    
}


# trackpoints <- loadpoints("../testprocess/testpoints.csv")
# ppf <- getPointsPerFrame(trackpoints)
# 
# with(ppf, plot(frame, points))
# 
# by(trackpoints, factor(trackpoints$frame), function(x){plot(x$x, x$y, xlim=c(260,360), ylim=c(50,120))})
