#' Read a depth file.  
#' 
#' This is a really simple format - one row per pixel.  The resolution of the depth frame is inferred from the 
#' number of rows, or can be specified manually with the width and height parameters.  By default, Kinect
#' depth camera (424x512) and full HD (1920x1080) frames are autodetected
#' 
#' @param infile The input file
#' @param width The width of the frame in pixels
#' @param height The height of the frame in pixels
#' 
#' @export
readDepth <- function(infile, width = 0, height = 0){
  
  
  depthData <- read.csv(infile, header = FALSE, col.names = "depth")
  if (width == 0 && height == 0) {
    # Guess dimensions
    if (nrow(depthData) == 512 * 424) {
      # Assuming depth frame 
      width = 512
      height = 424
      
    } else if (nrow(depthData) == 1920 * 1080) {
      width = 1920
      height = 1080
    } else {
      stop("Cannot guess frame dimensions")
    }
    
  }
  
  if (width * height != nrow(depthData)) {
    stop("Incorrect number of rows for specified resolution")
  }
  
  depthFrame <- data.frame(x = 1:width,
                           y = rep(1:height, each = width),
                           depth = depthData)
  
  depthFrame$depth <- ifelse(depthFrame$depth == 0, NA, depthFrame$depth)
  
  return(depthFrame)
  
}
