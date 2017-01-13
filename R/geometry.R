#' Calculate the area of a non-self-intersecting polygon
#' 
#' No checking is performed to ensure the polygon is non-self-intersecting
#' 
#' @param verticies  A 2 column matrix or dataframe containing the verticies (x in column 1)
#' 
#' @return The area of the polygon
#' 
#' @export
calcArea <- function(vertices){
  # Algorithm taken from 
  # http://stackoverflow.com/questions/2553149/area-of-a-irregular-shape
  # https://en.wikipedia.org/wiki/Polygon#Area_and_centroid
  if(ncol(vertices) != 2){
    stop("Vertices need to be provided 1 per row")
  }
  
  summation <- 0
  for(i in 1: (nrow(vertices)-1) ){
    sumterm <- (vertices[i,1]*vertices[i+1,2] - vertices[i+1,1]*vertices[i,2] )
    summation <- summation + sumterm
  }
  
  sumterm <- vertices[nrow(vertices),1]*vertices[1,2] - vertices[1,1]*vertices[nrow(vertices),2]
  summation <- summation + sumterm
  
  area <- abs(summation) * 0.5
  
  return(area)
  
}
