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



#' Calculate the area for a set of points stored horizontally
#' 
#' This function is typically used with apply, e.g. 
#' apply(vertexfile, 1, extractverticies, 31:36) to calculate the area of the right eye
#' 
#' @param inrow A row of input data
#' @param vertices The vertices to extract
#' 
#' @return The area enclosed by the selected vertices
#' 
#' @export
calcAreaRowWise <- function(inrow, vertices){
  x <- inrow[paste0("p", vertices, "x")]
  y <- inrow[paste0("p", vertices, "y")]
  
  calcArea(cbind(x,y))
}


#' Calculate the distance between two vertices
#' 
#' @param inrow A row of input data
#' @param vertices The pair of vertices to calculate the distance between
#' 
#' @return The distance between the vertices
#' 
#' @export
calcDistanceRowWise <- function(inrow, vertices){
  if(length(vertices) !=2){
    stop("Can only calculate the distance betweeen 2 vertices")
  }
  x <- as.vector(inrow[paste0("p", vertices, "x")])
  y <- as.vector(inrow[paste0("p", vertices, "y")])
  
  distance <- sqrt((x[1]-x[2])**2 + (y[1]-y[2])**2)
  
  return(distance)
}


#' Calculate the angle (from horizontal) between two vertices
#' 
#' @param inrow A row of input data
#' @param vertices The pair of vertices to calculate the angle from horizontal
#' 
#' @return The angle from horizontal in radians
#' 
#' @export
calcAngleRowWise <- function(inrow, vertices){
  if(length(vertices) !=2){
    stop("Can only calculate the distance betweeen 2 vertices")
  }
  
  x <- inrow[paste0("p", vertices, "x")]
  y <- inrow[paste0("p", vertices, "y")]
  
  deltax <- as.numeric(x[2] - x[1])
  deltay <- as.numeric(y[2] - y[1])
  
  if(deltax == 0 && deltay == 0){
    return(NA)
  }else{
    return(atan2(deltay, deltax))
  }
  
}