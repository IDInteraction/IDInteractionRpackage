#' Output ground truth or model prediction data for a data-set, to allow verification with abc-dt
#' 
#' @param attentions A vector containing the attentions.  Will be converted to numeric
#' @param frames A vector the same length as attentions containing the frame #s to output
#' @param outfile The name and location of the file to output
#' @param bbox The size and position of the bounding box to output: (x,y,w,h)
#' 
#' @export
writeAttentions <- function(
  attentions,
  frames,
  outfile,
  bbox=c(200,200,150,150))
{
  
  if ( length(bbox) != 4 )
  {
    stop("Bounding box is specified as (x,y,w,h)")
  }
  
  if ( length(attentions) != length(frames) )
  {
    stop("Attentions and frames must be the same length")
  }
  
  if ( is.factor(attentions) )
  {
    attentions <- as.numeric(attentions)
  }
  
  if ( min(attentions) > 0 )
  {
    attentions <- attentions - min(attentions)
  }
  
  outdata <- data.frame(frame = frames,
                        x = bbox[1],
                        y = bbox[2],
                        w = bbox[3],
                        h = bbox[4],
                        attention = attentions)
  
  write.table(outdata,
              file = outfile,
              row.names = FALSE,
              col.names = FALSE, sep = "," )
  
  
  
}