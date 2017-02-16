#' Read in the timestamps spreadsheet, and parse it
#' 
#' Read in the spreadsheet containing the relative times, which we use to synchronise the sensor
#' and webcam times
#' 
#' @param infile The input file
#' 
#' @return The relative times of the various parts of the experiment, for each participant, in seconds
#' 
#' @export
#
loadTimestampsSheet <- function(infile = "~/IDInteraction/spot_the_difference/controlfiles/begin_experiment_timestamp - added relative times.csv") {
  timestamps <- read.csv(infile, header = FALSE, stringsAsFactors = FALSE, colClasses = "character")
  
  names(timestamps) <- c("participant",
                         "multiInputVideo",
                         "experimentStart",
                         "weblogStart",
                         "weblogStartDate",
                         "relativeEndTraining",
                         "endTraining",
                         "endTrainingDate",
                         "relativeStart1",
                         "start1",
                         "start1Date",
                         "relativeEnd1",
                         "end1",
                         "end1Date",
                         "relativeStart2",
                         "start2",
                         "start2Date",
                         "end2",
                         "end2Date"
  ) # Rest of columns aren't needed so don't bother to name them
  
  # Drop un-needed columns
  timestamps <- timestamps[, -which(is.na(names(timestamps) ))]
  timestamps <- timestamps[, -which(!is.na(stringr::str_match(names(timestamps), "Date$")))]
  
  # Drop header rows
  timestamps <- timestamps[-(1:5),]
  
  # Parse participant to participantCode
  timestamps$participantCode <- paste0("P", stringr::str_pad(stringr::str_match(timestamps$participant, "\\d+"), 2, "left", "0"))
  
  
  # If we have multiple times we only want the last; times are delmited by /s
  getLasttime <- function(indata){
    
    lasttime <- stringr::str_match(indata, "/(.+)$")
    consolidatedTimes <- ifelse(is.na(lasttime[,2]), indata, lasttime[,2])
    
    return(consolidatedTimes)
  }
  
  timestamps$multiInputVideo <- getLasttime(timestamps$multiInputVideo)
  
  # Convert everything to seconds
  timestamps$multiInputVideo <- as.numeric(lubridate::ms(timestamps$multiInputVideo))
  
  relativeTimeColumns <- names(timestamps)[!is.na(stringr::str_match(names(timestamps), "relative"))]
  
  for (r in relativeTimeColumns) {
    timestamps[,r] <- as.numeric(timestamps[,r])/1000
  }
  
  # Convert the epoch times to numbers
  epochColumns <- c("weblogStart", "endTraining", "start1", "end1", "start2", "end2")
  for (r in epochColumns) {
    timestamps[,r] <-  as.numeric(timestamps[,r])
  }
  
  return(timestamps)
}

#' Read in accelerometer or gyro data from a file, and add useful fields
#' 
#' @param infile The input file
#' 
#' @return A data frame containing the accelerometer data
readSensdataFile <- function(infile){
  
  # First row is incomplete
  sensdata <- read.csv(infile, header = TRUE, col.names = c("tabletTime", "eventTime", "sens1", "sens2", "sens3") )
  
  sensdata$sensabs <- with(sensdata, sqrt(sens1**2 + sens2**2 + sens3**2))
  sensdata$relTime <- (sensdata$eventTime - sensdata$eventTime[1])/1000
  
  # Timebase is very irregular;  appears to have been recorded at 20fps, but some readings are just a couple of ms apart.  Unsure of the best approach to handle this.  
  
  return(sensdata)
}

#' Read in sensor data, and synchronise to webcam time
#' 
#' @param participantCode The participant Code
#' @param sensortype The sensor type; acc or gyro
#' @param hand The hand; right or left
#' @param sensorloc The location of the (renamed) sensor files
#' @param renamecols Whether to rename sensNN to accNN or gyroNN as appropriate
#' 
#' @return The sensor data, with useful data fields added
#' @export
readSensData <- function(participantCode, sensortype, hand, 
                         sensorloc = "~/IDInteraction/spot_the_difference/sensors/",
                         renamecols = TRUE) 
{
  
  if (!(tolower(sensortype) %in% c("acc", "gyro")))
  {
    stop("Invalid sensor type given")
  } 
  
  if (!(tolower(hand) %in% c("left", "right")))
  {
    stop("Invalid hand given")
  }
  
  casehand <- ifelse(tolower(hand) == "left", "Left", "Right")
  
  sensfile <- paste0(sensorloc, participantCode, "_STREAM", tolower(sensortype), casehand, ".csv")
  
  sensdata <- readSensdataFile(sensfile)
  #rename columns to reflect data type
  if (renamecols)
  {
    colstems <- stringr::str_match(names(sensdata), "sens(abs|\\d+)")[,2]
    newnames <- ifelse(is.na(colstems), names(sensdata), paste0(tolower(sensortype), colstems))
    names(sensdata) <- newnames
  }
  
  
  
  timestamps <- loadTimestampsSheet()
  
  
  return(sensdata)
  
}


#' Map the sensor data to webcam time
#' 
#' We have epoch time for each row of the sensor data.  We need to convert this to webcam time, as used
#' in the rest of the code.  Furthermore, the sensor data was recorded at a different frame rate to the webcam,
#' and contains dropouts.   There are also some readings ~1ms apart (i.e >> quicker than the nominal framerate). 
#' We get a sensor reading for each webcam reading by selecting the reading closest to the frame number, and
#' performing linear interpolation where we have gaps
#' 
#' @param sensdata The sensor data
#' @param participantCode The participant the sensor data is for
#' @param timefunction The function to call to load the offset data
#' 
#' @return The sensor data, mapped to webcam time
#' 
#' @export
mapSensorData <- function(sensdata, participantCode, timefunction = loadTimestampsSheet){
  
  timestamps <- timefunction()
  
  # Webcamtime is the difference between the epoch time each sensor reading was made at minus the weblogstart epoch.  This gets us a relative time from weblogStart.  This time is "multiInputVideo" in the timestamps file
  sensdata$webcamTime <- (sensdata$eventTime - timestamps[timestamps$participantCode == participantCode, "weblogStart"])/1000 + timestamps[timestamps$participantCode == "P01", "multiInputVideo"]
  
  # And map to closest video frame
  sensdata$framefloat <- sensdata$webcamTime * 30
  sensdata$frame <- round(sensdata$framefloat)
  #  Some frames have > 1 set of accelrations associated with them, owing to the irregular capture frequency. 
  # drop frames before the webcam started
  sensdata <- sensdata[sensdata$frame > 0,]
  # Want to end up with 1 row per frame.  
  # Where we have more than one row per frame, keep the one closest to an integer number of frames
  sensdata$frameoffset <- abs(sensdata$frame - sensdata$framefloat)
  sensdata <- sensdata[order(sensdata$frame, sensdata$frameoffset),]
  dedup <- sensdata[!duplicated(sensdata$frame, fromLast = FALSE),]
  
  # Interpolate where we lack a sensor reading for the frame
  
  # First we make missing observations where we lack a frame
  allframes <- data.frame(frame = seq(min(dedup$frame), to = max(dedup$frame), by = 1))
  allframes <- dplyr::left_join(allframes, dedup, by = "frame" )
  
    colstems <- stringr::str_match(names(allframes), "^(acc|gyro|sens)(\\d+|abs)")[,1]
    interpcols <- names(allframes)[!is.na(colstems)]
  for (i in interpcols) {
    allframes[,i] <- zoo::na.approx(allframes[,i])
  }
    
  return(allframes)
}

