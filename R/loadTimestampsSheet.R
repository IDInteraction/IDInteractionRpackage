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


#' Remove spurious (?) "STREAM" entries from sensor data
#' 
#' @param indata  Some sensor data
#' @param verbose Whether to report what we're doing
#' 
#' @return The sensor data with invalid readings removed
#' 
#' At the moment we just look for "STREAM" in the tabletTime column, and (optionally) report
cleanSensorData <- function(indata, verbose = TRUE){
 # Warnings suppressed since the creation of the NAs will throw a warning 
  badrows <- suppressWarnings(is.na(as.numeric(indata$tabletTime)))
  
  if (verbose) {
    print("The following rows will be removed")
    print(indata[badrows,])
  }
  
  indata <- indata[!badrows, ]
  
  indata$tabletTime <- as.numeric(indata$tabletTime)
  
  return(indata)
}


#' Read in accelerometer or gyro data from a file, and add useful fields
#' 
#' @param infile The input file
#' 
#' @return A data frame containing the accelerometer data
readSensdataFile <- function(infile){
  
  # First row is incomplete
  sensdata <- read.csv(infile, header = TRUE,
                       col.names = c("tabletTime", "eventTime", "sens1", "sens2", "sens3"),
                       stringsAsFactors = FALSE)
  
  sensdata$sensabs <- with(sensdata, sqrt(sens1**2 + sens2**2 + sens3**2))
  sensdata$relTime <- (sensdata$eventTime - sensdata$eventTime[1])/1000
  
  # Check each column is numeric
  needclean = FALSE
  for (n in names(sensdata)) {
    if (!is.numeric(sensdata[,n])) {
      warning(paste("Non numeric data found in column:", n))
      needclean = TRUE
    }
  }
  if (needclean) { 
    sensdata <- cleanSensorData(sensdata)
  }
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
  
  
  return(sensdata)
  
}

#' Convert an epoch time to a webcam time, for a given participant
#' 
#' Given an epoch timestamp and a participantCode, what's the time in the webcam timeframe?
#' 
#' @param epochtime The epoch timestamp
#' @param participantCode The participant code
#' @param timefunction The function to call to load the offset data
#' 
#' @return The corresponding webcam time, in seconds
#' 
#' @export
epochToWebcam <- function(epochtime, participantCode, timefunction=loadTimestampsSheet){
  
  
  timestamps <- timefunction()
  weblogStartEpoch <- timestamps[timestamps$participantCode == participantCode, "weblogStart"] 
  weblogStartWebcam <- timestamps[timestamps$participantCode == participantCode, "multiInputVideo"] * 1000
  
  eventtimeMS <- epochtime - weblogStartEpoch + weblogStartWebcam
  
  eventtime <- eventtimeMS / 1000
  return(eventtime)
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
#' @param webcamFps The frame rate of the webcam stream
#' @param maxgap The maximum number of NAs to interpolate between
#' 
#' @return The sensor data, mapped to webcam time
#' 
#' @export
mapSensorData <- function(sensdata, participantCode, timefunction = loadTimestampsSheet,
                          webcamFps = 30, maxgap = 3){
  # Convert the epoch time to webcam time
  sensdata$webcamTime <- epochToWebcam(sensdata$eventTime, participantCode)
  
  # And map to closest video frame
  sensdata$framefloat <- sensdata$webcamTime * 30
  sensdata$frame <- round(sensdata$framefloat)
  # drop frames before the webcam started
  sensdata <- sensdata[sensdata$frame > 0,]

  
  # Our sensor data is irregularly sampled (or has an irregular timestamp), contains dropouts and 
  # is sampled at a different rate to the webcam video.  
  # WE ASSUME THE EVENTTIME IS CORRECT - this (probably) isn't (quite) the case
  
  # We make a list of webcamtimes corresponding to our webcam frame rate, add these into the time series and use
  # the zoo package to interpolate between the missing values (corresponding to webcam time) and the observed
  
  
  # There are some timestamps where we have > 1 reading; i.e. two events have arrived within
  # 1ms 
  # We handle these by taking the average of each of them
  # ett <- table(sensdata$eventTime)
  #length( ett[ett>1])
  
  deduped <- list()
  tsfields <- c("acc1", "acc2", "acc3", "accabs")
  for (tsfield in tsfields) {
    # Warnings suppressed since we know we have duplicate timestamps
    thisSeries <- suppressWarnings(zoo::zoo(sensdata[,tsfield], sensdata$webcamTime))
    deduped[[tsfield]] <- aggregate(thisSeries, identity, mean)
    
  }
  
  dedupeddf <- data.frame(deduped)
  dedupeddf$webcamtime <- time(deduped[[1]])
  
  dedupeddf$actualFrameTime <- dedupeddf$webcamtime
  
  frameTimes <- data.frame(actualFrameTime = seq(from = 0,
                                                 to = max(sensdata$webcamTime),
                                                 by = 1/webcamFps),
                           webcamFrame = TRUE
  )
  
  
  
  
  joinedseries <- dplyr::full_join(dedupeddf, frameTimes, by = "actualFrameTime")
  joinedseries <- joinedseries[order(joinedseries$actualFrameTime, joinedseries$webcamFrame),]
  
  interpolated <- list()
  for (tsfield in tsfields) {
    thisSeries <- zoo::zoo(joinedseries[, tsfield], order.by = joinedseries$actualFrameTime)
    interpolated[[tsfield]] <- zoo::na.approx(thisSeries, na.rm = FALSE, maxgap = maxgap)
  }
  
  
  interpolateddf <- data.frame(interpolated)
  interpolateddf$actualFrameTime <- joinedseries$actualFrameTime
  interpolateddf$webcamFrame = joinedseries$webcamFrame
  
  allframes <- interpolateddf[!is.na(interpolateddf$webcamFrame),]
  

  return(allframes)
}

