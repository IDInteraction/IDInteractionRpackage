context("Test we can handle the skipped frames in the Kinect video properly")

test_that("Skipped kinect frames can be correctly recoded", {
  
  KinectVideoTimes <- seq(from = 0, to = 2, by = 1/30)
  KinectOffsetSeconds  <- 0.5
  KinectOffsetFrames <- KinectOffsetSeconds * 30
  
  KinectTimeStamps <- seq(from = 0, to = 2 + (1/30), by = 1/30)
  KinectTimeStamps <- KinectTimeStamps[-5] # Drop a frame
  
  expect_equal(KinectVideoTimes + KinectOffsetSeconds, 
               kinectTimeToWebcamTime(KinectVideoTimes,
                                      KinectOffsetFrames = KinectOffsetFrames))
  
  
  # This test is only OK because we start the KinectTimeStamps from 0
  expect_equal(kinectTimeToWebcamTime(KinectVideoTimes, KinectTimeStamps, 0),
               KinectTimeStamps)
  
  expect_equal(kinectTimeToWebcamTime(KinectVideoTimes, KinectTimeStamps, KinectOffsetFrames ),
               KinectTimeStamps + KinectOffsetSeconds)
  
  
})


