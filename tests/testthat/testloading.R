context("Test loading data")

trackingLoc <- "~/IDInteraction/dual_screen_free_experiment/tracking/high_quality/front_full_face/"
annoteLoc <- "~/IDInteraction/dual_screen_free_experiment/attention/"


test_that("Names are unique in loaded data", {
  
  trackdata <- loadParticipantTrackingData(paste0(trackingLoc, "P01_video.csv"))
  expect_equal(length(names(trackdata)), length(unique(names(trackdata))))

  annotedata <- loadParticipantAnnotationData(paste0(annoteLoc, "P01-timings.csv"))
  expect_equal(length(names(annotedata)), length(unique(names(annotedata))))
  
  annotetrack <- annotateTracking(trackdata, annotedata)
  expect_equal(length(names(annotetrack)), length(unique(names(annotetrack))))
  
  featureDF <- createFeatureDF(annotetrack, participantCode = "P01")
  expect_equal(length(names(featureDF)), length(unique(names(featureDF))))
  
})



test_that("We don't get duplicate columns in createTrackingAnnotation",{
  
  TA <- createTrackingAnnotation("P01",
                           trackingLoc = trackingLoc,
                           annoteLoc = annoteLoc)
  
  expect_equal(length(names(TA)), length(unique(names(TA))))
  
})

test_that("We can load data", {
exptdata<-loadExperimentData("P01", 
                             trackingLoc = trackingLoc,
                             annoteLoc = annoteLoc)


expect_equal(length(names(exptdata)),length(unique(names(exptdata))))
table(names(exptdata))
})