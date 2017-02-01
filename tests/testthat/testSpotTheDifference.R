context("Test spot the difference data")

test_that("We can load the data", {
  
  attentions <- loadSpotTheDifference("~/IDInteraction/spot_the_difference/attention/")
  expect_equal(sum(is.na(attentions$attTransMidss)),0)
  
  attentionsrecode <- loadSpotTheDifference("~/IDInteraction/spot_the_difference/attention/",
                                            keyfile="~/IDInteraction/spot_the_difference/controlfiles/transitionAnnotations.csv")
  expect_equal(sum(is.na(attentionsrecode$attTransMidss)),0)
  
  
  expect_equal(length(unique(attentions$eventtype)), 2)
  expect_equal(length(unique(attentionsrecode$eventtype)), 3)
  
  # Expect each type of annotation to be unique in time
  for(p in unique(attentions$participantCode)){
    for(a in unique(attentions$eventtype)){
      thisp <- attentions[attentions$participantCode==p & attentions$eventtype==a,]
      expect_equal(length(unique(thisp$attTransMidss)), length(thisp$attTransMidss), info=paste(p,a))
    }
  }
  
  for(p in unique(attentionsrecode$participantCode)){
    for(a in unique(attentionsrecode$eventtype)){
      thisr <- NULL
      thisr <- attentionsrecode[attentionsrecode$participantCode==p & attentionsrecode$eventtype == a,]
      #print(paste(p, a,length(unique(thisr$attTransMidss)), length(thisr$attTransMidss)))
      expect_equal(length(unique(thisr$attTransMidss)), length(thisr$attTransMidss), info=paste(p,a))
    }
  }
  
  
})


test_that("We can handle offsets", {
  
  p01offset <- getKinectWebcamOffset("P01")
  expect_equal(p01offset, 882)
  
  
  expect_error(getKinectWebcamOffset("P99"))
  
  
})