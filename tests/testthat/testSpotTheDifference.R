context("Test spot the difference data")

test_that("We have a time for every row", {

  attentions <- loadSpotTheDifference("~/IDInteraction/spot_the_difference/attention/")
  expect_equal(sum(is.na(attentions$attTransMidss)),0)
  attentionsrecode <- loadSpotTheDifference("~/IDInteraction/spot_the_difference/attention/",
                                      keyfile="~/IDInteraction/spot_the_difference/controlfiles/transitionAnnotations.csv")
  
  expect_equal(sum(is.na(attentionsrecode$attTransMidss)),0)
  
  
  
})


test_that("We can handle offsets", {
  
  p01offset <- getKinectWebcamOffset("P01")
  expect_equal(p01offset, 882)
  
  
  expect_error(getKinectWebcamOffset("P99"))
  
  
})