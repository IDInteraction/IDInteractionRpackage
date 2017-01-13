context("Test landmark data")

test_that("We can load landmark data",{
  
  landmark <- readLandMark("landmarkExtract.csv", renumber = TRUE)
  expect_equal(names(landmark)[2], "p0x")
  
  landmark2 <- readLandMark("landmarkExtract.csv", renumber = FALSE)
  expect_equal(names(landmark2)[2], "p1x")
  
  
})


