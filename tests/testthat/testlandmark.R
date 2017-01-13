context("Test landmark data")

test_that("We can load landmark data",{
  
  landmark <- readLandmark("landmarkExtract.csv", renumber = TRUE)
  expect_equal(names(landmark)[2], "p0x")
  
  landmark2 <- readLandmark("landmarkExtract.csv", renumber = FALSE)
  expect_equal(names(landmark2)[2], "p1x")
  
  
  expect_equal(readLandmark("landmarkExtract.csv", renumber = TRUE),
               readLandmark("landmarkExtract.csv"))
  
})



test_that("We can calculate landmark features", {
  landmark <- readLandmark("landmarkExtract.csv", renumber = TRUE)
  features <- calcLandmarkFeatures(landmark)
  expect_equal(nrow(features), nrow(landmark))
  expect_equal(features$frame, features$frame)
})