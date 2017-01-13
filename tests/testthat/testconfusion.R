context("Test confusion matrix etc.")

test_that("We can caluclate a confusion matrix", {
  
  testcase <- c(1,1,1,1,2,2,2)
  allcorrect = data.frame(observedAttentionName=testcase,
                         predclass=testcase)
  
  expect_equal(getConfusionMatrixPreds(allcorrect),
    structure(c(4L, 0L, 0L, 3L), .Dim = c(2L, 2L), .Dimnames = structure(list(
    c("1", "2"), c("1", "2")), .Names = c("obsdata", "preddata")), class = "table"))
  
  allwrong <- data.frame(observedAttentionName=testcase,
                         predclass=c(2,2,2,2,1,1,1))
  

  expect_equal(getConfusionMatrixPreds(allwrong),
               structure(c(0L, 3L, 4L, 0L), .Dim = c(2L, 2L), .Dimnames = structure(list(
                 c("1", "2"), c("1", "2")), .Names = c("obsdata", "preddata")), class = "table")
               )
  
  
  
})
