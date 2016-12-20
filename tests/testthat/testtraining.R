context("Test training")

test_that("We flag the right amount of training data", {
  
  dummydata = data.frame(timestampms = seq(from=0, to= 60*1000*3, by = 20 ), dummyval = 1)
  
  expect_equal(max(dummydata[flagtraining(dummydata, 1),1]), 60000)
  
})
