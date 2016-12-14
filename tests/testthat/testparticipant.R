context("Test participant detection")

test_that("Fails gracefully if no participants", {
  expect_error(getParticipantCodes("."))
  
})