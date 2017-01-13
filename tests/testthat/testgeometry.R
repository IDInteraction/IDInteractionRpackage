context("Test geometry functions")

test_that("Area calculation works", {
  
  testunitsquare <- data.frame(x=c(0,1,1,0), y=c(0,0,1,1))
  
  expect_equal(calcArea(testunitsquare), 1)
  
  testsquare <- testunitsquare * 2
  
  expect_equal(calcArea(testsquare), 4)
  
  testtriangle <- data.frame(x=c(0,1,1), y=c(0,0,1))
  expect_equal(calcArea(testtriangle), 0.5)
  
  # Hexagon, radius 100.  Nodes calculated from
    testhexagon <- data.frame(x=100*cos( 2*seq(1:6)*pi/6 ), y=100* sin(2*seq(1:6)*pi/6))
  expect_equal(calcArea(testhexagon), 25980.76, tolerance=0.01)
  
  
  # Check invariant to translation
  transhexagon <- testhexagon + 100
  expect_equal(calcArea(transhexagon), calcArea(testhexagon))
  
               
  testoctagon <- data.frame(x=cos( 2*seq(1:8)*pi/8 ), y= sin(2*seq(1:8)*pi/8))
  
  expect_equal(calcArea(testoctagon) , 2.828, tolerance=0.01)
})