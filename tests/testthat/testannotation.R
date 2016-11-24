context("Test annotation")

test_that("Annotation of attentions works", {
testannote <- data.frame(time = c(0, 500, 1000),
                         attentionlocation = c(1, 2, 1))


expect_equal(getattention(20, testannote), 1)
expect_equal(getattention(500, testannote), 2) 
expect_equal(getattention(501, testannote), 2)
expect_equal(getattention(999, testannote), 2)
expect_equal(getattention(1000, testannote), 1)
expect_equal(getattention(1001, testannote), 1)


})