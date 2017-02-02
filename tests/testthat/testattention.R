context("Test attention coding")

test_that("We can get attentions", {
  attentions <- loadSpotTheDifference("~/IDInteraction/spot_the_difference/attention/",
                                      keyfile = "~/IDInteraction/spot_the_difference/controlfiles/transitionAnnotations.csv")
  
  participantCode = "P07"
  
  thisParticipanAttention <- attentions[attentions$participantCode == participantCode,]
  
  
  expect_equal(getattention2(0, thisParticipanAttention),
               "participant starts experiment") # Since gets attention over all annotation types
  
  # TODO test fully
  
  # Encode transitions
  
  testparticipant <- attentions[attentions$participantCode=="P01",]
  transset <- encodeTransitions(testparticipant)
  
  expect_equal(getattention2(83.2, transset, 
                             annoteTimeColumn = "timeStamp", 
                             annoteAttentionColumn = "annotation"),
               "TV")
  
  expect_equal(getattention2(72.9, transset, 
                             annoteTimeColumn = "timeStamp", 
                             annoteAttentionColumn = "annotation"),
               "start_tablet__TV")
  
  
})