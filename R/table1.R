
# Copyright (c) 2016 The University of Manchester, UK.
#
# Licenced under LGPL version 2.1. See LICENCE for details.
#
# The IDInteraction Attention Classification was developed in the IDInteraction project, funded by the Engineering and Physical Sciences Research Council, UK through grant agreement number EP/M017133/1.
#
# Author: David Mawdsley

# Replicate figures for table 1 of paper - "all" data (i.e. robust + glitches)
library(rpart)
library(REEMtree)
library(formula.tools)
library(IDInteraction)
library(party)
rm(list=ls())


participants <- getParticipantCodes("~/IDInteraction/tracking-analysis/Rnotebooks/resources/dual_screen_free_experiment/high_quality/front_full_face/")
trainingtimes <- c(1,2,5,10)

allresults <- NULL
for(p in participants){
  thisparticipant <- createTrackingAnnotation(p,
                                              trackingLoc = "~/IDInteraction/tracking-analysis/Rnotebooks/resources/dual_screen_free_experiment/high_quality/front_full_face/",
                                              annoteLoc = "~/IDInteraction/tracking-analysis/Rnotebooks/resources/dual_screen_free_experiment/high_quality/attention/"
  )
  
  for(tt in trainingtimes){
    accuracy <- getAccuracy(getConfusionMatrix(thisparticipant, trainingtime = tt))
    allresults <- rbind(allresults, 
                        data.frame(participant = p,
                                   trainingtime = tt,
                                   accuracy = accuracy)
    )
  }
  
  print(p)
}

sqldf::sqldf("select trainingtime, avg(accuracy) as avgaccuracy from allresults group by trainingtime")
