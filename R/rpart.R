# Copyright (c) 2016 The University of Manchester, UK.
#
# Licenced under LGPL version 2.1. See LICENCE for details.
#
# The IDInteraction Attention Classification was developed in the IDInteraction project, funded by the Engineering and Physical Sciences Research Council, UK through grant agreement number EP/M017133/1.
#
# Author: David Mawdsley

# Functions to fit recursive partitioning trees and analyse the results

#' Return the confusion matrix for an rpart tree trained with trainingtime minutes of training data
#' using the model given in formula
#' 
#' @param indata The input data set
#' @param trainingtime The training time in minutes
#' @param formula The model formula
#' 
#' @return The confusion matrix
#' 
#' @export
getConfusionMatrix <- function(indata,
                               trainingtime,
                               formula = attentionName ~ boxHeight + boxRotation + boxArea + boxWidth + widthHeightRatio + boxYcoordRel#boxYcoord
){
  
  # We need to recode the lhs factor so that it only has levels in it that are used.  The
  # levels then get replaced a the end of the function so we can compare between participants
  lhsname <- formula.tools::lhs.vars(formula)
  lhslevels <- levels(indata[,lhsname])
  indata[, lhsname] <- factor(indata[,lhsname])
  
  
  indata$training <- ifelse(indata$timestampms <= trainingtime * 60 * 1000, TRUE, FALSE)
  
  treeclass <- rpart(formula,
                     method="class", data=indata[indata$training==TRUE, ])
  
  predclass <- predict(treeclass, newdata = indata[indata$training == FALSE, ],
                       type = "class")
  
  predclassprob <- predict(treeclass, newdata = indata[indata$training == FALSE , ],
                           type = "prob")
  
  dfpredclass <- data.frame(observedAttentionName = indata[indata$training == FALSE,
                                                              lhsname],
                            predclass, predclassprob)
  
  # Use all factor levels from input data-set
  dfpredclass$predclass <- factor(dfpredclass$predclass, levels = lhslevels)
  dfpredclass$observedAttentionName <- factor(dfpredclass$observedAttentionName, levels = lhslevels)
  
  confmat <-table(dfpredclass$observedAttentionName, dfpredclass$predclass)
  
  return(confmat)
}



#' Get the accuracy from the confusion matrix
#' 
#' "Accuracy is defined as the percentage of correctly coded values"
#' 
#' @param inmat The input confusion matrix
#' 
#' @return The accuracy as a decimal
#' 
#' @export
getAccuracy <- function(inmat){
  
  matdim <- dim(inmat)

    if(length(matdim) != 2){
    stop("Expecting a 2d matrix")
  }
  
  if(matdim[1] != matdim[2]){
    stop("Matrix must be square")
  }
  
    correct <- sum(diag(inmat))
    total <- sum(inmat)
  
    return(correct/total)
}