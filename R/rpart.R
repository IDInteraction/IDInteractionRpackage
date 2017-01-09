# Copyright (c) 2016 The University of Manchester, UK.
#
# Licenced under LGPL version 2.1. See LICENCE for details.
#
# The IDInteraction Attention Classification was developed in the IDInteraction project, funded by the Engineering and Physical Sciences Research Council, UK through grant agreement number EP/M017133/1.
#
# Author: David Mawdsley

# Functions to fit recursive partitioning trees and analyse the results

#' Get the lhs of a formula
#' 
#' formula.tools version was failing in odd ways - something to do with lazy evaluation
#' @param informula
#' 
#' @return The LHS of the formula as a string
getFormulaLHS <- function(informula){
  if(class(informula) != "formula"){
    stop("Must be a formula")
  }
  
  return(as.character(informula[[2]]))
  
}



#' Get a prediction for each observation using recusrsive partitioning, given
#' trainingtime minutes of training time.
#' 
#' @param indata The input data set
#' @param trainingtime The training time in minutes
#' @param formula The model formula
#' 
#' @return A data frame containing the observed attention, the predicted attention and the 
#' probabilities for the predicted attention
#' 
#' @export
getPartitionPredictions <- function(indata, trainingtime, predformula, ...){
  
  # We recode the lhs factor so that it only has levels in it that are used.  The
  # levels then get replaced at the end of the function so we can compare between participants
  
  if(class(predformula) != "formula"){
    stop("formula must be of class formula")
  }
  
  lhsname <- getFormulaLHS(predformula)
  lhslevels <- levels(indata[,lhsname])
  indata[, lhsname] <- factor(indata[,lhsname])
  
  
  indata$training <- flagtraining(indata, trainingtime, ...)
  
  treeclass <- rpart::rpart(predformula,
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
  
  class(dfpredclass) <- c("pred", class(dfpredclass))
  
  return(dfpredclass)
  
}

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
                               formula, ...
){
  
  dfpredclass <- getPartitionPredictions(indata, trainingtime, formula, ...)
  
  confmat <-table(dfpredclass$observedAttentionName, dfpredclass$predclass)
  
  return(confmat)
}

#' Get the confusion matrix for a set of predictions
#' 
#' @param inpred a data frame of class pred containing the observed and predicted behaviours
#' @param observed string containing column name containing the observed attentions (i.e. ground truth)
#' @param predicted string containing column name containing the predicted attentions
#' @return The confusion matrix
#' @export
getConfusionMatrixPreds <- function(inpred,
                                    observed = "observedAttentionName",
                                    predicted = "predclass"){
  
  colnames <- names(inpred)
  if(!(observed %in% colnames)){
    stop(paste(observed, "not found as observed data"))
  }
  if(!(predicted %in% colnames)){
    stop(paste(predicted, "not foundas predicted data"))
  }
  
  obsdata <- inpred[,observed]
  preddata <- inpred[,predicted]
  
  if(xor(is.factor(obsdata), is.factor(preddata))){
    warning("Only one of observed and predicted is a factor")
  }
  
  if(is.factor(obsdata) && is.factor(preddata) && (levels(obsdata) != levels(preddata))){
    stop("Factor levels don't match")
  }
  
  confmat <-table(obsdata, preddata)
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

#' Calculate Matthews' correlation coefficient given a confusion matrix
#' 
#' Calculate Matthews' correlation coefficeint - a measure of classification quality
#' https://en.wikipedia.org/wiki/Matthews_correlation_coefficient
#' 
#' @param inmat The confusion matrix
#' @return Matthews' correlation coefficient
#' 
#' @export
#' 
matthewsCC <- function(inmat){
  
  TP <- as.double(inmat[1,1])
  TN <- as.double(inmat[2,2])
  FP <- as.double(inmat[1,2])
  FN <- as.double(inmat[2,1])
  
  mcc <- (TP*TN - FP*FN)/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  
  
  if(mcc < -1 | mcc > 1){
    stop(paste("error in calculation", mcc))
  }
  
  return(mcc)
  
}

